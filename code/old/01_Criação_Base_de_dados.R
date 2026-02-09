# install.packages('rstudioapi')
# install.packages("fixest")
# install.packages("dplyr")
# install.packages("stargazer")
# install.packages('summarytools')
# install.packages('plm')
# install.packages('broom')
# install.packages('openxlsx')
# install.packages("tidyr")
# install.packages("arrow")


library(rstudioapi)
library(fixest)
library(dplyr)
library(stargazer)
library(summarytools)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)
library(arrow)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# print(getwd())

# file.exists('df_balanceado.rds')

# /Users/teste/Desktop/Script_Tese/

rm(list = ls())


ideb_ai <- readxl::read_excel("./input/dados/divulgacao_anos_iniciais_escolas_2023.xlsx", skip = 9) %>% 
  filter(SG_UF == "RJ") %>% 
  janitor::clean_names() %>% 
  select(c(pk_cod_entidade = id_escola,vl_observado_2005:vl_observado_2023)) %>% 
  pivot_longer(!c(pk_cod_entidade), names_to = 'ano', values_to = 'ideb_ai') %>% 
  mutate(ano = as.numeric(gsub("vl_observado_","",ano)),
         ideb_ai = as.numeric(ideb_ai)) %>% 
  filter(!is.na(ideb_ai))

ideb_af <- readxl::read_excel("./input/dados/divulgacao_anos_finais_escolas_2023.xlsx", skip = 9) %>% 
  filter(SG_UF == "RJ") %>% 
  janitor::clean_names() %>% 
  select(c(pk_cod_entidade = id_escola,vl_observado_2005:vl_observado_2023)) %>% 
  pivot_longer(!c(pk_cod_entidade), names_to = 'ano', values_to = 'ideb_af') %>% 
  mutate(ano = as.numeric(gsub("vl_observado_","",ano)),
         ideb_af = as.numeric(ideb_af)) %>% 
  filter(!is.na(ideb_af))

ideb_em <- readxl::read_excel("./input/dados/divulgacao_ensino_medio_escolas_2023.xlsx", skip = 9) %>% 
  filter(SG_UF == "RJ") %>% 
  janitor::clean_names() %>% 
  select(c(pk_cod_entidade = id_escola,vl_observado_2017:vl_observado_2023)) %>% 
  pivot_longer(!c(pk_cod_entidade), names_to = 'ano', values_to = 'ideb_em') %>% 
  mutate(ano = as.numeric(gsub("vl_observado_","",ano)),
         ideb_em = as.numeric(ideb_em)) %>% 
  filter(!is.na(ideb_em))


etapas_escola <- readRDS('./data/etapas_escola.rds')

df_turmas <- data.table::fread("input/dados/Porte Escola/df_turmas.csv") %>% 
  mutate(
    porte_escola = case_when(
      tot_matriculas <= 100 ~ "<100",
      tot_matriculas > 100 & tot_matriculas <= 500 ~ "101-500",
      tot_matriculas > 500 ~ ">501",
      TRUE ~ NA_character_  
    )
  )

alunos <- haven::read_dta('./base/alunos_2007_2020.dta') %>% 
  filter(substr(pk_cod_entidade ,1,2) == '33') %>% 
  select(c(pk_cod_entidade,ano,aluno_fundamental,aluno_anos_inciais,aluno_anos_finais,   
           aluno_ensino_medio,aluno,turma_idade_media,aluno_prop_meninos,aluno_prop_branco,
           aluno_prop_rural,nascimento_municipio,residencia_municipio,aluno_ensino_regular,
           transporte_publico))

df_balanceado <- readRDS('./data/df_balanceado_pos_tratamento_v1.rds') %>% 
  select(-c(ideb_matematica_ai:ideb_em,
            aluno_fundamental,aluno_anos_inciais,aluno_anos_finais,   
            aluno_ensino_medio,aluno,turma_idade_media,aluno_prop_meninos,aluno_prop_branco,
            aluno_prop_rural,nascimento_municipio,residencia_municipio,aluno_ensino_regular,
            transporte_publico)) %>% 
  left_join(ideb_ai) %>% 
  left_join(ideb_af) %>% 
  left_join(etapas_escola) %>% 
  left_join(df_turmas) %>% 
  left_join(alunos) %>% 
  group_by(pk_cod_entidade) %>% 
  tidyr::fill(dependencia_adm,id_localizacao,id_dependencia_adm,in_fun,in_med,porte_escola, .direction = 'downup')

df_balanceado$ideb_media <- rowMeans(
  df_balanceado[, c("ideb_ai", "ideb_af")], 
  na.rm = TRUE
)

df_balanceado$aprov_media <- rowMeans(
  df_balanceado[, c("aprov_fund", "aprov_medio")], 
  na.rm = TRUE
)

df_balanceado$aband_media <- rowMeans(
  df_balanceado[, c("aband_fund", "aband_medio")], 
  na.rm = TRUE
)

df_balanceado$reprov_media <- rowMeans(
  df_balanceado[, c("reprov_fund", "reprov_medio")], 
  na.rm = TRUE
)

df_balanceado$log_aluno                <- log(df_balanceado$aluno+1)
df_balanceado$log_aluno_ensino_regular <- log(df_balanceado$aluno_ensino_regular+1)

geocode <- bind_rows(lapply(list.files(path = './Desastre e escolas - Pré code/geocode_censo_escolar',
                                       full.names = T,
                                       pattern = '*.parquet'),
                            arrow::read_parquet))
geocode <- subset(geocode, substr(CO_ENTIDADE,1,2) == '33')
geocode <- subset(geocode,Addr_type %in% c('PointAddress','Postal',
                                           'PostalExt','PostalLoc',
                                           'StreetAddress','StreetAddressExt',
                                           'StreetName'))
geocode <- geocode %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) 
geocode <- geocode %>% 
  select(c(pk_cod_entidade = CO_ENTIDADE,geometry))

geocode <- geocode %>% distinct(pk_cod_entidade, .keep_all = T)


census_df <- data.table::fread('./data/census_tracts2010_brazil.csv',
                               select = c('code_tract','pop_branca',
                               'income_total','pop_per_household',
                               'households_total','pop_water_network',
                               'pop_total','urban','favela','men'))
census_df$code_tract <- as.character(census_df$code_tract)

census_geo <- left_join(geobr::read_census_tract(code_tract = 'RJ') %>% 
                          select(c(code_tract,zone,geom)),
                        census_df)


# Garante que ambos estão no mesmo CRS
census_geo <- sf::st_transform(census_geo, sf::st_crs(geocode))
schools_census_geo <- sf::st_join(geocode,census_geo, join = sf::st_nearest_feature)
schools_census_geo <- as.data.frame(schools_census_geo) %>% 
  select(-c(geometry))
schools_census_geo$pk_cod_entidade <- as.numeric(schools_census_geo$pk_cod_entidade)


df_final <- left_join(df_balanceado, schools_census_geo)

## Variavel de tratamento
df_final <- df_final %>% 
  filter(ano >= 2007) %>%
  filter(ano < 2020) %>%
  mutate(treat      = ifelse(raio == 1 & ano >= 2011,1,0),
         treat_unid = ifelse(raio == 1,1,0),
         treat_1yr  = ifelse(raio == 1 & ano == 2011,1,0),
         treat_2yr  = ifelse(raio == 1 & ano == 2012,1,0),
         treat_3yr  = ifelse(raio == 1 & ano == 2013,1,0),
         treat_4yr  = ifelse(raio == 1 & ano == 2014,1,0),
         treat_5yr  = ifelse(raio == 1 & ano == 2015,1,0),
         treat_6yr  = ifelse(raio == 1 & ano == 2016,1,0),
         treat_7yr  = ifelse(raio == 1 & ano == 2017,1,0),
         treat_8yr  = ifelse(raio == 1 & ano == 2018,1,0),
         treat_9yr  = ifelse(raio == 1 & ano == 2019,1,0)
  )


saveRDS(df_final,'./data/df_final.rds')
