install.packages('rstudioapi')
install.packages("fixest")
install.packages("dplyr")
install.packages("stargazer")
install.packages('summarytools')
install.packages('plm')
install.packages('broom')
install.packages('openxlsx')
install.packages("tidyr")
install.packages("arrow")


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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print(getwd())

file.exists('df_balanceado.rds')

# /Users/teste/Desktop/Script_Tese/

rm(list = ls())


####### FUNCTIONS

save_results <- function(results, file) {
  wb <- createWorkbook()
  for (name in names(results)) {
    addWorksheet(wb, name)
    writeData(wb, name, results[[name]])
  }
  saveWorkbook(wb, file, overwrite = TRUE)
}


#######  GLOBAL VARS
features_padronizadas <- c('computador', 'biblioteca', 'quadra_esportes')

features_selecionadas <- c('num_salas_existentes',
                           'num_funcionarios',
                           'docente',
                           'aluno',
                           'aluno_fundamental',
                           'aluno_ensino_medio',
                           'quadra_esportes',
                           'biblioteca',
                           #'dsi_fun',
                           #'dsi_medio',
                           'computador',
                           'id_cozinha')


variaveis <- c("fechamento", 
               "num_salas_existentes", 
               "num_funcionarios", 
               "num_computadores", 
               "num_comp_alunos", 
               "docente", 
               "concursado", 
               "temporario", 
               "terceirizado", 
               "professor_clt", 
               "professor_ensino_medio", 
               "professor_fundamental", 
               #"aluno_anos_iniciais", 
               "aluno_anos_finais", 
               "aluno_fundamental", 
               "aluno_ensino_medio", 
               "aluno", "qtd_turmas", 
               "log_num_salas_existentes", 
               "log_num_funcionarios", 
               "log_num_computadores", 
               "log_num_comp_alunos", 
               "log_docente", 
               "log_concursado", 
               "log_temporario",
               "log_terceirizado", 
               "log_professor_clt", 
               "log_professor_ensino_medio", 
               "log_professor_fundamental", 
               #"log_aluno_anos_iniciais", 
               "log_aluno_anos_finais", 
               "log_aluno_fundamental", 
               "log_aluno_ensino_medio", 
               "log_aluno", 
               "log_qtd_turmas", 
               "id_agua_inexistente", 
               "id_energia_inexistente", 
               "id_esgoto_inexistente", 
               "id_quadra_esportes", 
               "id_biblioteca", 
               "id_cozinha", 
               "id_parque_infantil", 
               "id_sanitario_dentro_predio", 
               "id_internet", 
               "id_sala_leitura", 
               "id_despensa", 
               "atu_inf", 
               "atu_cre", 
               "atu_pre", 
               "atu_fun", 
               "atu_f14", 
               "atu_f58", 
               "atu_med", 
               "aprov_fund", 
               "aprov_medio", 
               "reprov_efai", 
               "reprov_efaf", 
               "reprov_fund", 
               "reprov_medio", 
               "aband_efai", 
               "aband_efaf", 
               "aband_fund", 
               "aprov_efai", 
               "aprov_efaf", 
               "aband_medio", 
               "dsi_ai", 
               "dsi_af", 
               "dsi_fun", 
               "dsi_medio", 
               "ideb_matematica_ai" ,
               "qtd_turmas",
               "qtd_turmas_creche",
               "qtd_turmas_pre_escola",
               "qtd_turmas_infantil",
               "qtd_turmas_fundamental",
               "qtd_turmas_medio",
               "transporte_publico",
               "turma_idade_media",
               "aluno_prop_meninos",
               "aluno_prop_branco",
               "aluno_prop_rural"
)

features_ideb_raw <- c(
  'ideb_matematica_ai',
  'ideb_portugues_ai',
  'ideb_media_ai',
  'ideb_ai',
  'ideb_matematica_af',
  'ideb_portugues_af',
  'ideb_media_af',
  'ideb_af',
  'ideb_matematica_em',
  'ideb_portugues_em',
  'ideb_media_em',
  'ideb_em',
  'log_ideb_matematica_ai',
  'log_ideb_portugues_ai',
  'log_ideb_media_ai',
  'log_ideb_ai',
  'log_ideb_matematica_af',
  'log_ideb_portugues_af',
  'log_ideb_media_af',
  'log_ideb_af',
  'log_ideb_matematica_em',
  'log_ideb_portugues_em',
  'log_ideb_media_em',
  'log_ideb_em'
)



features_ideb <- c("ideb_portugues_ai", "ideb_media_ai", "ideb_ai", "ideb_matematica_af", 
                   "ideb_portugues_af", "ideb_media_af", "ideb_af",  
                   "ideb_em", "log_ideb_matematica_ai", 
                   "log_ideb_portugues_ai", "log_ideb_media_ai", "log_ideb_ai", "log_ideb_matematica_af", 
                   "log_ideb_portugues_af", "log_ideb_media_af", "log_ideb_af",  
                   "log_ideb_em")

results_mean_effect <- list()
results_years <- list()
results_event_study <- list()

variaveis_aux <- c("num_salas_existentes", "num_funcionarios", "num_computadores", 
                   "num_comp_alunos", "docente", "concursado", "temporario", 
                   "terceirizado", "professor_clt", "professor_ensino_medio", 
                   "professor_fundamental", "aluno_anos_inciais", 
                   "aluno_anos_finais", "aluno_fundamental", "aluno_ensino_medio", 
                   "aluno", "qtd_turmas", "vlcusteio", "vlcapital", "vltotal", 
                   "ideb_matematica_ai", "ideb_portugues_ai", "ideb_media_ai", 
                   "ideb_ai", "ideb_matematica_af", "ideb_portugues_af", 
                   "ideb_media_af", "ideb_af", "ideb_matematica_em", 
                   "ideb_portugues_em", "ideb_media_em", "ideb_em")


####### DATASETS

df <- readRDS('df_balanceado.rds') %>% filter(sigla == 'RJ')


# 237.136+

#df <- readRDS('database.rds')%>% filter(sigla == 'RJ')
# 222.315


base_line <- 2010
df$period <- df$ano - 2011 + 1

data_rj <- df %>% 
  #filter(between(ano,base_line - 9,base_line + 9)) %>% 
  # filter(raio_otimo_rj == 1 | between(min_dist_rj,30,60)) %>%
  filter(ano >= 2007) %>%
  filter(ano < 2020) %>%
  #filter(raio_otimo_rj == 1 | (min_dist_rj >= 30 & min_dist_rj <= 60)) %>%
  mutate(treat      = ifelse(raio_otimo_rj == 1 & ano >= 2011,1,0),
         treat_unid = ifelse(raio_otimo_rj == 1,1,0),
         treat_1yr  = ifelse(raio_otimo_rj == 1 & ano == 2011,1,0),
         treat_2yr  = ifelse(raio_otimo_rj == 1 & ano == 2012,1,0),
         treat_3yr  = ifelse(raio_otimo_rj == 1 & ano == 2013,1,0),
         treat_4yr  = ifelse(raio_otimo_rj == 1 & ano == 2014,1,0),
         treat_5yr  = ifelse(raio_otimo_rj == 1 & ano == 2015,1,0),
         treat_6yr  = ifelse(raio_otimo_rj == 1 & ano == 2016,1,0),
         treat_7yr  = ifelse(raio_otimo_rj == 1 & ano == 2017,1,0),
         treat_8yr  = ifelse(raio_otimo_rj == 1 & ano == 2018,1,0),
         treat_9yr  = ifelse(raio_otimo_rj == 1 & ano == 2019,1,0)
  )


df_balanceado <- expand.grid(unique(data_rj$pk_cod_entidade),c(min(data_rj$ano):max(data_rj$ano))) %>% distinct() %>% 
  rename(pk_cod_entidade = 1,ano = 2) %>% left_join(data_rj %>% mutate(morte_escola = 0) %>% 
                                                      distinct(pk_cod_entidade, ano, .keep_all = T)) %>% 
  group_by(pk_cod_entidade) %>% 
  mutate(raio_otimo_rj = max(raio_otimo_rj, na.rm = T),
         min_dist   = max(min_dist_rj, na.rm = T),
         morte_escola = ifelse(is.na(morte_escola),1,morte_escola)) %>% 
  mutate(treat      = ifelse(raio_otimo_rj == 1 & ano >= 2011,1,0),
         treat_unid = ifelse(raio_otimo_rj == 1,1,0),
         treat_1yr  = ifelse(treat_unid == 1 & ano == 2011,1,0),
         treat_2yr  = ifelse(treat_unid == 1 & ano == 2012,1,0),
         treat_3yr  = ifelse(treat_unid == 1 & ano == 2013,1,0),
         treat_4yr  = ifelse(treat_unid == 1 & ano == 2014,1,0),
         treat_5yr  = ifelse(treat_unid == 1 & ano == 2015,1,0),
         treat_6yr  = ifelse(treat_unid == 1 & ano == 2016,1,0),
         treat_7yr  = ifelse(treat_unid == 1 & ano == 2017,1,0),
         treat_8yr  = ifelse(treat_unid == 1 & ano == 2018,1,0),
         treat_9yr  = ifelse(treat_unid == 1 & ano == 2019,1,0),
  )


# Substitui valores NA por 0 e gera as variáveis logarítmicas
for (i in variaveis_aux) {
  df_balanceado[[i]][is.na(df_balanceado[[i]])] <- 0
  df_balanceado[[paste0("log_", i)]] <- log(df_balanceado[[i]] + 1)
}

# Cria a variável temp
df_balanceado$temp <- ifelse(!is.na(df_balanceado$SITUACAO), df_balanceado$ano, NA)

# Gera a variável temp_ para cada grupo
df_balanceado <- df_balanceado %>%
  group_by(pk_cod_entidade) %>%
  mutate(temp_ = min(temp, na.rm = TRUE)) %>%
  ungroup()

# Cria a variável fechamento
df_balanceado$fechamento <- ifelse(df_balanceado$SITUACAO != 1, 1, 0)
df_balanceado$fechamento[is.na(df_balanceado$fechamento)] <- 0
df_balanceado$fechamento[df_balanceado$ano < df_balanceado$temp_] <- NA

# Remove as variáveis temporárias
df_balanceado <- df_balanceado %>%
  select(-temp, -temp_)


# Regra caso Escola esteja fechada preenche log_ vars com NA

df_balanceado <- df_balanceado %>%
  mutate(across(starts_with("log_"), ~ ifelse(fechamento == 1, NA, .)))


# Padronização de algumas variáveis


df_balanceado <- df_balanceado %>%
  mutate(quadra_esportes = pmax(id_quadra_esportes, id_quadra_esportes_coberta, id_quadra_esportes_descoberta, na.rm = TRUE),
         biblioteca = pmax(id_biblioteca, id_biblioteca_sala_leitura, na.rm = TRUE),
         fl_num_computador = ifelse(num_computador >= 1, 1, 0),
         fl_num_computadores = ifelse(num_computadores >= 1, 1, 0),
         computador = pmax(id_computador, id_computadores, fl_num_computador, fl_num_computadores, na.rm = TRUE)
  )


# Variáveis de heterogeneidade

df_balanceado <- df_balanceado %>%
  mutate(
    dependencia_adm = case_when(
      id_dependencia_adm %in% c(1, 2, 3) ~ "Publica",
      id_dependencia_adm == 4 ~ "Privada",
      TRUE ~ NA_character_  # Para lidar com outros casos ou valores ausentes
    )
  )

#write.csv(df_balanceado, file = 'df_balanceado_vf.csv', row.names = FALSE)

#df_balanceado <- df_balanceado %>%
#  filter(id_localizacao == 2)


# 
df_ano_escola <- df_balanceado %>% 
  filter(fechamento == 0) %>%
  group_by(pk_cod_entidade) %>% 
  mutate(primeiro_ano = min(ano)) %>% 
  select(pk_cod_entidade, primeiro_ano) %>%
  distinct(pk_cod_entidade, .keep_all = TRUE)


df_balanceado <- df_balanceado %>% left_join(df_ano_escola) 
df_balanceado <- df_balanceado %>% filter(primeiro_ano <= 2011)

#summary(df_balanceado$primeiro_ano)

#View(df_test)

# Regra de preenchimento das demais features caso fechamento seja 1

features_model <- c(variaveis, features_ideb, features_padronizadas)
features_model <- setdiff(features_model, "fechamento")


df_balanceado <- df_balanceado %>%
  mutate(across(features_model, ~ ifelse(fechamento == 1, NA, .)))


# Transforma variáveis de água

df_balanceado <- df_balanceado %>%
  mutate(id_agua_filtrada = case_when(
    ano >= 2009 & ano <= 2013 ~ ifelse(id_agua_filtrada == 0, 0, 
                                       ifelse(id_agua_filtrada == 1, 0, 
                                              ifelse(id_agua_filtrada == 2, 1, id_agua_filtrada))),  # 0 -> 0, 1 -> 0, 2 -> 1
    TRUE ~ id_agua_filtrada  # Mantém o valor original para outros anos
  )) %>%
  mutate(id_esgoto = pmax(id_esgoto_rede_publica, id_esgoto_fossa, id_esgoto_fossa_septica, id_esgoto_fossa_comum),
         id_agua   = pmax(id_agua_filtrada, id_agua_rede_publica, id_agua_poco_artesiano, id_agua_cacimba, id_agua_fonte_rio)
         )


# Transforma features do IDEB

anos_interesse <- c(2008, 2010, 2012, 2014, 2016, 2018)

# Substitua os valores zero por NA nas variáveis especificadas
df_balanceado <- df_balanceado %>%
  mutate(across(all_of(features_ideb_raw), 
                ~ ifelse(ano %in% anos_interesse & . == 0, NA, .)))



# Substitui os valores zero por NA para outras variáveis específicas independente do ano

variaveis_adicionais <- c(
  "qtd_turmas_creche",
  "qtd_turmas_pre_escola",
  "qtd_turmas_infantil",
  "professor_clt",
  "terceirizado",
  "qtd_turmas_fundamental",
  "qtd_turmas_medio",
  "num_salas_existentes",
  "qtd_turmas",
  "num_comp_alunos",
  "num_computadores",
  "temporario",
  "num_funcionarios",
  "professor_fundamental",
  "professor_ensino_medio",
  "concursado",
  "docente",
  "aluno_anos_inciais",
  "aluno_anos_finais",
  "aluno_ensino_medio",
  "aluno_fundamental",
  "aluno"
)

variaveis_comb <- c(variaveis_adicionais, features_ideb_raw)

df_balanceado <- df_balanceado %>%
  mutate(across(all_of(variaveis_comb), ~ na_if(., 0)))




# Salva banco de dados
saveRDS(df_balanceado, 'df_balanceado_pos_tratamento_v1.rds')



## Incluindo variáveis do setor censitario