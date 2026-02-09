library(fixest)
library(dplyr)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(readr)
library(broom)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

outcomes_principais     <- c('fechamento','log_docente','log_aluno','log_num_funcionarios')
outcomes_complementares <- c('id_internet','id_agua','biblioteca','quadra_esportes',
                             'computador')

variables_of_interest <- c(outcomes_principais, outcomes_complementares)

# Load dataset

ideb_ai <- readxl::read_excel("dados/divulgacao_anos_iniciais_escolas_2023.xlsx", skip = 9) %>% 
  filter(SG_UF == "RJ") %>% 
  janitor::clean_names() %>% 
  select(c(pk_cod_entidade = id_escola,vl_observado_2005:vl_observado_2023)) %>% 
  pivot_longer(!c(pk_cod_entidade), names_to = 'ano', values_to = 'ideb_ai') %>% 
  mutate(ano = as.numeric(gsub("vl_observado_","",ano)),
         ideb_ai = as.numeric(ideb_ai)) %>% 
  filter(!is.na(ideb_ai))

ideb_af <- readxl::read_excel("dados/divulgacao_anos_finais_escolas_2023.xlsx", skip = 9) %>% 
  filter(SG_UF == "RJ") %>% 
  janitor::clean_names() %>% 
  select(c(pk_cod_entidade = id_escola,vl_observado_2005:vl_observado_2023)) %>% 
  pivot_longer(!c(pk_cod_entidade), names_to = 'ano', values_to = 'ideb_af') %>% 
  mutate(ano = as.numeric(gsub("vl_observado_","",ano)),
         ideb_af = as.numeric(ideb_af)) %>% 
  filter(!is.na(ideb_af))

etapas_escola <- readRDS('etapas_escola.rds')

df_balanceado_raw <- readRDS('df_balanceado_pos_tratamento_v1.rds') %>% 
  filter(ano <= 2015) %>% 
  filter(raio_otimo_rj == 1 | data.table::between(min_dist_rj,20,30)) %>% 
  select(-c(ideb_matematica_ai:ideb_em)) %>% 
  left_join(ideb_ai) %>% 
  left_join(ideb_af) %>% 
  left_join(etapas_escola) %>% 
  group_by(pk_cod_entidade) %>% 
  tidyr::fill(dependencia_adm,id_localizacao,id_dependencia_adm,in_fun,in_med, .direction = 'downup')

saveRDS(df_balanceado_raw, "df_balanceado_pos_tratamento_v3_teste_medias.rds")

count <- df_balanceado %>% 
  group_by(treat,ano) %>% 
  dplyr::summarise(total = n()) %>% 
  pivot_wider(names_from = 'treat', values_from = 'total')


outcomes_principais     <- c('fechamento','log_docente','log_aluno','log_num_funcionarios')
outcomes_complementares <- c('id_internet','id_agua','biblioteca','quadra_esportes',
                             'computador')
# Teste de diferença de médias Fechamento
#df_balanceado <- df_balanceado_raw %>% filter(ano <= 2010) %>%
#  mutate(treat_ds = ifelse(treat_unid_rj == 1, "Afetado", "Não Afetado"))


df_balanceado_csv <- read_csv("df_balanceado_pos_tratamento_v3_teste_medias.csv") %>% filter(ano == 2010) 
  

teste_t <- t.test(log_num_funcionarios ~ treat_desc, data = df_balanceado_csv)

# Resultado do teste t
# H_0 : Não há diferença entre as médias
# H_1 : Há diferença entre as médias
print(round(teste_t$p.value, 7))
print(teste_t$p.value)

if(teste_t$p.value < 0.05) {
  print("Há diferença significativa entre os grupos.")
} else {
  print("Não há diferença significativa entre os grupos.")
}


table(df_balanceado_csv$id_dependencia_adm)





# Teste de médias 2007 - 2010

df_balanceado <- df_balanceado_raw %>% filter(ano <= 2010) %>%
  mutate(treat_ds = ifelse(treat_unid_rj == 1, "Afetado", "Não Afetado")) %>%


# table(df_balanceado$treat_unid_rj,df_balanceado$ano)

columns_1_2 <- df_balanceado %>%
  mutate(treat_ds = ifelse(treat_unid_rj == 1, "Affected", "Not affected")) %>%
  group_by(treat_ds) %>%
  dplyr::summarise(across(c(variables_of_interest), list(
    mean = ~ mean(., na.rm = T),
    sd = ~ sd(., na.rm = T)
  ))) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  mutate(across(c(ends_with("sd")), ~ paste0("(", ., ")")))

n_obs <- df_balanceado %>%
  mutate(treat_ds = ifelse(treat_unid_rj == 1, "Affected", "Not affected")) %>%
  group_by(treat_ds) %>%
  dplyr::summarise(n_obs = n()) %>%
  mutate(treat_ds = ifelse(treat_ds == 1, "Affected", "Not affected Paired")) %>%
  t()


# Realizando o teste t para cada variável de interesse
for (var in variables_of_interest) {
  
  # Cálculo das médias e desvios padrão para os dois grupos (afetado e não afetado)
  stats <- df_balanceado %>%
    group_by(treat_unid_rj) %>%
    summarise(
      mean = mean(get(var), na.rm = TRUE),
      sd = sd(get(var), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Realizando o teste t entre os dois grupos
  t_test <- t.test(as.formula(paste(var, "~ treat_unid_rj")), data = df_balanceado)
  
  # Calculando a diferença de médias
  diff_mean <- diff(stats$mean)
  
  # Armazenando os resultados em uma lista
  resultados[[var]] <- list(
    mean_afetado = stats$mean[stats$treat_unid_rj == "Afetado"],
    sd_afetado = stats$sd[stats$treat_unid_rj == "Afetado"],
    mean_nao_afetado = stats$mean[stats$treat_unid_rj == "Não Afetado"],
    sd_nao_afetado = stats$sd[stats$treat_unid_rj == "Não Afetado"],
    diff_mean = diff_mean,
    p_value = round(t_test$p.value, 3)
  )
}

# Convertendo a lista de resultados em um dataframe
resultados_df <- bind_rows(lapply(resultados, function(res) {
  tibble(
    Variable = names(res),
    Mean_Afetado = res$mean_afetado,
    SD_Afetado = res$sd_afetado,
    Mean_Nao_Afetado = res$mean_nao_afetado,
    SD_Nao_Afetado = res$sd_nao_afetado,
    Diff_Mean = res$diff_mean,
    P_Value = res$p_value
  )
}), .id = "Variable")

# Organizando e formatando a tabela para exibição
resultados_df <- resultados_df %>%
  mutate(
    Diff_Mean = round(Diff_Mean, 2),
    P_Value = round(P_Value, 3),
    P_Value_Label = case_when(
      P_Value <= 0.01 ~ paste0(Diff_Mean, "***"),
      P_Value <= 0.05 ~ paste0(Diff_Mean, "**"),
      P_Value <= 0.10 ~ paste0(Diff_Mean, "*"),
      TRUE ~ as.character(Diff_Mean)
    )
  )

# Exibindo a tabela final
resultados_df





