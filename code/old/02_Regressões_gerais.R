install.packages('rstudioapi')
install.packages("fixest")
install.packages("dplyr")
install.packages("stargazer")
install.packages('summarytools')
install.packages('plm')
install.packages('broom')
install.packages('openxlsx')
install.packages("tidyr")


library(rstudioapi)
library(fixest)
library(dplyr)
library(stargazer)
library(summarytools)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)

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
features_padronizadas <- c('computador', 'biblioteca', 'quadra_esportes', 'id_esgoto', 'id_agua')

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
               "aluno_anos_iniciais", 
               "aluno_anos_finais", 
               "aluno_fundamental", 
               "aluno_ensino_medio", 
               "aluno", 
               "qtd_turmas", 
               #"log_num_salas_existentes", 
               #"log_num_funcionarios", 
               #"log_num_computadores", 
               #"log_num_comp_alunos", 
               #"log_docente", 
               #"log_concursado", 
               #"log_temporario",
               #"log_terceirizado", 
               #"log_professor_clt", 
               #"log_professor_ensino_medio", 
               #"log_professor_fundamental", 
               #"log_aluno_anos_iniciais", 
               #"log_aluno_anos_finais", 
               #"log_aluno_fundamental", 
               #"log_aluno_ensino_medio", 
               #"log_aluno", 
               #"log_qtd_turmas", 
               #"id_agua_inexistente", 
               "id_energia_inexistente", 
               #"id_esgoto_inexistente", 
               #"id_quadra_esportes", 
               #"id_biblioteca", 
               #"id_cozinha", 
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

features_ideb <- c("ideb_portugues_ai", "ideb_media_ai", "ideb_ai", "ideb_matematica_af", 
                   "ideb_portugues_af", "ideb_media_af", "ideb_af"  
                   )


results_mean_effect <- list()
results_years <- list()
results_event_study <- list()



features_model <- c(
  'fechamento',
  'docente',
  'aluno',
  'num_funcionarios',
  'num_salas_existentes',
  'qtd_turmas',
  'computador',
  'id_internet',
  'id_esgoto',
  'id_agua',
  'biblioteca',
  'quadra_esportes'
)



# Load dataset

df_balanceado <- readRDS('df_balanceado_pos_tratamento_v0.rds')


# REGRESSÃO

results_mean_effect <- NULL
results_effect <- NULL
results_event_study <- NULL

#features_model = c(variaveis, features_ideb, features_padronizadas)




for (feature in features_model){
  tryCatch({
    if (length(unique(df_balanceado[[feature]])) > 1) {
      
      # Efeito médio - sem tendência
      formula_1 <- as.formula(paste(feature, "~ treat | pk_cod_entidade + ano"))
      reg1_sem_tendencia <- fixest::feols(formula_1, data = df_balanceado, cluster = c("pk_cod_entidade"))
      nobs = reg1_sem_tendencia$nobs
      reg1_sem_tendencia <- tidy(reg1_sem_tendencia) %>% 
        mutate(term = feature,
               p.value_sig = ifelse(p.value <= 0.01, "***",
                                    ifelse(p.value <= 0.05, "**",
                                           ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value_sig),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(" " = term, coef = estimate, "  " = std.error)) %>% 
        mutate(nobs = nobs) %>%
        t
      
      reg1_sem_tendencia <- cbind(rownames(reg1_sem_tendencia), reg1_sem_tendencia)
      
      
      # Efeito médio - com tendência
      formula_1 <- as.formula(paste(feature, "~ treat | pk_cod_entidade + ano + fk_cod_municipio * ano"))
      reg1_com_tendencia <- fixest::feols(formula_1, data = df_balanceado, cluster = c("pk_cod_entidade"))
      nobs = reg1_com_tendencia$nobs
      reg1_com_tendencia <- tidy(reg1_com_tendencia) %>% 
        mutate(term = feature,
               p.value_sig = ifelse(p.value <= 0.01, "***",
                                    ifelse(p.value <= 0.05, "**",
                                           ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value_sig),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(" " = term, coef = estimate, "  " = std.error)) %>% 
        mutate(nobs = nobs) %>%
        t
      
      reg1_com_tendencia <- cbind(rownames(reg1_com_tendencia), reg1_com_tendencia)
      
      reg1 <- cbind(reg1_sem_tendencia,reg1_com_tendencia)
      
      if (feature == "fechamento"){
        results_mean_effect <- bind_cols(results_mean_effect,reg1)
      }else{
        results_mean_effect <- bind_cols(results_mean_effect,reg1[,2])
      }
      
      
      
      
      # Efeito ano a ano
      formula_2 <- as.formula(paste(feature, "~ treat_1yr + treat_2yr + treat_3yr + treat_4yr + treat_5yr + treat_6yr + treat_7yr + treat_8yr + treat_9yr | pk_cod_entidade + ano"))
      reg2 <- fixest::feols(formula_2, data = df_balanceado, cluster = c("pk_cod_entidade"))
      nobs = reg2$nobs
      reg2 <- tidy(reg2) %>% 
        mutate(var = feature, 
               p.value = ifelse(p.value <= 0.01, "***",
                                ifelse(p.value <= 0.05, "**",
                                       ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(var = term, coef = estimate, se = std.error))
      
      
      # Transformar a estrutura dos dados
      reg2_long <- reg2 %>%
        pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
        arrange(var, type) %>%
        mutate(var = if_else(type == "se", "", var)) %>%
        select(-type)
      
      reg2_long <- rbind(reg2_long, data.frame(var = c("nobs"), value = c(nobs)))
      
      names(reg2_long) <-  c(' ',paste0(feature))
      
      if (feature == "fechamento"){
        results_effect <- bind_cols(results_effect,reg2_long)
      }else{
        results_effect <- bind_cols(results_effect,reg2_long[,2])
      }
      
      
      # Efeito event study
      formula_3 <- as.formula(paste(feature, "~ i(period,treat_unid, 0) | pk_cod_entidade + ano"))
      reg3 <- fixest::feols(formula_3, data = df_balanceado, cluster = c("pk_cod_entidade"))
      nobs = reg3$nobs
      reg3 <- tidy(reg3) %>% 
        mutate(var = feature, 
               p.value = ifelse(p.value <= 0.01, "***",
                                ifelse(p.value <= 0.05, "**",
                                       ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(var = term, coef = estimate, se = std.error))
      
      
      # Transformar a estrutura dos dados
      reg3_long <- reg3 %>%
        pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
        arrange(var, type) %>%
        mutate(var = if_else(type == "se", "", var)) %>%
        select(-type)
      
      reg3_long <- rbind(reg3_long, data.frame(var = c("nobs"), value = c(nobs)))
      
      names(reg3_long) <-  c(' ',paste0(feature))
      
      if (feature == "fechamento"){
        results_event_study <- bind_cols(results_event_study,reg3_long)
      }else{
        results_event_study <- bind_cols(results_event_study,reg3_long[,2])
      }
      
    }
  }, error = function(e) {
    cat(paste("Erro ao processar", feature, ": ", conditionMessage(e), "\n"))
    # Pode adicionar lógica para lidar com o erro, se necessário
  })
}


View(results_mean_effect)
View(results_effect)
View(results_event_study)


save_results(results_mean_effect, "tb_results_mean_effects_vf.xlsx")
save_results(results_effect, "tb_results_effects_vf.xlsx")
save_results(results_event_study, "tb_results_event_study_vf.xlsx")



# Regressões Público

df_balanceado <- readRDS('df_balanceado_pos_tratamento_v0.rds') %>% filter(dependencia_adm == "Publica")


results_mean_effect <- NULL
results_effect <- NULL
results_event_study <- NULL

#features_model = c(
#  'aluno_anos_inciais',
#  'aluno_anos_finais',
#  'aluno_ensino_medio',
#  'aluno_fundamental',
#  'aluno'
#)


for (feature in features_model){
  tryCatch({
    if (length(unique(df_balanceado[[feature]])) > 1) {
      
      # Efeito médio - sem tendência
      formula_1 <- as.formula(paste(feature, "~ treat | pk_cod_entidade + ano"))
      reg1_sem_tendencia <- fixest::feols(formula_1, data = df_balanceado, cluster = c("pk_cod_entidade"))
      nobs = reg1_sem_tendencia$nobs
      reg1_sem_tendencia <- tidy(reg1_sem_tendencia) %>% 
        mutate(term = feature,
               p.value_sig = ifelse(p.value <= 0.01, "***",
                                    ifelse(p.value <= 0.05, "**",
                                           ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value_sig),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(" " = term, coef = estimate, "  " = std.error)) %>% 
        mutate(nobs = nobs) %>%
        t
      
      reg1_sem_tendencia <- cbind(rownames(reg1_sem_tendencia), reg1_sem_tendencia)
      
      
      # Efeito médio - com tendência
      formula_1 <- as.formula(paste(feature, "~ treat | pk_cod_entidade + ano + fk_cod_municipio * ano"))
      reg1_com_tendencia <- fixest::feols(formula_1, data = df_balanceado, cluster = c("pk_cod_entidade"))
      nobs = reg1_com_tendencia$nobs
      reg1_com_tendencia <- tidy(reg1_com_tendencia) %>% 
        mutate(term = feature,
               p.value_sig = ifelse(p.value <= 0.01, "***",
                                    ifelse(p.value <= 0.05, "**",
                                           ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value_sig),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(" " = term, coef = estimate, "  " = std.error)) %>% 
        mutate(nobs = nobs) %>%
        t
      
      reg1_com_tendencia <- cbind(rownames(reg1_com_tendencia), reg1_com_tendencia)
      
      reg1 <- cbind(reg1_sem_tendencia,reg1_com_tendencia)
      
      if (feature == "fechamento"){
        results_mean_effect <- bind_cols(results_mean_effect,reg1)
      }else{
        results_mean_effect <- bind_cols(results_mean_effect,reg1[,2])
      }
      
      
      
      
      # Efeito ano a ano
      formula_2 <- as.formula(paste(feature, "~ treat_1yr + treat_2yr + treat_3yr + treat_4yr + treat_5yr + treat_6yr + treat_7yr + treat_8yr + treat_9yr | pk_cod_entidade + ano"))
      reg2 <- fixest::feols(formula_2, data = df_balanceado)
      nobs = reg2$nobs
      reg2 <- tidy(reg2) %>% 
        mutate(var = feature, 
               p.value = ifelse(p.value <= 0.01, "***",
                                ifelse(p.value <= 0.05, "**",
                                       ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(var = term, coef = estimate, se = std.error))
      
      
      # Transformar a estrutura dos dados
      reg2_long <- reg2 %>%
        pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
        arrange(var, type) %>%
        mutate(var = if_else(type == "se", "", var)) %>%
        select(-type)
      
      reg2_long <- rbind(reg2_long, data.frame(var = c("nobs"), value = c(nobs)))
      
      names(reg2_long) <-  c(' ',paste0(feature))
      
      if (feature == "fechamento"){
        results_effect <- bind_cols(results_effect,reg2_long)
      }else{
        results_effect <- bind_cols(results_effect,reg2_long[,2])
      }
      
      
      # Efeito event study
      formula_3 <- as.formula(paste(feature, "~ i(period,treat_unid, 0) | pk_cod_entidade + ano"))
      reg3 <- fixest::feols(formula_3, data = df_balanceado)
      nobs = reg3$nobs
      reg3 <- tidy(reg3) %>% 
        mutate(var = feature, 
               p.value = ifelse(p.value <= 0.01, "***",
                                ifelse(p.value <= 0.05, "**",
                                       ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(var = term, coef = estimate, se = std.error))
      
      
      # Transformar a estrutura dos dados
      reg3_long <- reg3 %>%
        pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
        arrange(var, type) %>%
        mutate(var = if_else(type == "se", "", var)) %>%
        select(-type)
      
      reg3_long <- rbind(reg3_long, data.frame(var = c("nobs"), value = c(nobs)))
      
      names(reg3_long) <-  c(' ',paste0(feature))
      
      if (feature == "fechamento"){
        results_event_study <- bind_cols(results_event_study,reg3_long)
      }else{
        results_event_study <- bind_cols(results_event_study,reg3_long[,2])
      }
      
    }
  }, error = function(e) {
    cat(paste("Erro ao processar", feature, ": ", conditionMessage(e), "\n"))
    # Pode adicionar lógica para lidar com o erro, se necessário
  })
}
results_mean_effect_publica <- results_mean_effect
results_effect_publica <- results_effect
results_event_study_publica <- results_event_study

View(results_mean_effect_publica)
View(results_effect_publica)
View(results_event_study_publica)


# Regressões Privada

df_balanceado <- readRDS('df_balanceado_pos_tratamento_v0.rds') %>% filter(dependencia_adm == "Privada")


results_mean_effect <- NULL
results_effect <- NULL
results_event_study <- NULL

#features_model = c(
#  'aluno_anos_inciais',
#  'aluno_anos_finais',
#  'aluno_ensino_medio',
#  'aluno_fundamental',
#  'aluno'
#)


for (feature in features_model){
  tryCatch({
    if (length(unique(df_balanceado[[feature]])) > 1) {
      
      # Efeito médio - sem tendência
      formula_1 <- as.formula(paste(feature, "~ treat | pk_cod_entidade + ano"))
      reg1_sem_tendencia <- fixest::feols(formula_1, data = df_balanceado, cluster = c("pk_cod_entidade"))
      nobs = reg1_sem_tendencia$nobs
      reg1_sem_tendencia <- tidy(reg1_sem_tendencia) %>% 
        mutate(term = feature,
               p.value_sig = ifelse(p.value <= 0.01, "***",
                                    ifelse(p.value <= 0.05, "**",
                                           ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value_sig),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(" " = term, coef = estimate, "  " = std.error)) %>% 
        mutate(nobs = nobs) %>%
        t
      
      reg1_sem_tendencia <- cbind(rownames(reg1_sem_tendencia), reg1_sem_tendencia)
      
      
      # Efeito médio - com tendência
      formula_1 <- as.formula(paste(feature, "~ treat | pk_cod_entidade + ano + fk_cod_municipio * ano"))
      reg1_com_tendencia <- fixest::feols(formula_1, data = df_balanceado, cluster = c("pk_cod_entidade"))
      nobs = reg1_com_tendencia$nobs
      reg1_com_tendencia <- tidy(reg1_com_tendencia) %>% 
        mutate(term = feature,
               p.value_sig = ifelse(p.value <= 0.01, "***",
                                    ifelse(p.value <= 0.05, "**",
                                           ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value_sig),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(" " = term, coef = estimate, "  " = std.error)) %>% 
        mutate(nobs = nobs) %>%
        t
      
      reg1_com_tendencia <- cbind(rownames(reg1_com_tendencia), reg1_com_tendencia)
      
      reg1 <- cbind(reg1_sem_tendencia,reg1_com_tendencia)
      
      if (feature == "fechamento"){
        results_mean_effect <- bind_cols(results_mean_effect,reg1)
      }else{
        results_mean_effect <- bind_cols(results_mean_effect,reg1[,2])
      }
      
      
      
      
      # Efeito ano a ano
      formula_2 <- as.formula(paste(feature, "~ treat_1yr + treat_2yr + treat_3yr + treat_4yr + treat_5yr + treat_6yr + treat_7yr + treat_8yr + treat_9yr | pk_cod_entidade + ano"))
      reg2 <- fixest::feols(formula_2, data = df_balanceado)
      nobs = reg2$nobs
      reg2 <- tidy(reg2) %>% 
        mutate(var = feature, 
               p.value = ifelse(p.value <= 0.01, "***",
                                ifelse(p.value <= 0.05, "**",
                                       ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(var = term, coef = estimate, se = std.error))
      
      
      # Transformar a estrutura dos dados
      reg2_long <- reg2 %>%
        pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
        arrange(var, type) %>%
        mutate(var = if_else(type == "se", "", var)) %>%
        select(-type)
      
      reg2_long <- rbind(reg2_long, data.frame(var = c("nobs"), value = c(nobs)))
      
      names(reg2_long) <-  c(' ',paste0(feature))
      
      if (feature == "fechamento"){
        results_effect <- bind_cols(results_effect,reg2_long)
      }else{
        results_effect <- bind_cols(results_effect,reg2_long[,2])
      }
      
      
      # Efeito event study
      formula_3 <- as.formula(paste(feature, "~ i(period,treat_unid, 0) | pk_cod_entidade + ano"))
      reg3 <- fixest::feols(formula_3, data = df_balanceado)
      nobs = reg3$nobs
      reg3 <- tidy(reg3) %>% 
        mutate(var = feature, 
               p.value = ifelse(p.value <= 0.01, "***",
                                ifelse(p.value <= 0.05, "**",
                                       ifelse(p.value <= 0.10, "*",""))),
               estimate = paste0(round(estimate,5),p.value),
               std.error = paste0("(",round(std.error,5),")")) %>% 
        select(c(var = term, coef = estimate, se = std.error))
      
      
      # Transformar a estrutura dos dados
      reg3_long <- reg3 %>%
        pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
        arrange(var, type) %>%
        mutate(var = if_else(type == "se", "", var)) %>%
        select(-type)
      
      reg3_long <- rbind(reg3_long, data.frame(var = c("nobs"), value = c(nobs)))
      
      names(reg3_long) <-  c(' ',paste0(feature))
      
      if (feature == "fechamento"){
        results_event_study <- bind_cols(results_event_study,reg3_long)
      }else{
        results_event_study <- bind_cols(results_event_study,reg3_long[,2])
      }
      
    }
  }, error = function(e) {
    cat(paste("Erro ao processar", feature, ": ", conditionMessage(e), "\n"))
    # Pode adicionar lógica para lidar com o erro, se necessário
  })
}
results_mean_effect_privada <- results_mean_effect
results_effect_privada <- results_effect
results_event_study_privada <- results_event_study

View(results_mean_effect_privada)
View(results_effect_privada)
View(results_event_study_privada)

