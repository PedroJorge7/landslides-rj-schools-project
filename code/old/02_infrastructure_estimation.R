# install.packages('rstudioapi')
# install.packages("fixest")
# install.packages("dplyr")
# install.packages("stargazer")
# install.packages('summarytools')
# install.packages('plm')
# install.packages('broom')
# install.packages('openxlsx')
# install.packages("tidyr")


# library(rstudioapi)
library(fixest)
library(dplyr)
# library(stargazer)
# library(summarytools)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)
library(ggplot2)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# print(getwd())
# 
# file.exists('df.rds')

# /Users/teste/Desktop/Script_Tese/

rm(list = ls())


####### FUNCTIONS

# save_results <- function(results, file) {
#   wb <- createWorkbook()
#   for (name in names(results)) {
#     addWorksheet(wb, name)
#     writeData(wb, name, results[[name]])
#   }
#   saveWorkbook(wb, file, overwrite = TRUE)
# }


outcomes_principais     <- c('fechamento','log_docente',
                             'log_aluno','log_num_funcionarios')

label <- c(
  'School Closure',
  'Log of Number of Teachers',
  'Log of Number of Students',
  'Log of Number of Staff'
)


label <- c(
  "Fechamento da Escola",
  "Logaritmo do Número de Docentes",
  "Logaritmo do Número de Alunos",
  "Logaritmo do Número de Funcionários"
)



# Load dataset
df <- readRDS('./data/df_final.rds') %>% 
  filter(ano <= 2015) %>% 
  filter(raio_otimo_rj == 1 | data.table::between(min_dist_rj,20,30))

# INSE
inse <- readxl::read_excel('./base/Indicador_INSE_por_Escola.xlsx', skip = 9) %>% 
  select(c(pk_cod_entidade = COD_ESCOLA, inse = "INSE - CLASSIFICAÇÃO" ))

df <- left_join(df,inse)
rm(inse)

count <- df %>% 
  group_by(treat,ano) %>% 
  dplyr::summarise(total = n()) %>% 
  pivot_wider(names_from = 'treat', values_from = 'total')


# REGRESSÃO
source('./code/00_functions.R')
library(writexl)
#features_model = c(variaveis, features_ideb, features_padronizadas)

df$pop_branca <- df$pop_branca*df$ano
df$income_total <- df$income_total*df$ano
df$pop_per_household <- df$pop_per_household*df$ano
df$pop_total <- df$pop_total*df$ano
df$urban <- df$urban*df$ano
df$favela <- df$favela*df$ano

controles <- c("pop_branca","income_total","pop_per_household",
               "pop_total","urban","favela")

controles <- c("income_total","pop_total",
               "pop_per_household",
               "urban","favela","pop_water_network")

# Remover até 20 (até 30km)
# renda media e escolaridade
# fechamento. Alunos fundamental = 0 e existe fundamental

## Main results  ----------------------------

# Aplicando a função para todas as variáveis com lapply
results_mean_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df, feature, type = "mean")
}))

names(results_mean_effect) <- c("",paste0("(",1:(ncol(results_mean_effect)-1),")"))

# Aplicando a função para todas as variáveis com lapply
results_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df, feature, type = "time_effect")
}))

names(results_effect) <- c("",paste0("(",1:(ncol(results_effect)-1),")"))
print(results_effect)


write_xlsx(results_mean_effect, "./output/infraestrutura/tb_results_mean_effects_vf_infraestrutura.xlsx")
write_xlsx(results_effect, "./output/infraestrutura/tb_results_effects_vf_infraestrutura.xlsx")

## Gráfico event study ----------------------------


# Aplicando para gerar os resultados
output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = df, feature, type = "time_effect"),
    process_plot_data(df = df, feature, type = "event_study")
  )
}))


output <- output %>% tidyr::fill(Regression, .direction = 'down')

# Criar múltiplos gráficos para `event_study`
event_study_plots <- lapply(unique(output$Regression), plot_event_study)
cowplot::plot_grid(plotlist = event_study_plots, ncol = 2)

# Salvar o gráfico
ggsave(filename  = './output/infraestrutura/event_study_infraestrutura.png',
       dpi=300, width = 40, height = 20, units='cm')

# Heterogeneidade: Regressões Público/Privado ----------------------------

# Aplicando a função para todas as variáveis com lapply

output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = subset(df,dependencia_adm == "Publica"), feature, type = "time_effect") %>% mutate(tipo = 'Publica'),
    process_plot_data(df = subset(df,dependencia_adm == "Privada"), feature, type = "time_effect") %>% mutate(tipo = 'Privada'),
  )
}))

output$term <- output$term + 2010

# Usando cowplot para arranjar os gráficos
lapply(unique(output$Regression), plot_event_time) %>% 
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")


# Salvando o gráfico
ggsave(filename  = './output/infraestrutura/dependencia_infraestrutura.png',
       dpi = 300, width = 30, height = 15, units = 'cm')



# Heterogeneidade: Regressões Rural/Urbana ----------------------------



output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = subset(df,id_localizacao == 1), feature, type = "time_effect") %>% mutate(tipo = 'Urbano'),
    process_plot_data(df = subset(df,id_localizacao == 2), feature, type = "time_effect") %>% mutate(tipo = 'Rural'),
  )
}))

output$term <- output$term + 2010

# Usando cowplot para arranjar os gráficos
lapply(unique(output$Regression), plot_event_time) %>% 
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")


# Salvando o gráfico
ggsave(filename  = './output/infraestrutura/localizacao_infraestrutura.png',
       dpi = 300, width = 30, height = 15, units = 'cm')



# Heterogeneidade: Regressões Fundamental/Médio/Ambos ----------------------------

output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = subset(df,in_fun == 1 & in_med == 0), feature, type = "time_effect") %>% mutate(tipo = 'Apenas Fundamental'),
    process_plot_data(df = subset(df,in_fun == 0 & in_med == 1), feature, type = "time_effect") %>% mutate(tipo = 'Apenas Médio'),
    process_plot_data(df = subset(df,in_fun == 1 & in_med == 1), feature, type = "time_effect") %>% mutate(tipo = 'Ambos'),
  )
}))

output$term <- output$term + 2010

# Usando cowplot para arranjar os gráficos
lapply(unique(output$Regression), plot_event_time) %>% 
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")


# Salvando o gráfico
ggsave(filename  = './output/infraestrutura/etapa_ensino_infraestrutura.png',
       dpi = 300, width = 30, height = 15, units = 'cm')

# Heterogeneidade: Tamanho da turma ----------------------------

output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = subset(df,  porte_escola == "<100"), feature, type = "time_effect") %>% mutate(tipo = "<100"),
    process_plot_data(df = subset(df,  porte_escola == "101-500"), feature, type = "time_effect") %>% mutate(tipo = "101-500"),
    process_plot_data(df = subset(df,  porte_escola == ">501"), feature, type = "time_effect") %>% mutate(tipo = ">501"),
  )
}))

output$term <- output$term + 2010

output$tipo <- factor(output$tipo, levels = c("<100","101-500",">501"))

# Usando cowplot para arranjar os gráficos
lapply(unique(output$Regression), plot_event_time) %>% 
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")


# Salvando o gráfico
ggsave(filename  = './output/infraestrutura/tamanho_escola_infraestrutura.png',
       dpi = 300, width = 30, height = 15, units = 'cm')



# Heterogeneidade: Médio-Baixo/Alto INSE ----------------------------

# output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
#   bind_rows(
#     process_plot_data(df = subset(df,  inse %in% c("Médio","Médio Baixo")), feature, type = "time_effect") %>% mutate(tipo = "Médio/Médio Baixo"),
#     process_plot_data(df = subset(df,  inse %in% c("Médio Alto")), feature, type = "time_effect") %>% mutate(tipo = "Médio Alto"),
#     process_plot_data(df = subset(df,  inse %in% c("Muito Alto","Alto")), feature, type = "time_effect") %>% mutate(tipo = "Muito Alto/Alto")
#   )
# }))
# 
# output$term <- output$term + 2010
# 
# output$tipo <- factor(output$tipo, levels = c("Médio/Médio Baixo","Médio Alto","Muito Alto/Alto"))
# 
# # Usando cowplot para arranjar os gráficos
# lapply(unique(output$Regression), plot_event_time) %>% 
#   ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
#                     common.legend = TRUE, legend = "bottom")
# 
# 
# # Salvando o gráfico
# ggsave(filename  = './output/infraestrutura/inse_infraestrutura.png',
#        dpi = 300, width = 30, height = 15, units = 'cm')

# Robustez: Mudando Raios de controle  ----------------------------


df_robustez <- readRDS('./data/df_final.rds') %>% 
  filter(ano <= 2015)



output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = subset(df_robustez, raio_otimo_rj == 1 | data.table::between(min_dist_rj,20,40)), feature, type = "time_effect") %>% mutate(tipo = '20-40 km'),
    process_plot_data(df = subset(df_robustez, raio_otimo_rj == 1 | data.table::between(min_dist_rj,20,50)), feature, type = "time_effect") %>% mutate(tipo = '20-50 km'),
    process_plot_data(df = subset(df_robustez, raio_otimo_rj == 1 | data.table::between(min_dist_rj,30,50)), feature, type = "time_effect") %>% mutate(tipo = '30-50 km'),
    process_plot_data(df = subset(df_robustez, raio_otimo_rj == 1 | data.table::between(min_dist_rj,0,30)), feature, type = "time_effect") %>% mutate(tipo = 'Até 30 km'),
  )
}))

output$term <- output$term + 2010

# Usando cowplot para arranjar os gráficos
lapply(unique(output$Regression), plot_event_time) %>% 
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")


# Salvando o gráfico
ggsave(filename  = './output/infraestrutura/mudando_raios_controle_infraestrutura.png',
       dpi = 300, width = 30, height = 15, units = 'cm')


# Robustez: Mudando Raios de tratamento  ----------------------------

df_robustez <- readRDS('./data/df_final.rds') %>% 
  filter(ano <= 2015)




output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = subset(df_robustez, data.table::between(min_dist_rj,0,2.5) | data.table::between(min_dist_rj,20,30)), feature, type = "time_effect") %>% mutate(tipo = '0-2.5 km'),
    process_plot_data(df = subset(df_robustez, data.table::between(min_dist_rj,0,5)   | data.table::between(min_dist_rj,20,30)), feature, type = "time_effect") %>% mutate(tipo = '0-5 km'),
    process_plot_data(df = subset(df_robustez, data.table::between(min_dist_rj,0,7.5) | data.table::between(min_dist_rj,20,30)), feature, type = "time_effect") %>% mutate(tipo = '0-7.5 km'),
    process_plot_data(df = subset(df_robustez, data.table::between(min_dist_rj,0,10)  | data.table::between(min_dist_rj,20,30)), feature, type = "time_effect") %>% mutate(tipo = '0-10 km'),
  )
}))

output$term <- output$term + 2010

# Usando cowplot para arranjar os gráficos
lapply(unique(output$Regression), plot_event_time) %>% 
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")


# Salvando o gráfico
ggsave(filename  = './output/infraestrutura/mudando_raios_tratado_infraestrutura.png',
       dpi = 300, width = 30, height = 15, units = 'cm')


# Robustez: Extendendo Painel  ----------------------------

df_robustez <- readRDS('./data/df_final.rds') %>% 
  subset(raio_otimo_rj == 1 | data.table::between(min_dist_rj,20,30))


# Aplicando a função para todas as variáveis com lapply
results_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df_robustez, feature, type = "time_effect")
}))

#results_effect <- results_effect[-(12:18),]
results_effect <- results_effect[,-c(3,5,7,9,11)]

names(results_effect) <- c("",paste0("(",1:(ncol(results_effect)-1),")"))


write_xlsx(results_effect, "./output/infraestrutura/extend_panel_infraestrutura.xlsx")


# Robustez: Outros outcomes  ----------------------------
outcomes_principais <- c('id_internet','id_agua_rede_publica','biblioteca','quadra_esportes',
                             'computador')

label <- c(
  'Internet Access',
  'Water Supply',
  'Library Availability',
  'Sports Court',
  'Computer Availability'
)

label <- c(
  'Acesso à Internet',
  'Abastecimento de Água',
  'Disponibilidade de Biblioteca',
  'Quadra de Esportes',
  'Disponibilidade de Computadores'
)


# Aplicando a função para todas as variáveis com lapply
results_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df, feature, type = "time_effect")
}))

results_effect <- results_effect[-(12:18),]
results_effect <- results_effect[,-c(3,5,7,9,11)]

names(results_effect) <- c("",paste0("(",1:(ncol(results_effect)-1),")"))


write_xlsx(results_effect, "./output/infraestrutura/tb_results_effects_outros_outcomes_infraestrutura.xlsx")