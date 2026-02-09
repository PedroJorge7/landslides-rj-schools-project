library(fixest)
library(dplyr)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(writexl)

rm(list = ls())


####### FUNCTIONS
source('./code/00_functions.R')


outcomes_principais     <- c('fechamento','log_docente',
                             'log_salas','log_num_funcionarios')

label <- c(
  "A. School Closure (0/1)",
  "B. Log of Number of Teachers",
  "C. Log of Number of Class",
  "D. Log of Number of Staff"
)


# label <- c(
#   "Fechamento da Escola",
#   "Logaritmo do Número de Docentes",
#   "Logaritmo do Número de Salas Existentes",
#   "Logaritmo do Número de Funcionários"
# )



# Load dataset
df_robustez <- arrow::read_parquet('./output/painel_escolas.parquet') %>% 
  arrange(code_inep, ano) |> 
  group_by(code_inep) |> 
  filter(ano <= 2015)

# REGRESSÃO

df_robustez$pop_branca <- df_robustez$pop_branca*df_robustez$ano
df_robustez$income_total <- df_robustez$income_total*df_robustez$ano
df_robustez$pop_per_household <- df_robustez$pop_per_household*df_robustez$ano
df_robustez$pop_total <- df_robustez$pop_total*df_robustez$ano
df_robustez$urban <- df_robustez$urban*df_robustez$ano
df_robustez$favela <- df_robustez$favela*df_robustez$ano

controles <- c("income_total","pop_per_household","pop_branca","urban","favela")

# Remover até 20 (até 30km)
# renda media e escolaridade
# fechamento. Alunos fundamental = 0 e existe fundamental



# Robustez: Mudando Raios de controle  ----------------------------



output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = subset(df_robustez, raio == 1 | data.table::between(min_dist,30,40)), feature, type = "time_effect") %>% mutate(tipo = '30-40 km'),
    process_plot_data(df = subset(df_robustez, raio == 1 | data.table::between(min_dist,20,40)), feature, type = "time_effect") %>% mutate(tipo = '20-40 km'),
    process_plot_data(df = subset(df_robustez, raio == 1 | data.table::between(min_dist,25,30)), feature, type = "time_effect") %>% mutate(tipo = '25-30 km'),
    process_plot_data(df = subset(df_robustez, raio == 1 | data.table::between(min_dist,20,25)), feature, type = "time_effect") %>% mutate(tipo = '20-25 km'),
    process_plot_data(df = subset(df_robustez, raio == 1 | data.table::between(min_dist,15,20)), feature, type = "time_effect") %>% mutate(tipo = '15-20 km'),
  )
}))

output$term <- output$term + 2010

# Usando cowplot para arranjar os gráficos
lapply(unique(output$Regression), plot_event_time) %>% 
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")


# Salvando o gráfico
ggsave(filename  = './results/mudando_raios_controle.jpg',
       dpi = 300, width = 30, height = 15, units = 'cm')
