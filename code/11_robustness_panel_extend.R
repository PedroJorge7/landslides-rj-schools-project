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
  filter(ano <= 2019) %>% 
  filter(raio == 1 | data.table::between(min_dist,20,30))

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

# Robustez: Extendendo Painel  ----------------------------



# Aplicando a função para todas as variáveis com lapply
results_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df_robustez, feature, type = "time_effect")
}))

#results_effect <- results_effect[-(12:18),]
results_effect <- results_effect[,-c(3,5,7,9,11)]

names(results_effect) <- c("",paste0("(",1:(ncol(results_effect)-1),")"))


write_xlsx(results_effect, "./results/extend_panel.xlsx")
