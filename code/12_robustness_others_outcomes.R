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




# Load dataset
df <- arrow::read_parquet('./output/painel_escolas.parquet') %>% 
  arrange(code_inep, ano) |> 
  group_by(code_inep) |> 
  filter(ano <= 2015) |> 
  filter(raio == 1 | data.table::between(min_dist,20,30)) |> 
  mutate(in_internet = ifelse(is.na(n_alunos_total),NA,in_internet),
         in_agua_rede_publica = ifelse(is.na(n_alunos_total),NA,in_agua_rede_publica),
         in_biblioteca = ifelse(is.na(n_alunos_total),NA,in_biblioteca),
         in_quadra_esportes = ifelse(is.na(n_alunos_total),NA,in_quadra_esportes),
         in_computador = ifelse(is.na(n_alunos_total),NA,in_computador))

# REGRESSÃO

df$pop_branca <- df$pop_branca*df$ano
df$income_total <- df$income_total*df$ano
df$pop_per_household <- df$pop_per_household*df$ano
df$pop_total <- df$pop_total*df$ano
df$urban <- df$urban*df$ano
df$favela <- df$favela*df$ano

controles <- c("income_total","pop_per_household","pop_branca","urban","favela")

# Remover até 20 (até 30km)
# renda media e escolaridade
# fechamento. Alunos fundamental = 0 e existe fundamental


# Robustez: Outros outcomes  ----------------------------
outcomes_principais <- c('in_internet','in_agua_rede_publica','in_biblioteca','in_quadra_esportes',
                         'in_computador')

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


write_xlsx(results_effect, "./results/tb_results_effects_outros_outcomes_infraestrutura.xlsx")