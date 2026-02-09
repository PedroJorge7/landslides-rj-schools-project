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
  'School Closure',
  'Log of Number of Teachers',
  'Log of Number of Class',
  'Log of Number of Staff'
)


label <- c(
  "Fechamento da Escola",
  "Logaritmo do Número de Docentes",
  "Logaritmo do Número de Salas Existentes",
  "Logaritmo do Número de Funcionários"
)



# Load dataset
df <- arrow::read_parquet('./output/painel_escolas.parquet') %>% 
  filter(ano <= 2015) %>% 
  # filter(sum(n_alunos_total, na.rm = T) != 0) |> 
  filter(raio == 1 | data.table::between(min_dist,20,30))



# df$fechamento <- df$fechamento_ultimo_aberto

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

## Main results  ----------------------------

results_mean_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df, feature = feature, type = "mean")
}))
names(results_mean_effect) <- c("", paste0("(", 1:(ncol(results_mean_effect)-1), ")"))

results_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df, feature = feature, type = "time_effect")
}))
names(results_effect) <- c("", paste0("(", 1:(ncol(results_effect)-1), ")"))



results_mean_effect <- to_df_fix_names(results_mean_effect)
results_effect      <- to_df_fix_names(results_effect)

# --- alinha colunas ---
all_cols <- union(names(results_mean_effect), names(results_effect))


results_mean_effect <- add_missing_cols(results_mean_effect, all_cols)
results_effect      <- add_missing_cols(results_effect, all_cols)

# --- linha em branco ---
blank_row <- as.data.frame(as.list(rep("", length(all_cols))),
                           stringsAsFactors = FALSE, check.names = FALSE)
names(blank_row) <- all_cols

# --- junta (mean em cima, depois time) ---
results_all <- rbind(results_mean_effect, blank_row, results_effect)
rownames(results_all) <- NULL

write_xlsx(results_all, "./results/tb_main_results.xlsx")
