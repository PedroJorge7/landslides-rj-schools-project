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
results_mean_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df, feature = feature, type = "mean")
}))

rowlab <- c("", "coef", "", "N", "School FE", "Time FE", "Census Control")
# results_mean_effect <- results_mean_effect[-1]
results_mean_effect <- cbind(rowlab, results_mean_effect)
names(results_mean_effect) <- c("", paste0("(", 1:(ncol(results_mean_effect)-1), ")"))


results_effect <- do.call(cbind, lapply(outcomes_principais, function(feature) {
  process_feature(df = df, feature = feature, type = "time_effect")
}))


rowlab <- c(
  outcomes_principais[1],
  as.vector(rbind(paste0("Treat ", 1:9), rep("", 9))),
  "N", "School FE", "Time FE", "Census Control"
)
# results_effect <- results_effect[-1]
results_effect <- cbind(rowlab, results_effect)

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

write_xlsx(results_all, "./results/tb_results_effects_outros_outcomes.xlsx")
