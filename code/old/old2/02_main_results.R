library(arrow)
library(fixest)
library(dplyr)
library(broom)
library(writexl)

rm(list = ls()); gc()

source("./code/00_functions.R")

# =========================
# CONFIG
# =========================
outcomes_principais <- c("fechamento", "log_docente", "log_aluno", "log_num_funcionarios")

controles <- c(
  "income_total_trend", "pop_total_trend", "pop_per_household_trend",
  "urban_trend", "favela_trend", "pop_water_network_trend"
)

# =========================
# LOAD DATA
# =========================
df <- arrow::open_dataset("./output/painel_escolas.parquet") %>%
  dplyr::filter(ano <= 2015) %>%
  dplyr::filter(raio == 1 | data.table::between(min_dist/1000, 20, 30)) %>%
  dplyr::collect()

df$fechamento <- df$fechamento_ultimo_aberto

# =========================
# CRIA *_trend se não existirem
# =========================
df <- add_missing_trends(df, controles_trend = controles, year_col = "ano")

# =========================
# MAIN RESULTS (AGORA COM 1a COLUNA: ATT / 1yr..9yr)
# E SE VAZIO
# =========================
results_mean_effect <- build_results_table(df, outcomes_principais, "mean",
                                           spec_row = c("Sem controle", "Com controle"))

results_effect <- build_results_table(df, outcomes_principais, "time_effect",
                                      spec_row = c("Sem controle", "Com controle"))

write_xlsx(results_mean_effect, "./output/infraestrutura/tb_results_mean_effects_vf_infraestrutura.xlsx")
write_xlsx(results_effect,      "./output/infraestrutura/tb_results_effects_vf_infraestrutura.xlsx")
