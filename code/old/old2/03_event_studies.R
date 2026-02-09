library(arrow)
library(fixest)
library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)
library(cowplot)

rm(list = ls()); gc()

source("./code/00_functions.R")

# =========================
# CONFIG
# =========================
outcomes_principais <- c("fechamento", "log_docente", "log_aluno", "log_num_funcionarios")

label <- c(
  "Fechamento da Escola",
  "Logaritmo do Número de Docentes",
  "Logaritmo do Número de Alunos",
  "Logaritmo do Número de Funcionários"
)

controles <- c(
  "income_total_trend", "pop_total_trend", "pop_per_household_trend",
  "urban_trend", "favela_trend", "pop_water_network_trend"
)

# =========================
# LOAD DATA (IGUAL AO MAIN)
# =========================
df <- arrow::open_dataset("./output/painel_escolas.parquet") %>%
  dplyr::filter(ano <= 2015) %>%
  dplyr::filter(raio == 1 | data.table::between(min_dist/1000, 20, 30)) %>%
  dplyr::collect()

df$fechamento <- df$fechamento_ultimo_aberto

# =========================
# CRIA *_trend (IGUAL)
# =========================
df <- add_missing_trends(df, controles_trend = controles, year_col = "ano")

# =========================
# EVENT STUDY VARS (igual ao padrão)
# =========================
df$treat_unid <- as.integer(df$treat == 1)
df$period     <- as.integer(df$ano - 2010)

# =========================
# EVENT STUDY (SÓ ISSO)
# =========================
output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  process_plot_data(
    df = df,
    feature = feature,
    type = "event_study",
    base_year = 2010,
    leads = 3,
    lags = 9,
    z = 1.645
  )
}))

event_study_plots <- lapply(outcomes_principais, plot_event_study)
cowplot::plot_grid(plotlist = event_study_plots, ncol = 2)

ggsave(
  filename = "./output/infraestrutura/event_study_infraestrutura.png",
  dpi = 300, width = 40, height = 20, units = "cm"
)
