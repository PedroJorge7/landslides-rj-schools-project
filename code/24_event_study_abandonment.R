library(arrow)
library(fixest)
library(dplyr)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)
library(ggplot2)

rm(list = ls())

####### FUNCTIONS
source('./code/00_functions.R')

outcomes_principais <- c(
  'aprov_media',
  'reprov_media',
  'aband_media',
  'tdi_media_calc'
)

label <- c(
  'A. Approval Rate',
  'B. Failure Rate',
  'C. School Abandonment Rate',
  'D. Age-Grade Distortion Rate'
)

build_stage_average <- function(df, fund_col, medio_col, output_col) {
  if (!fund_col %in% names(df)) {
    df[[fund_col]] <- NA_real_
  }

  if (!medio_col %in% names(df)) {
    df[[medio_col]] <- NA_real_
  }

  df[[fund_col]] <- suppressWarnings(as.numeric(df[[fund_col]]))
  df[[medio_col]] <- suppressWarnings(as.numeric(df[[medio_col]]))

  df[[output_col]] <- dplyr::case_when(
    !is.na(df[[fund_col]]) & !is.na(df[[medio_col]]) ~ rowMeans(cbind(df[[fund_col]], df[[medio_col]]), na.rm = TRUE),
    !is.na(df[[fund_col]]) ~ df[[fund_col]],
    !is.na(df[[medio_col]]) ~ df[[medio_col]],
    TRUE ~ NA_real_
  )

  df
}

# Load dataset
df <- arrow::read_parquet('./output/painel_escolas.parquet') %>%
  filter(ano <= 2015) |>
  filter(raio == 1 | data.table::between(min_dist, 20, 30)) |>
  tidyr::fill(
    lat, lon, income_total,
    pop_total, pop_per_household,
    urban, favela, pop_water_network,
    .direction = 'downup'
  )

df <- build_stage_average(df, 'aprov_cat_fund', 'aprov_cat_medio', 'aprov_media')
df <- build_stage_average(df, 'reprov_cat_fund', 'reprov_cat_medio', 'reprov_media')
df <- build_stage_average(df, 'aband_cat_fund', 'aband_cat_medio', 'aband_media')
df <- build_stage_average(df, 'tdi_fund', 'tdi_medio', 'tdi_media_calc')

for (feature in outcomes_principais) {
  feature_values <- df[[feature]][!is.na(df[[feature]])]
  if (length(unique(feature_values)) <= 1) {
    stop(paste0("A variavel '", feature, "' nao tem variacao suficiente para estimar o event study."))
  }
}

df$period <- df$ano - 2011 + 1

# REGRESSION

df$pop_branca <- df$pop_branca * df$ano
df$income_total <- df$income_total * df$ano
df$pop_per_household <- df$pop_per_household * df$ano
df$pop_total <- df$pop_total * df$ano
df$urban <- df$urban * df$ano
df$favela <- df$favela * df$ano

controles <- c('income_total', 'pop_per_household', 'pop_branca', 'urban', 'favela')

## Event study plot ----------------------------

output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  process_plot_data(df = df, feature, type = 'event_study')
}))

output <- output %>% tidyr::fill(Regression, .direction = 'downup')

if (!nrow(output) || all(is.na(output$estimate))) {
  stop('Nao foi possivel estimar o event study para aprovacao, reprovacao, abandono e distorcao idade-serie.')
}

event_study_plots <- lapply(outcomes_principais, plot_event_study)
event_study_panel <- cowplot::plot_grid(plotlist = event_study_plots, ncol = 2)

print(event_study_panel)

ggsave(
  filename = './results/event_study_school_flow.jpg',
  plot = event_study_panel,
  dpi = 300,
  width = 40,
  height = 20,
  units = 'cm'
)