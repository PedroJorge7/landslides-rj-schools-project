library(fixest)
library(dplyr)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(writexl)

rm(list = ls())

source('./code/00_functions.R')

outcomes_principais <- c(
  'fechamento',
  'log_docente',
  'log_salas',
  'log_num_funcionarios'
)

label <- c(
  'A. School Closure (0/1)',
  'B. Log of Number of Teachers',
  'C. Log of Number of Class',
  'D. Log of Number of Staff'
)

controles <- c('income_total', 'pop_per_household', 'pop_branca', 'urban', 'favela')

style_files <- c(
  legacy = 'baseline_legacy',
  line_ribbon = 'option_1_line_ribbon',
  lollipop = 'option_2_lollipop',
  connected = 'option_3_connected'
)

prepare_panel <- function(limit_year = 2015) {
  arrow::read_parquet('./output/painel_escolas.parquet') %>%
    arrange(code_inep, ano) %>%
    group_by(code_inep) %>%
    filter(ano <= limit_year) %>%
    ungroup()
}

add_time_controls <- function(df) {
  df$pop_branca <- df$pop_branca * df$ano
  df$income_total <- df$income_total * df$ano
  df$pop_per_household <- df$pop_per_household * df$ano
  df$pop_total <- df$pop_total * df$ano
  df$urban <- df$urban * df$ano
  df$favela <- df$favela * df$ano
  df
}

build_dependency_output <- function(df) {
  df_filtered <- df %>%
    filter(raio == 1 | data.table::between(min_dist, 20, 30))

  output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
    bind_rows(
      process_plot_data(df = filter(df_filtered, is_publica == 1), feature, type = 'time_effect') %>% mutate(tipo = 'Public'),
      process_plot_data(df = filter(df_filtered, is_privada == 1), feature, type = 'time_effect') %>% mutate(tipo = 'Private')
    )
  }))

  output$term <- output$term + 2010
  output
}

build_change_control_output <- function(df) {
  output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
    bind_rows(
      process_plot_data(df = filter(df, raio == 1 | data.table::between(min_dist, 30, 40)), feature, type = 'time_effect') %>% mutate(tipo = '30-40 km'),
      process_plot_data(df = filter(df, raio == 1 | data.table::between(min_dist, 20, 40)), feature, type = 'time_effect') %>% mutate(tipo = '20-40 km'),
      process_plot_data(df = filter(df, raio == 1 | data.table::between(min_dist, 25, 30)), feature, type = 'time_effect') %>% mutate(tipo = '25-30 km'),
      process_plot_data(df = filter(df, raio == 1 | data.table::between(min_dist, 20, 25)), feature, type = 'time_effect') %>% mutate(tipo = '20-25 km'),
      process_plot_data(df = filter(df, raio == 1 | data.table::between(min_dist, 15, 20)), feature, type = 'time_effect') %>% mutate(tipo = '15-20 km')
    )
  }))

  output$term <- output$term + 2010
  output
}

dir.create('./results/style_options', recursive = TRUE, showWarnings = FALSE)

panel_2015 <- prepare_panel(limit_year = 2015)
panel_2015 <- add_time_controls(panel_2015)

output_dependency <- build_dependency_output(panel_2015)
output_change_control <- build_change_control_output(panel_2015)

for (style in names(style_files)) {
  dependency_plot <- plot_event_time_grid(data = output_dependency, style = style)
  ggsave(
    filename = sprintf('./results/style_options/%s_heterogeneity_dependency.jpg', style_files[[style]]),
    plot = dependency_plot,
    dpi = 300,
    width = 30,
    height = 15,
    units = 'cm'
  )

  change_control_plot <- plot_event_time_grid(data = output_change_control, style = style)
  ggsave(
    filename = sprintf('./results/style_options/%s_robustness_change_control.jpg', style_files[[style]]),
    plot = change_control_plot,
    dpi = 300,
    width = 30,
    height = 15,
    units = 'cm'
  )
}

writeLines(
  c(
    'baseline_legacy: current dot-whisker layout for reference',
    'option_1_line_ribbon: lines over time with confidence ribbons',
    'option_2_lollipop: horizontal lollipop intervals with stems from zero',
    'option_3_connected: original layout with emphasized confidence bars'
  ),
  './results/style_options/README.txt'
)


