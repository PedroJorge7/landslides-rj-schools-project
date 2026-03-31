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

# Load dataset
df <- arrow::read_parquet('./output/painel_escolas.parquet') %>% 
  arrange(code_inep, ano) |>
  group_by(code_inep) |>
  filter(ano <= 2015) |>
  filter(raio == 1 | data.table::between(min_dist,25,30))

# REGRESSION

df$pop_branca <- df$pop_branca*df$ano
df$income_total <- df$income_total*df$ano
df$pop_per_household <- df$pop_per_household*df$ano
df$pop_total <- df$pop_total*df$ano
df$urban <- df$urban*df$ano
df$favela <- df$favela*df$ano

controles <- c("income_total","pop_per_household","pop_branca","urban","favela")

# Heterogeneity: School size ----------------------------
porte_levels <- c(
  "Up to 50",
  "51 to 150",
  "151 to 300",
  "301 to 500",
  "More than 500"
)

df <- df |>
  group_by(code_inep) |>
  mutate(
    matriculas_2010 = ifelse(any(ano == 2010), max(ifelse(ano == 2010, n_alunos_total, NA), na.rm = TRUE), NA_real_),
    matriculas_2010 = ifelse(is.infinite(matriculas_2010), NA_real_, matriculas_2010),
    porte_escola = case_when(
      matriculas_2010 <= 50 ~ "Up to 50",
      dplyr::between(matriculas_2010, 51, 150) ~ "51 to 150",
      dplyr::between(matriculas_2010, 151, 300) ~ "151 to 300",
      dplyr::between(matriculas_2010, 301, 500) ~ "301 to 500",
      matriculas_2010 > 500 ~ "More than 500",
      TRUE ~ NA_character_
    ),
    porte_escola = factor(porte_escola, levels = porte_levels)
  ) |>
  ungroup()

output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(lapply(porte_levels, function(grupo) {
    process_plot_data(
      df = subset(df, porte_escola == grupo),
      feature,
      type = "time_effect"
    ) %>% mutate(tipo = grupo)
  }))
}))

output$term <- output$term + 2010
output$tipo <- factor(output$tipo, levels = porte_levels)

# Arrange plots
school_size_plot <- plot_event_time_grid(
  data = output,
  style = 'legacy',
  nrow = 2,
  ncol = 2,
  legend = 'bottom'
)

print(school_size_plot)

# Save plot
ggsave(
  filename = './results/tamanho_escola.jpg',
  plot = school_size_plot,
  dpi = 300,
  width = 30,
  height = 15,
  units = 'cm'
)