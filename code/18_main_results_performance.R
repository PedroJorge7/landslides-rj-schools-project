library(fixest)
library(dplyr)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(writexl)

rm(list = ls())

source("./code/00_functions.R")
source("./code/00_performance_helpers.R")

controles <- performance_controls

df_main <- performance_prepare_panel(limit_year = 2015)
df_main <- performance_add_time_controls(df_main)

outcomes_principais <- performance_available_outcomes(df_main, performance_main_outcomes)

if (!"fechamento" %in% names(df_main)) {
  stop("A coluna 'fechamento' nao existe no painel. Nao foi possivel montar a tabela.")
}

if (length(unique(df_main$fechamento[!is.na(df_main$fechamento)])) <= 1) {
  stop("A coluna 'fechamento' nao tem variacao suficiente para servir de ancora na tabela.")
}

if (length(outcomes_principais) == 0) {
  stop("Nenhum outcome de desempenho com variacao suficiente foi encontrado no painel.")
}

cat("Outcomes de desempenho utilizados na tabela principal:\n")
cat(paste0("- ", outcomes_principais), sep = "\n")
cat("\n")

results_mean_effect <- performance_build_columns(df_main, outcomes_principais, "mean", model_position = "last")
results_effect <- performance_build_columns(df_main, outcomes_principais, "time_effect", model_position = "last")
results_all <- performance_bind_with_blank(results_mean_effect, results_effect)

write_xlsx(results_all, "./results/tb_main_results_performance.xlsx")
write_latex_table(
  results_all,
  "./results/tb_main_results_performance.tex",
  caption = "Main results for school abandonment and age-grade distortion.",
  label = "tab:main_results_performance"
)

df_event <- performance_prepare_panel(limit_year = 2015, fill_controls = TRUE, add_period = TRUE)
df_event <- performance_add_time_controls(df_event)

outcomes_principais <- performance_available_outcomes(df_event, performance_event_outcomes)
label <- performance_labels_for(outcomes_principais)

if (length(outcomes_principais) == 0) {
  stop("Nenhum outcome central de desempenho com variacao suficiente foi encontrado para o event study.")
}

cat("Outcomes de desempenho utilizados no event study:\n")
cat(paste0("- ", outcomes_principais), sep = "\n")
cat("\n")

results_event_study <- performance_build_columns(df_event, outcomes_principais, "event_study", model_position = "first")
write_xlsx(results_event_study, "./results/tb_event_study_performance.xlsx")
write_latex_table(
  results_event_study,
  "./results/tb_event_study_performance.tex",
  caption = "Event-study coefficients for school abandonment and age-grade distortion.",
  label = "tab:event_study_performance"
)

output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  process_plot_data(df = df_event, feature, type = "event_study")
}))

output <- output %>% tidyr::fill(Regression, .direction = "downup")
valid_event_output <- output %>% dplyr::filter(!is.na(Regression), !is.na(estimate))

if (!nrow(valid_event_output)) {
  stop("Nao foi possivel estimar o event study para abandono e distorcao idade-serie.")
}

event_study_plots <- lapply(outcomes_principais, plot_event_study)
event_study_panel <- cowplot::plot_grid(plotlist = event_study_plots, ncol = 2)

print(event_study_panel)

ggsave(
  filename = "./results/event_study_school_flow.jpg",
  plot = event_study_panel,
  dpi = 300,
  width = 40,
  height = 20,
  units = "cm"
)

ggsave(
  filename = "./results/event_study_performance.jpg",
  plot = event_study_panel,
  dpi = 300,
  width = 10,
  height = 5,
  units = "cm"
)
