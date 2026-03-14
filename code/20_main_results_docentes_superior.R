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
source("./code/00_functions.R")

# Load dataset
df <- arrow::read_parquet("./output/painel_escolas.parquet") %>%
  filter(ano <= 2015) %>%
  filter(raio == 1 | data.table::between(min_dist, 20, 30))

first_existing <- function(candidates, available = names(df)) {
  found <- candidates[candidates %in% available]
  if (length(found) == 0) return(NA_character_)
  found[1]
}

log_doc_total_col <- first_existing(c("log_docente", "log_docentes"), names(df))

if (is.na(log_doc_total_col) && !"n_docentes_total" %in% names(df)) {
  stop("Nem 'log_docente' nem 'n_docentes_total' existem no painel.")
}

if (!"dsu_media" %in% names(df)) {
  stop("A coluna 'dsu_media' nao existe no painel.")
}

doc_total <- if (!is.na(log_doc_total_col)) {
  pmax(exp(as.numeric(df[[log_doc_total_col]])) - 1, 0)
} else {
  as.numeric(df$n_docentes_total)
}

share_superior <- as.numeric(df$dsu_media) / 100
share_superior <- pmin(pmax(share_superior, 0), 1)
share_superior[is.na(as.numeric(df$dsu_media))] <- NA_real_

df$log_docentes_superior <- ifelse(
  is.na(doc_total) | is.na(share_superior),
  NA_real_,
  log(doc_total * share_superior + 1)
)

df$log_docentes_nao_superior <- ifelse(
  is.na(doc_total) | is.na(share_superior),
  NA_real_,
  log(doc_total * (1 - share_superior) + 1)
)

# Aliases para manter compatibilidade com scripts anteriores
if (!"log_doc_superior" %in% names(df)) {
  df$log_doc_superior <- df$log_docentes_superior
}

if (!"log_nao_doc_superior" %in% names(df)) {
  df$log_nao_doc_superior <- df$log_docentes_nao_superior
}

# REGRESSION
df$pop_branca <- df$pop_branca * df$ano
df$income_total <- df$income_total * df$ano
df$pop_per_household <- df$pop_per_household * df$ano
df$pop_total <- df$pop_total * df$ano
df$urban <- df$urban * df$ano
df$favela <- df$favela * df$ano

controles <- c("income_total", "pop_per_household", "pop_branca", "urban", "favela")

outcomes_candidatos <- c(
  "log_docentes_superior",
  "log_docentes_nao_superior"
)

outcomes_principais <- outcomes_candidatos[outcomes_candidatos %in% names(df)]

tem_variacao <- function(feature) {
  vals <- suppressWarnings(as.numeric(df[[feature]]))
  vals <- vals[!is.na(vals)]
  length(unique(vals)) > 1
}

outcomes_principais <- outcomes_principais[vapply(outcomes_principais, tem_variacao, logical(1))]

if (!"fechamento" %in% names(df)) {
  stop("A coluna 'fechamento' nao existe no painel. Nao foi possivel montar a tabela.")
}

if (length(unique(df$fechamento[!is.na(df$fechamento)])) <= 1) {
  stop("A coluna 'fechamento' nao tem variacao suficiente para servir de ancora na tabela.")
}

if (length(outcomes_principais) == 0) {
  stop("Nenhum outcome de docentes com/sem superior com variacao suficiente foi encontrado no painel.")
}

cat("Outcomes de docentes com/sem superior utilizados:\n")
cat(paste0("- ", outcomes_principais), sep = "\n")
cat("\n")

## Main results: docentes com/sem superior ----------------------------
get_control_column <- function(tbl, feature) {
  col <- tbl[, ncol(tbl), drop = FALSE]
  names(col) <- feature
  col
}

mean_row_label <- process_feature(df = df, feature = "fechamento", type = "mean")[, 1, drop = FALSE]
mean_control_list <- lapply(outcomes_principais, function(feature) {
  tbl <- process_feature(df = df, feature = feature, type = "mean")
  get_control_column(tbl, feature)
})
results_mean_effect <- do.call(cbind, c(list(mean_row_label), mean_control_list))
names(results_mean_effect) <- c("", paste0("(", 1:(ncol(results_mean_effect) - 1), ")"))

time_row_label <- process_feature(df = df, feature = "fechamento", type = "time_effect")[, 1, drop = FALSE]
time_control_list <- lapply(outcomes_principais, function(feature) {
  tbl <- process_feature(df = df, feature = feature, type = "time_effect")
  get_control_column(tbl, feature)
})
results_effect <- do.call(cbind, c(list(time_row_label), time_control_list))
names(results_effect) <- c("", paste0("(", 1:(ncol(results_effect) - 1), ")"))

results_mean_effect <- to_df_fix_names(results_mean_effect)
results_effect <- to_df_fix_names(results_effect)

all_cols <- union(names(results_mean_effect), names(results_effect))
results_mean_effect <- add_missing_cols(results_mean_effect, all_cols)
results_effect <- add_missing_cols(results_effect, all_cols)

blank_row <- as.data.frame(as.list(rep("", length(all_cols))),
  stringsAsFactors = FALSE, check.names = FALSE
)
names(blank_row) <- all_cols

results_all <- rbind(results_mean_effect, blank_row, results_effect)
rownames(results_all) <- NULL

write_xlsx(results_all, "./results/tb_main_results_docentes_superior.xlsx")
