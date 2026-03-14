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
  filter(raio == 1 | data.table::between(min_dist, 20, 30)) %>%
  mutate(
    aprov_media = rowMeans(cbind(as.numeric(aprov_cat_fund), as.numeric(aprov_cat_medio)), na.rm = TRUE),
    reprov_media = rowMeans(cbind(as.numeric(reprov_cat_fund), as.numeric(reprov_cat_medio)), na.rm = TRUE),
    aband_media = rowMeans(cbind(as.numeric(aband_cat_fund), as.numeric(aband_cat_medio)), na.rm = TRUE),
    ideb_media = rowMeans(cbind(as.numeric(ideb_ai), as.numeric(ideb_af), as.numeric(ideb_em)), na.rm = TRUE),
    portugues_media = rowMeans(cbind(as.numeric(ideb_pt_ai), as.numeric(ideb_pt_af), as.numeric(ideb_pt_em)), na.rm = TRUE),
    matematica_media = rowMeans(cbind(as.numeric(ideb_mat_ai), as.numeric(ideb_mat_af), as.numeric(ideb_mat_em)), na.rm = TRUE),
    had_fund = as.numeric(had_fund),
    had_medio = as.numeric(had_medio),
    had_media = rowMeans(cbind(had_fund, had_medio), na.rm = TRUE),
    tdi_fund = as.numeric(tdi_fund),
    tdi_medio = as.numeric(tdi_medio),
    tdi_media = rowMeans(cbind(tdi_fund, tdi_medio), na.rm = TRUE),
    aprov_media = ifelse(is.nan(aprov_media), NA_real_, aprov_media),
    reprov_media = ifelse(is.nan(reprov_media), NA_real_, reprov_media),
    aband_media = ifelse(is.nan(aband_media), NA_real_, aband_media),
    ideb_media = ifelse(is.nan(ideb_media), NA_real_, ideb_media),
    portugues_media = ifelse(is.nan(portugues_media), NA_real_, portugues_media),
    matematica_media = ifelse(is.nan(matematica_media), NA_real_, matematica_media),
    had_media = ifelse(is.nan(had_media), NA_real_, had_media),
    tdi_media = ifelse(is.nan(tdi_media), NA_real_, tdi_media)
  )


# REGRESSION
df$pop_branca <- df$pop_branca * df$ano
df$income_total <- df$income_total * df$ano
df$pop_per_household <- df$pop_per_household * df$ano
df$pop_total <- df$pop_total * df$ano
df$urban <- df$urban * df$ano
df$favela <- df$favela * df$ano

controles <- c("income_total", "pop_per_household", "pop_branca", "urban", "favela")


outcomes_candidatos <- c(
  "aprov_media", "reprov_media", "aband_media",
  "ideb_media", "portugues_media", "matematica_media",
  "had_fund", "had_medio", "had_media",
  "tdi_fund", "tdi_medio", "tdi_media"
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
  stop("Nenhum outcome de desempenho com variacao suficiente foi encontrado no painel.")
}

cat("Outcomes de desempenho utilizados:\n")
cat(paste0("- ", outcomes_principais), sep = "\n")
cat("\n")


## Main results: desempenho (apenas especificacao com controle) ----------------------------
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

# Align columns
all_cols <- union(names(results_mean_effect), names(results_effect))
results_mean_effect <- add_missing_cols(results_mean_effect, all_cols)
results_effect <- add_missing_cols(results_effect, all_cols)

# Blank line
blank_row <- as.data.frame(as.list(rep("", length(all_cols))),
  stringsAsFactors = FALSE, check.names = FALSE
)
names(blank_row) <- all_cols

# Bind (mean above, then time)
results_all <- rbind(results_mean_effect, blank_row, results_effect)
rownames(results_all) <- NULL

write_xlsx(results_all, "./results/tb_main_results_performance.xlsx")