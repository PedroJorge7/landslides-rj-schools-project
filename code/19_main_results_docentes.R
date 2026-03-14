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
    n_docentes_total = as.numeric(n_docentes_total)
  )


# Helpers para montar contagens docentes e logs
first_existing <- function(candidates, available = names(df)) {
  found <- candidates[candidates %in% available]
  if (length(found) == 0) return(NA_character_)
  found[1]
}

num_col_or_na <- function(data, col_name) {
  if (is.na(col_name) || !col_name %in% names(data)) {
    return(rep(NA_real_, nrow(data)))
  }
  as.numeric(data[[col_name]])
}

count_from_raw_or_pct <- function(data, raw_candidates, pct_candidates, total_var = "n_docentes_total") {
  raw_col <- first_existing(raw_candidates, names(data))
  pct_col <- first_existing(pct_candidates, names(data))

  raw_vals <- num_col_or_na(data, raw_col)
  pct_vals <- num_col_or_na(data, pct_col)
  total_vals <- as.numeric(data[[total_var]])

  pct_count <- ifelse(
    is.na(total_vals) | is.na(pct_vals),
    NA_real_,
    total_vals * pct_vals / 100
  )

  dplyr::coalesce(raw_vals, pct_count)
}


# Contagens alvo
# 1) licenciados
df$doc_licenciados <- count_from_raw_or_pct(
  data = df,
  raw_candidates = c("n_docentes_licenciatura", "n_docentes_com_licenciatura", "qt_doc_bas_licenciatura"),
  pct_candidates = c("pct_docentes_licenciados")
)

# 2) efetivos
df$doc_efetivos <- count_from_raw_or_pct(
  data = df,
  raw_candidates = c("n_docentes_efetivos", "n_docentes_concursados", "qt_docentes_efetivos", "qt_docentes_concursados"),
  pct_candidates = c("pct_docentes_efetivos")
)

# 3) temporarios
df$doc_temporarios <- count_from_raw_or_pct(
  data = df,
  raw_candidates = c("n_docentes_temporarios", "qt_docentes_temporarios"),
  pct_candidates = c("pct_docentes_temporarios")
)

# 4) pos-graduacao
df$doc_pos_graduacao <- count_from_raw_or_pct(
  data = df,
  raw_candidates = c("n_docentes_pos_graduacao", "n_docentes_com_pos_graduacao", "qt_doc_pos_graduacao"),
  pct_candidates = c("pct_docentes_pos_graduacao")
)

# 5) docentes do fundamental
df$doc_fundamental <- count_from_raw_or_pct(
  data = df,
  raw_candidates = c("qt_doc_fund", "n_docentes_fund", "n_docentes_fundamental"),
  pct_candidates = c("pct_docentes_fundamental")
)

# 6) docentes do medio
df$doc_medio <- count_from_raw_or_pct(
  data = df,
  raw_candidates = c("qt_doc_med", "n_docentes_medio", "n_docentes_ensino_medio"),
  pct_candidates = c("pct_docentes_medio")
)

# Logs (mesma logica do log_docente principal)
if (!"log_docente" %in% names(df)) {
  df$log_docente <- log(as.numeric(df$n_docentes_total) + 1)
}

df$log_doc_licenciados <- log(as.numeric(df$doc_licenciados) + 1)
df$log_doc_efetivos <- log(as.numeric(df$doc_efetivos) + 1)
df$log_doc_temporarios <- log(as.numeric(df$doc_temporarios) + 1)
df$log_doc_pos_graduacao <- log(as.numeric(df$doc_pos_graduacao) + 1)
df$log_doc_fundamental <- log(as.numeric(df$doc_fundamental) + 1)
df$log_doc_medio <- log(as.numeric(df$doc_medio) + 1)


# REGRESSION
df$pop_branca <- df$pop_branca * df$ano
df$income_total <- df$income_total * df$ano
df$pop_per_household <- df$pop_per_household * df$ano
df$pop_total <- df$pop_total * df$ano
df$urban <- df$urban * df$ano
df$favela <- df$favela * df$ano

controles <- c("income_total", "pop_per_household", "pop_branca", "urban", "favela")

df$log_doc_superior <- log(((df$dsu_media/100)*df$n_docentes_total)+1)
df$log_nao_doc_superior <- log(((1-(df$dsu_media/100))*df$n_docentes_total)+1)

# Outcomes docentes pedidos (todos em log)
outcomes_candidatos <- c(
  "log_doc_licenciados",
  "log_doc_efetivos",
  "log_doc_temporarios",
  "log_doc_pos_graduacao",
  "log_doc_fundamental",
  "log_doc_medio",
  "dsu_media",
  "log_doc_superior",
  "log_nao_doc_superior"
)

outcomes_principais <- outcomes_candidatos[outcomes_candidatos %in% names(df)]

non_missing_por_outcome <- sapply(outcomes_principais, function(feature) {
  vals <- suppressWarnings(as.numeric(df[[feature]]))
  sum(!is.na(vals))
})

if (length(non_missing_por_outcome) > 0) {
  cat("Obs nao-missing por outcome (antes do filtro de variacao):\n")
  for (nm in names(non_missing_por_outcome)) {
    cat(sprintf("- %s: %s\n", nm, format(non_missing_por_outcome[[nm]], big.mark = ".", decimal.mark = ",")))
  }
  cat("\n")
}

if (length(non_missing_por_outcome) > 0 && all(non_missing_por_outcome == 0)) {
  stop(paste0(
    "Todos os outcomes docentes pedidos estao 100% NA no painel final. ",
    "Isso ocorre porque as colunas brutas/pct de perfil docente nao existem (ou vieram vazias) nas fontes usadas no 01_geracao_base_de_dados. ",
    "Com os parquets atuais, so ha n_docentes_total (sem quebra de licenciatura/vinculo/pos/fund/medio)."
  ))
}

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
  stop("Nenhum outcome docente com variacao suficiente foi encontrado no painel.")
}

cat("Outcomes docentes utilizados:\n")
cat(paste0("- ", outcomes_principais), sep = "\n")
cat("\n")


## Main results: docentes (apenas especificacao com controle) ----------------------------
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

write_xlsx(results_all, "./results/tb_main_results_docentes.xlsx")
