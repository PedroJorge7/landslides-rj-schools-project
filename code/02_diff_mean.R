rm(list = ls()); gc()

library(arrow)
library(dplyr)
library(tidyr)
library(data.table)
library(xlsx)

# -------------------------
# CONFIG
# -------------------------
path_panel <- "./output/painel_escolas.parquet"
file_out   <- "./results/teste_diferenca_medias_2010_2011.xlsx"

outcomes_infra <- c(
  "fechamento","log_docente","log_salas","log_num_funcionarios",
  "income_total","pop_total","pop_per_household","urban","favela","pop_water_network"
)

vars_needed <- unique(c("ano","raio","min_dist", outcomes_infra))

# -------------------------
# HELPERS
# -------------------------
to_num <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  as.numeric(x)
}

stars_vec <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

safe_t_p <- function(v, g) {
  x <- v[g == "Afetado"];     x <- x[!is.na(x)]
  y <- v[g == "Nao_afetado"]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  if (sd(x) == 0 || sd(y) == 0) return(NA_real_)
  t.test(x, y)$p.value
}

fmt_mean <- function(x) {
  if (all(is.na(x))) return(NA_character_)
  sprintf("%.3f", mean(x, na.rm = TRUE))
}

fmt_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return("(-)")
  paste0("(", sprintf("%.3f", sd(x)), ")")
}

# -------------------------
# DATA (2010 vs 2011)
# -------------------------
df <- arrow::read_parquet(path_panel) %>%
  dplyr::select(dplyr::any_of(vars_needed)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(raio == 1 | data.table::between(min_dist, 20, 30)) %>%
  dplyr::filter(ano %in% c(2010, 2011)) %>%
  dplyr::mutate(
    period   = ifelse(ano == 2010, "pre_treatment", "pos_treatment"),
    treat_ds = ifelse(raio == 1, "Afetado", "Nao_afetado")
  ) %>%
  dplyr::mutate(dplyr::across(dplyr::any_of(outcomes_infra), to_num)) %>%
  as_tibble()

outcomes_infra <- outcomes_infra[outcomes_infra %in% names(df)]

dl <- df %>%
  tidyr::pivot_longer(
    cols = all_of(outcomes_infra),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  dplyr::mutate(outcome = factor(outcome, levels = outcomes_infra))

# -------------------------
# MEAN e SD (separados) por grupo/periodo  -> wide
# -------------------------
ms <- dl %>%
  dplyr::group_by(outcome, period, treat_ds) %>%
  dplyr::summarise(
    mean = fmt_mean(value),
    sd   = fmt_sd(value),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = treat_ds,
    values_from = c(mean, sd)
  ) %>%
  tidyr::pivot_wider(
    names_from  = period,
    values_from = c(mean_Afetado, mean_Nao_afetado, sd_Afetado, sd_Nao_afetado)
  )

# -------------------------
# DIFF + t.test + stars (sem SD) -> wide
# -------------------------
diff_w <- dl %>%
  dplyr::group_by(outcome, period) %>%
  dplyr::summarise(
    diff = mean(value[treat_ds == "Afetado"], na.rm = TRUE) -
      mean(value[treat_ds == "Nao_afetado"], na.rm = TRUE),
    p    = safe_t_p(value, treat_ds),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    diff = ifelse(is.finite(diff), diff, NA_real_),
    diff_mean = paste0(sprintf("%.3f", diff), stars_vec(p))
  ) %>%
  dplyr::select(outcome, period, diff_mean) %>%
  tidyr::pivot_wider(
    names_from  = period,
    values_from = diff_mean,
    names_prefix = "diff_mean_"
  )

base <- ms %>%
  dplyr::left_join(diff_w, by = "outcome") %>%
  dplyr::arrange(outcome) %>%
  dplyr::mutate(row_id = dplyr::row_number())

# -------------------------
# 7 COLUNAS, SD NA LINHA DE BAIXO (OUTRA LINHA MESMO)
# -------------------------
tab_top <- base %>%
  dplyr::transmute(
    row_id,
    line = 1L,
    outcome,
    mean_afetado_pre_treatment     = mean_Afetado_pre_treatment,
    mean_nao_afetado_pre_treatment = mean_Nao_afetado_pre_treatment,
    diff_mean_pre_treatment        = diff_mean_pre_treatment,
    mean_afetado_pos_treatment     = mean_Afetado_pos_treatment,
    mean_nao_afetado_pos_treatment = mean_Nao_afetado_pos_treatment,
    diff_mean_pos_treatment        = diff_mean_pos_treatment
  )

tab_bot <- base %>%
  dplyr::transmute(
    row_id,
    line = 2L,
    outcome = "",
    mean_afetado_pre_treatment     = sd_Afetado_pre_treatment,
    mean_nao_afetado_pre_treatment = sd_Nao_afetado_pre_treatment,
    diff_mean_pre_treatment        = "",
    mean_afetado_pos_treatment     = sd_Afetado_pos_treatment,
    mean_nao_afetado_pos_treatment = sd_Nao_afetado_pos_treatment,
    diff_mean_pos_treatment        = ""
  )

tab_infra <- dplyr::bind_rows(tab_top, tab_bot) %>%
  dplyr::arrange(row_id, line) %>%
  dplyr::select(-row_id, -line)

xlsx::write.xlsx(as.data.frame(tab_infra), file_out, sheetName = "infra", row.names = FALSE, showNA = FALSE)
