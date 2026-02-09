# =========================================================
# 01_hazard_cloglog.R  (hazard discreto via cloglog)
# - 2 tabelas: ATT agregado (treat) e Time treatment (treat_1yr..treat_9yr)
# - 2 specs: Sem controle / Com controle
# - salva 2 .xlsx no estilo coef*** e (sd)
# =========================================================

library(arrow)
library(dplyr)
library(tidyr)
library(writexl)
library(sandwich)

pstars <- function(p) {
  ifelse(p <= 0.01, "***",
         ifelse(p <= 0.05, "**",
                ifelse(p <= 0.10, "*", "")))
}

make_tab_style <- function(tab_long, expected_terms, term_map,
                           school_fe = "No", time_fe = "Yes",
                           census_map = c("Sem controle"="No","Com controle"="Yes")) {
  
  tab_long <- tab_long
  tab_long$spec     <- as.character(tab_long$spec)
  tab_long$term     <- as.character(tab_long$term)
  tab_long$estimate <- as.numeric(tab_long$estimate)
  tab_long$std_error<- as.numeric(tab_long$std_error)
  tab_long$p_value  <- as.numeric(tab_long$p_value)
  tab_long$N        <- as.numeric(tab_long$N)
  
  term_lbl <- unname(term_map[expected_terms])
  rows <- c(rbind(term_lbl, rep("", length(term_lbl))))
  rows <- c(rows, "N", "School FE", "Time FE", "Census Control")
  
  build_vals <- function(spec_name) {
    sub <- tab_long[tab_long$spec == spec_name, , drop = FALSE]
    
    coef_str <- rep("", length(expected_terms)); names(coef_str) <- expected_terms
    se_str   <- rep("", length(expected_terms)); names(se_str)   <- expected_terms
    
    if (nrow(sub) > 0) {
      idx <- match(sub$term, expected_terms)
      ok  <- which(!is.na(idx))
      sig <- pstars(sub$p_value[ok])
      
      coef_str[idx[ok]] <- paste0(format(round(sub$estimate[ok], 5), nsmall = 5), sig)
      se_str[idx[ok]]   <- paste0("(", format(round(sub$std_error[ok], 5), nsmall = 5), ")")
      N_s <- as.character(sub$N[!is.na(sub$N)][1])
    } else {
      N_s <- ""
    }
    
    vals <- as.vector(rbind(coef_str, se_str))
    vals <- c(vals, N_s, school_fe, time_fe, census_map[[spec_name]])
    vals
  }
  
  v0 <- build_vals("Sem controle")
  v1 <- build_vals("Com controle")
  
  data.frame(" " = rows,
             "Sem controle" = v0,
             "Com controle" = v1,
             check.names = FALSE,
             stringsAsFactors = FALSE)
}

get_glm_cluster <- function(model, cluster_vec, keep_terms, spec_name) {
  V <- sandwich::vcovCL(model, cluster = cluster_vec, type = "HC1")
  b <- stats::coef(model)
  se <- sqrt(diag(V))
  z <- b / se
  p <- 2 * stats::pnorm(-abs(z))
  
  tab <- tibble(
    spec = spec_name,
    term = names(b),
    estimate = as.numeric(b),
    std_error = as.numeric(se),
    p_value = as.numeric(p),
    N = nrow(stats::model.frame(model))
  ) %>%
    filter(term %in% keep_terms)
  
  tab
}

# paths
path_panel <- "./output/painel_escolas.parquet"
dir_out    <- "./results"
dir.create(dir_out, showWarnings = FALSE)

# data
df <- arrow::read_parquet(path_panel) %>%
  filter(raio == 1 | data.table::between(min_dist, 20, 30))

# fechamento + ano_fechamento
df <- df %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep) %>%
  mutate(
    ano_fechamento = suppressWarnings(min(ano[tp_situacao_funcionamento != 1], na.rm = TRUE)),
    ano_fechamento = if_else(is.infinite(ano_fechamento), as.numeric(NA), as.numeric(ano_fechamento)),
    fechamento = if_else(!is.na(ano_fechamento) & ano == ano_fechamento, 1L, 0L),
    fechamento = if_else(!is.na(ano_fechamento) & ano > ano_fechamento, NA_integer_, fechamento)
  ) %>%
  ungroup()

# risk set + evento
df_risk <- df %>%
  filter(is.na(ano_fechamento) | ano <= ano_fechamento) %>%
  mutate(
    event = if_else(!is.na(ano_fechamento) & ano == ano_fechamento, 1L, 0L)
  )

controles <- c("income_total","pop_total","pop_per_household","urban","favela","pop_water_network")
df_risk <- df_risk %>%
  mutate(across(any_of(controles), ~ replace_na(as.numeric(.), 0)))

# -------------------------
# ATT agregado (treat)
# -------------------------
f0_att <- as.formula(paste("event ~ treat + factor(ano)"))
f1_att <- as.formula(paste("event ~ treat +", paste(controles, collapse = " + "), "+ factor(ano)"))

m0_att <- glm(f0_att, data = df_risk, family = binomial(link = "cloglog"))
m1_att <- glm(f1_att, data = df_risk, family = binomial(link = "cloglog"))

tab_att <- bind_rows(
  get_glm_cluster(m0_att, df_risk$code_inep, "treat", "Sem controle"),
  get_glm_cluster(m1_att, df_risk$code_inep, "treat", "Com controle")
)

tab_att_xlsx <- make_tab_style(
  tab_att,
  expected_terms = "treat",
  term_map = c("treat" = "Treat")
)

write_xlsx(tab_att_xlsx, file.path(dir_out, "hazard_att_agregado.xlsx"))

# -------------------------
# Time treatment (treat_1yr..treat_9yr)
# -------------------------
tt <- paste0("treat_", 1:5, "yr")

f0_tt <- as.formula(paste("event ~", paste(tt, collapse = " + "), "+ factor(ano)"))
f1_tt <- as.formula(paste("event ~", paste(c(tt, controles), collapse = " + "), "+ factor(ano)"))

m0_tt <- glm(f0_tt, data = df_risk, family = binomial(link = "cloglog"))
m1_tt <- glm(f1_tt, data = df_risk, family = binomial(link = "cloglog"))

tab_tt <- bind_rows(
  get_glm_cluster(m0_tt, df_risk$code_inep, tt, "Sem controle"),
  get_glm_cluster(m1_tt, df_risk$code_inep, tt, "Com controle")
)

term_map_tt <- setNames(paste0("Treat ", 1:5), tt)

tab_tt_xlsx <- make_tab_style(
  tab_tt,
  expected_terms = tt,
  term_map = term_map_tt
)

write_xlsx(tab_tt_xlsx, file.path(dir_out, "hazard_time_treatment.xlsx"))
