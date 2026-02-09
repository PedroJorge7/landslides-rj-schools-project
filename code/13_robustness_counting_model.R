# =========================================================
# 02_counting_models_fechamento.R
# - COM CONTROLE apenas
# - 4 modelos: Poisson, NegBin, ZIP, ZINB (se pscl disponível)
# - 2 tabelas: ATT (treat) e Time treatment (treat_1yr..treat_9yr)
# - salva 2 .xlsx no estilo coef*** e (sd)
# =========================================================

library(arrow)
library(dplyr)
library(tidyr)
library(writexl)
library(sandwich)

pstars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p <= 0.01, "***",
                ifelse(p <= 0.05, "**",
                       ifelse(p <= 0.10, "*", ""))))
}

safe_fit <- function(expr) tryCatch(expr, error = function(e) NULL)

get_count_bV <- function(model, V) {
  b0 <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(b0) || is.null(V)) return(list(b = NULL, V = NULL))
  
  if (is.list(b0) && !is.null(b0$count)) {
    b <- b0$count
    nm <- names(b)
    rn <- rownames(V)
    
    if (!is.null(rn)) {
      if (all(nm %in% rn)) {
        sel <- nm
      } else if (all(paste0("count_", nm) %in% rn)) {
        sel <- paste0("count_", nm)
        names(b) <- sel
      } else {
        sel <- intersect(rn, paste0("count_", nm))
        if (length(sel) == 0) return(list(b = NULL, V = NULL))
        names(b) <- sel
      }
      V <- V[sel, sel, drop = FALSE]
    } else {
      V <- V[seq_along(b), seq_along(b), drop = FALSE]
    }
    return(list(b = b, V = V))
  }
  
  b <- b0
  
  if (!is.null(names(b)) && any(grepl("^count_", names(b)))) {
    b <- b[grepl("^count_", names(b))]
    if (!is.null(rownames(V))) V <- V[names(b), names(b), drop = FALSE]
    return(list(b = b, V = V))
  }
  
  if (!is.null(rownames(V)) && !is.null(names(b)) && all(names(b) %in% rownames(V))) {
    V <- V[names(b), names(b), drop = FALSE]
  }
  
  list(b = b, V = V)
}

robust_tab <- function(model, df, cluster_col, keep_terms, model_name) {
  out_empty <- tibble(
    model = model_name,
    term = keep_terms,
    estimate = NA_real_,
    std_error = NA_real_,
    p_value = NA_real_,
    N = as.numeric(NA)
  )
  
  if (is.null(model)) return(out_empty)
  
  res <- tryCatch({
    cl <- as.factor(df[[cluster_col]])
    
    V <- safe_fit(sandwich::vcovCL(model, cluster = cl, type = "HC1"))
    if (is.null(V)) V <- safe_fit(sandwich::vcovHC(model, type = "HC1"))
    if (is.null(V)) V <- safe_fit(vcov(model))
    if (is.null(V)) return(out_empty)
    
    bv <- get_count_bV(model, V)
    b  <- bv$b
    Vc <- bv$V
    if (is.null(b) || is.null(Vc)) return(out_empty)
    
    se <- sqrt(pmax(diag(Vc), 0))
    se[!is.finite(se)] <- NA_real_
    
    z <- b / se
    p <- 2 * stats::pnorm(-abs(z))
    
    tab <- tibble(
      model = model_name,
      term = names(b),
      estimate = as.numeric(b),
      std_error = as.numeric(se),
      p_value = as.numeric(p),
      N = as.numeric(tryCatch(stats::nobs(model), error = function(e) NA_real_))
    ) %>%
      mutate(term = gsub("^count_", "", term)) %>%
      filter(term %in% keep_terms)
    
    if (nrow(tab) == 0) out_empty else tab
  }, error = function(e) out_empty)
  
  res
}

make_xlsx_models <- function(tab_long, expected_terms, term_map) {
  tab_long <- tab_long %>%
    mutate(
      model = as.character(model),
      term = as.character(term),
      estimate = as.numeric(estimate),
      std_error = as.numeric(std_error),
      p_value = as.numeric(p_value),
      N = as.numeric(N)
    )
  
  models <- unique(tab_long$model)
  
  term_lbl <- unname(term_map[expected_terms])
  rows <- c(as.vector(rbind(term_lbl, rep("", length(term_lbl)))),
            "N", "School FE", "Time FE", "Census Control")
  
  out <- data.frame(" " = rows, check.names = FALSE, stringsAsFactors = FALSE)
  
  for (m in models) {
    sub <- tab_long %>% filter(model == m)
    
    coef_str <- rep("", length(expected_terms)); names(coef_str) <- expected_terms
    se_str   <- rep("", length(expected_terms)); names(se_str)   <- expected_terms
    
    if (nrow(sub) > 0) {
      idx <- match(sub$term, expected_terms)
      ok  <- which(!is.na(idx) & is.finite(sub$estimate) & is.finite(sub$std_error))
      
      sig <- pstars(sub$p_value[ok])
      coef_str[idx[ok]] <- paste0(format(round(sub$estimate[ok], 5), nsmall = 5), sig)
      se_str[idx[ok]]   <- paste0("(", format(round(sub$std_error[ok], 5), nsmall = 5), ")")
      
      N_s <- as.character(sub$N[!is.na(sub$N)][1])
    } else {
      N_s <- ""
    }
    
    vals <- as.vector(rbind(coef_str, se_str))
    vals <- c(vals, N_s, "No", "Yes", "Yes")
    
    out[[m]] <- vals
  }
  
  out
}

# -------------------------
# DATA
# -------------------------
path_panel <- "./output/painel_escolas.parquet"
dir_out    <- "./results"
dir.create(dir_out, showWarnings = FALSE)

df <- arrow::read_parquet(path_panel) %>%
  filter(raio == 1 | data.table::between(min_dist, 20, 30)) %>%
  filter(ano <= 2015)  %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep) %>%
  mutate(
    ano_fechamento = suppressWarnings(min(ano[tp_situacao_funcionamento != 1], na.rm = TRUE)),
    ano_fechamento = if_else(is.infinite(ano_fechamento), as.numeric(NA), as.numeric(ano_fechamento)),
    fechamento = if_else(!is.na(ano_fechamento) & ano == ano_fechamento, 1L, 0L),
    fechamento = if_else(!is.na(ano_fechamento) & ano > ano_fechamento, NA_integer_, fechamento)
  ) %>%
  ungroup()

df_risk <- df %>%
  filter(is.na(ano_fechamento) | ano <= ano_fechamento) %>%
  mutate(event = if_else(!is.na(ano_fechamento) & ano == ano_fechamento, 1L, 0L)) %>%
  as.data.frame()

controles <- c("income_total","pop_total","pop_per_household","urban","favela","pop_water_network")
df_risk <- df_risk %>%
  mutate(across(any_of(controles), ~ replace_na(as.numeric(.), 0)))

tt <- paste0("treat_", 1:5, "yr")
miss_tt <- setdiff(tt, names(df_risk))
if (length(miss_tt) > 0) df_risk[miss_tt] <- 0

zip_ok <- requireNamespace("pscl", quietly = TRUE)
nb_ok  <- requireNamespace("MASS", quietly = TRUE)

# remove NA no que entra na estimação (evita mismatch e NA-actions)
needed_att <- c("event", "treat", "ano", "code_inep", controles)
needed_tt  <- c("event", tt, "ano", "code_inep", controles)

df_risk_att <- df_risk %>% filter(complete.cases(dplyr::select(., any_of(needed_att))))
df_risk_tt  <- df_risk %>% filter(complete.cases(dplyr::select(., any_of(needed_tt))))

# -------------------------
# MODELOS (COM CONTROLE)
# -------------------------
fit_models_att <- function(df) {
  rhs <- c("treat", controles, "factor(ano)")
  f_p  <- as.formula(paste("event ~", paste(rhs, collapse = " + ")))
  f_zi <- as.formula(paste("event ~", paste(rhs, collapse = " + "), "| 1"))
  
  list(
    "Poisson" = safe_fit(glm(f_p, data = df, family = poisson(link = "log"))),
    "NegBin"  = if (nb_ok) safe_fit(MASS::glm.nb(f_p, data = df, control = glm.control(maxit = 200))) else NULL,
    "ZIP"     = if (zip_ok) safe_fit(pscl::zeroinfl(f_zi, data = df, dist = "poisson", link = "logit")) else NULL,
    "ZINB"    = if (zip_ok) safe_fit(pscl::zeroinfl(f_zi, data = df, dist = "negbin",  link = "logit")) else NULL
  )
}

fit_models_tt <- function(df) {
  rhs <- c(tt, controles, "factor(ano)")
  f_p  <- as.formula(paste("event ~", paste(rhs, collapse = " + ")))
  f_zi <- as.formula(paste("event ~", paste(rhs, collapse = " + "), "| 1"))
  
  list(
    "Poisson" = safe_fit(glm(f_p, data = df, family = poisson(link = "log"))),
    "NegBin"  = if (nb_ok) safe_fit(MASS::glm.nb(f_p, data = df, control = glm.control(maxit = 200))) else NULL,
    "ZIP"     = if (zip_ok) safe_fit(pscl::zeroinfl(f_zi, data = df, dist = "poisson", link = "logit")) else NULL,
    "ZINB"    = if (zip_ok) safe_fit(pscl::zeroinfl(f_zi, data = df, dist = "negbin",  link = "logit")) else NULL
  )
}

# -------------------------
# 1) ATT (treat)
# -------------------------
mods_att <- fit_models_att(df_risk_att)

tab_att <- bind_rows(lapply(names(mods_att), function(nm) {
  robust_tab(mods_att[[nm]], df_risk_att, "code_inep", keep_terms = "treat", model_name = nm)
}))

tab_att_xlsx <- make_xlsx_models(
  tab_att,
  expected_terms = "treat",
  term_map = c(treat = "Treat")
)

write_xlsx(tab_att_xlsx, file.path(dir_out, "counting_att_agregado.xlsx"))

# -------------------------
# 2) Time treatment (treat_1yr..treat_9yr)
# -------------------------
mods_tt <- fit_models_tt(df_risk_tt)

tab_tt <- bind_rows(lapply(names(mods_tt), function(nm) {
  robust_tab(mods_tt[[nm]], df_risk_tt, "code_inep", keep_terms = tt, model_name = nm)
}))

term_map_tt <- setNames(paste0("Treat ", 1:5), tt)

tab_tt_xlsx <- make_xlsx_models(
  tab_tt,
  expected_terms = tt,
  term_map = term_map_tt
)

write_xlsx(tab_tt_xlsx, file.path(dir_out, "counting_time_treatment.xlsx"))
