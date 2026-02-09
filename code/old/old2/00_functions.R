# ============================================================
# 00_functions.R — PATCH (header com outcome + period automático)
# ============================================================

# -------------------------
# Trends (_trend)
# -------------------------
add_missing_trends <- function(df, controles_trend, year_col = "ano") {
  if (!year_col %in% names(df)) stop(paste0("Falta coluna ", year_col, " no df."))
  
  t0 <- min(df[[year_col]], na.rm = TRUE)
  df$trend_t <- df[[year_col]] - t0
  
  for (trend_name in controles_trend) {
    base <- sub("_trend$", "", trend_name)
    if (!trend_name %in% names(df) && base %in% names(df)) {
      df[[trend_name]] <- df[[base]] * df$trend_t
    }
  }
  df
}

# -------------------------
# Format helpers (SE fica VAZIO)
# -------------------------
fmt_sig <- function(p) ifelse(is.na(p), "", ifelse(p <= 0.01, "***", ifelse(p <= 0.05, "**", ifelse(p <= 0.10, "*", ""))))
fmt_num <- function(x) ifelse(is.na(x), "", format(round(x, 5), nsmall = 5))
fmt_est <- function(est, p) ifelse(is.na(est), "", paste0(fmt_num(est), fmt_sig(p)))

# -------------------------
# Blanks
# -------------------------
blank_mean <- function(feature) {
  data.frame(
    ` ` = c("ATT", "", "N", "School FE", "Time FE", "Census Control"),
    a = c("", "", "", "Yes", "Yes", "No"),
    b = c("", "", "", "Yes", "Yes", "Yes"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

blank_time <- function(feature) {
  lbl <- c("", as.vector(rbind(paste0(1:9, "yr"), rep("", 9))), "N", "School FE", "Time FE", "Census Control")
  data.frame(
    ` ` = lbl,
    a = c("", rep("", 18), "", "Yes", "Yes", "No"),
    b = c("", rep("", 18), "", "Yes", "Yes", "Yes"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# -------------------------
# process_feature() — MAIN RESULTS (mean/time)
#  - labels ATT e 1yr..9yr aparecem na 1a coluna
#  - linha de SE existe mas fica VAZIA
# -------------------------
process_feature <- function(df, feature, type) {
  
  if (!type %in% c("mean", "time_effect")) stop("Tipo inválido. Use 'mean' ou 'time_effect'.")
  if (!feature %in% names(df)) stop(paste0("Outcome não existe no df: ", feature))
  if (!"code_inep" %in% names(df)) stop("Falta code_inep no df.")
  if (!"ano" %in% names(df)) stop("Falta ano no df.")
  if (!"treat" %in% names(df)) stop("Falta treat no df.")
  
  ctr <- intersect(controles, names(df))
  
  x <- df[[feature]]
  if (all(is.na(x)) || length(unique(x[!is.na(x)])) <= 1) {
    warning(paste0("A variável ", feature, " não tem variação suficiente."))
    if (type == "mean") return(blank_mean(feature))
    if (type == "time_effect") return(blank_time(feature))
  }
  
  if (type == "mean") {
    
    f1 <- stats::as.formula(paste0(feature, " ~ treat | code_inep + ano"))
    rhs2 <- if (length(ctr) > 0) paste(c("treat", ctr), collapse = " + ") else "treat"
    f2 <- stats::as.formula(paste0(feature, " ~ ", rhs2, " | code_inep + ano"))
    
    reg1 <- tryCatch(fixest::feols(f1, data = df, cluster = "code_inep"), error = function(e) NULL)
    reg2 <- tryCatch(fixest::feols(f2, data = df, cluster = "code_inep"), error = function(e) NULL)
    
    if (is.null(reg1)) return(blank_mean(feature))
    
    t1 <- broom::tidy(reg1)
    r1 <- t1[t1$term == "treat", , drop = FALSE]
    est1 <- if (nrow(r1) == 1) fmt_est(r1$estimate[1], r1$p.value[1]) else ""
    n1 <- as.character(reg1$nobs)
    
    if (is.null(reg2)) {
      est2 <- ""; n2 <- ""
    } else {
      t2 <- broom::tidy(reg2)
      r2 <- t2[t2$term == "treat", , drop = FALSE]
      est2 <- if (nrow(r2) == 1) fmt_est(r2$estimate[1], r2$p.value[1]) else ""
      n2 <- as.character(reg2$nobs)
    }
    
    return(data.frame(
      ` ` = c("ATT", "", "N", "School FE", "Time FE", "Census Control"),
      a   = c(est1, "", n1, "Yes", "Yes", "No"),
      b   = c(est2, "", n2, "Yes", "Yes", "Yes"),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
  
  if (type == "time_effect") {
    
    treat_terms <- paste0("treat_", 1:9, "yr")
    if (!all(treat_terms %in% names(df))) return(blank_time(feature))
    
    f3 <- stats::as.formula(paste0(feature, " ~ ", paste(treat_terms, collapse = " + "), " | code_inep + ano"))
    rhs4 <- if (length(ctr) > 0) paste(c(treat_terms, ctr), collapse = " + ") else paste(treat_terms, collapse = " + ")
    f4 <- stats::as.formula(paste0(feature, " ~ ", rhs4, " | code_inep + ano"))
    
    reg1 <- tryCatch(fixest::feols(f3, data = df, cluster = "code_inep"), error = function(e) NULL)
    reg2 <- tryCatch(fixest::feols(f4, data = df, cluster = "code_inep"), error = function(e) NULL)
    
    if (is.null(reg1)) return(blank_time(feature))
    
    pull_est <- function(reg) {
      tt <- broom::tidy(reg)
      unlist(lapply(treat_terms, function(tm) {
        r <- tt[tt$term == tm, , drop = FALSE]
        if (nrow(r) == 0) return("")
        fmt_est(r$estimate[1], r$p.value[1])
      }))
    }
    
    est1 <- pull_est(reg1); n1 <- as.character(reg1$nobs)
    if (is.null(reg2)) { est2 <- rep("", 9); n2 <- "" } else { est2 <- pull_est(reg2); n2 <- as.character(reg2$nobs) }
    
    lbl  <- c("", as.vector(rbind(paste0(1:9, "yr"), rep("", 9))), "N", "School FE", "Time FE", "Census Control")
    colA <- c("", as.vector(rbind(est1, rep("", 9))), n1, "Yes", "Yes", "No")
    colB <- c("", as.vector(rbind(est2, rep("", 9))), n2, "Yes", "Yes", "Yes")
    
    return(data.frame(
      ` ` = lbl,
      a   = colA,
      b   = colB,
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
}

# -------------------------
# build_results_table()
#  - CABEÇALHO: nome do outcome (repetido 2x)
#  - 1a linha da planilha: "(1)" e "(2)" (ou seus labels)
# -------------------------
build_results_table <- function(df, outcomes, type,
                                spec_row = c("(1)", "(2)")) {
  
  tabs <- lapply(outcomes, function(feature) {
    tb <- process_feature(df, feature, type)
    colnames(tb) <- c(" ", feature, feature)
    tb
  })
  
  labels <- tabs[[1]][, 1, drop = FALSE]
  mats   <- lapply(tabs, function(tb) tb[, -1, drop = FALSE])
  
  res <- cbind(labels, do.call(cbind, mats))
  colnames(res)[1] <- ""
  
  # colnames viram OUTCOMES (repetidos 2x)
  colnames(res)[-1] <- rep(outcomes, each = 2)
  
  # adiciona linha com specs (1)/(2)
  hdr <- data.frame(
    t(c("", rep(spec_row, times = length(outcomes)))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  colnames(hdr) <- colnames(res)
  
  rbind(hdr, res)
}

# ============================================================
# EVENT STUDY — cria period automaticamente (com pré-trend)
# ============================================================
ensure_event_time <- function(df,
                              id = "code_inep",
                              year = "ano",
                              treat = "treat",
                              period_col = "period",
                              treat_unid_col = "treat_unid",
                              event_year_cols = c(
                                "ano_fechamento","ano_tratamento","ano_evento","event_year",
                                "treat_year","year_treat","year_event","year_closure"
                              )) {
  
  if (!id %in% names(df))   stop(paste0("Falta ", id))
  if (!year %in% names(df)) stop(paste0("Falta ", year))
  if (!treat %in% names(df)) stop(paste0("Falta ", treat))
  
  # treat_unid = ever treated
  if (!treat_unid_col %in% names(df)) {
    df <- df %>%
      dplyr::group_by(.data[[id]]) %>%
      dplyr::mutate(!!treat_unid_col := as.integer(max(.data[[treat]], na.rm = TRUE) == 1)) %>%
      dplyr::ungroup()
  }
  
  # acha coluna de ano do evento se existir
  ev_col <- event_year_cols[event_year_cols %in% names(df)]
  use_col <- if (length(ev_col) > 0) ev_col[1] else NA_character_
  
  if (!period_col %in% names(df)) {
    if (!is.na(use_col)) {
      # usa coluna do evento (por id)
      df <- df %>%
        dplyr::group_by(.data[[id]]) %>%
        dplyr::mutate(.event_year_tmp = suppressWarnings(min(.data[[use_col]], na.rm = TRUE))) %>%
        dplyr::ungroup()
      
      df$.event_year_tmp[is.infinite(df$.event_year_tmp)] <- NA_real_
      
    } else {
      # fallback: primeiro ano em que treat==1 por id
      df <- df %>%
        dplyr::group_by(.data[[id]]) %>%
        dplyr::mutate(.event_year_tmp = suppressWarnings(min(.data[[year]][.data[[treat]] == 1], na.rm = TRUE))) %>%
        dplyr::ungroup()
      
      df$.event_year_tmp[is.infinite(df$.event_year_tmp)] <- NA_real_
    }
    
    # period = ano - event_year para tratados; controles ficam 0 (não dropa)
    df[[period_col]] <- ifelse(
      df[[treat_unid_col]] == 1 & !is.na(df$.event_year_tmp),
      as.integer(df[[year]] - df$.event_year_tmp),
      0L
    )
    
    df$.event_year_tmp <- NULL
  }
  
  df
}

process_plot_data <- function(df, feature,
                              leads = 3, lags = 9, z = 1.645,
                              verbose = TRUE) {
  
  if (!feature %in% names(df)) stop(paste0("Outcome não existe no df: ", feature))
  if (!"code_inep" %in% names(df)) stop("Falta code_inep no df.")
  if (!"ano" %in% names(df)) stop("Falta ano no df.")
  if (!"treat" %in% names(df)) stop("Falta treat no df.")
  
  # cria period + treat_unid se faltar
  df <- ensure_event_time(df)
  
  ctr <- character(0)
  if (exists("controles", inherits = TRUE)) {
    ctr <- intersect(get("controles", inherits = TRUE), names(df))
  }
  
  x <- df[[feature]]
  if (all(is.na(x)) || length(unique(x[!is.na(x)])) <= 1 || sd(x, na.rm = TRUE) < 1e-6) {
    if (verbose) warning(paste("A variável", feature, "não tem variação suficiente."))
    return(data.frame(
      term = NA_real_, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
      type = "event_study", Regression = feature, conf.low = NA_real_, conf.high = NA_real_, nobs = NA_integer_
    ))
  }
  
  expected_k <- c((-leads):-1, 1:lags)
  
  rhs <- c(ctr, "i(period, treat_unid, ref = 0)")
  fml <- stats::as.formula(paste0(feature, " ~ ", paste(rhs, collapse = " + "), " | code_inep + ano"))
  reg <- fixest::feols(fml, data = df, cluster = "code_inep")
  
  tmp <- broom::tidy(reg) %>%
    dplyr::filter(grepl("treat_unid", term, fixed = TRUE)) %>%
    dplyr::mutate(
      term = as.numeric(gsub("period::", "", gsub(":treat_unid", "", term))),
      type = "event_study",
      Regression = feature,
      nobs = reg$nobs,
      conf.low  = estimate - z * std.error,
      conf.high = estimate + z * std.error
    ) %>%
    dplyr::filter(term %in% expected_k) %>%
    dplyr::select(term, estimate, std.error, statistic, p.value, type, Regression, conf.low, conf.high, nobs)
  
  tmp %>%
    tidyr::complete(
      term = expected_k,
      fill = list(
        estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
        conf.low = NA_real_, conf.high = NA_real_
      )
    ) %>%
    dplyr::mutate(type = "event_study", Regression = feature, nobs = reg$nobs) %>%
    dplyr::bind_rows(
      tibble::tibble(
        term = 0, estimate = 0, std.error = 0, statistic = 0, p.value = 0,
        type = "event_study", Regression = feature,
        conf.low = 0, conf.high = 0, nobs = reg$nobs
      )
    ) %>%
    dplyr::arrange(term)
}
