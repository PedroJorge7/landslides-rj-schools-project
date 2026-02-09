# ============================================================
# 00_functions.R — FORÇADO: FE e cluster SÓ code_inep + ano
# - ZERO referência a pk_cod_entidade
# - Nunca retorna NULL (pra não quebrar do.call(cbind, ...))
# - Mean e Time effect
# ============================================================

fmt_sig <- function(p) {
  if (is.na(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.10) return("*")
  ""
}
fmt_num <- function(x, digits = 5) {
  if (is.na(x)) return("")
  format(round(x, digits), nsmall = digits)
}
fmt_est <- function(est, p, digits = 5) {
  if (is.na(est)) return("")
  paste0(fmt_num(est, digits), fmt_sig(p))
}
fmt_se <- function(se, digits = 5) {
  if (is.na(se)) return("")
  paste0("(", fmt_num(se, digits), ")")
}

.make_blank_mean <- function(feature, keep_label = FALSE) {
  rows <- c(feature, "", "N", "School FE", "Time FE", "Census Control")
  col1 <- c("", "", "", "Yes", "Yes", "No")
  col2 <- c("", "", "", "Yes", "Yes", "Yes")
  out <- data.frame(" " = rows, a = col1, b = col2, check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[2:3] <- c(feature, feature)
  if (!keep_label) out <- out[, 2:3, drop = FALSE]
  out
}

.make_blank_time <- function(feature, keep_label = FALSE) {
  rows <- as.vector(rbind(paste0("Treat ", 1:9), rep("", 9)))
  rows <- c(rows, "N", "School FE", "Time FE", "Census Control")
  
  col1 <- c(rep("", 18), "", "Yes", "Yes", "No")
  col2 <- c(rep("", 18), "", "Yes", "Yes", "Yes")
  
  out <- data.frame(" " = rows, a = col1, b = col2, check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[2:3] <- c(feature, feature)
  
  header <- as.data.frame(as.list(names(out)), stringsAsFactors = FALSE)
  names(header) <- names(out)
  out <- rbind(header, out)
  
  if (!keep_label) out <- out[, 2:3, drop = FALSE]
  out
}

process_feature <- function(df, feature, type, keep_label = FALSE) {
  
  if (!type %in% c("mean", "time_effect")) stop("Tipo inválido. Use 'mean' ou 'time_effect'.")
  if (!"code_inep" %in% names(df)) stop("Falta coluna code_inep no df.")
  if (!"ano" %in% names(df)) stop("Falta coluna ano no df.")
  if (!feature %in% names(df)) stop(paste0("Outcome não existe no df: ", feature))
  
  # controles: só os que existem no df
  ctr <- if (exists("controles")) controles else character(0)
  ctr <- intersect(ctr, names(df))
  
  # sem variação -> devolve vazio (mas no formato certo)
  x <- df[[feature]]
  if (all(is.na(x)) || length(unique(x[!is.na(x)])) <= 1) {
    warning(paste0("A variável ", feature, " não tem variação suficiente."))
    if (type == "mean") return(.make_blank_mean(feature, keep_label))
    if (type == "time_effect") return(.make_blank_time(feature, keep_label))
  }
  
  # -------------------------
  # MEAN
  # -------------------------
  if (type == "mean") {
    
    if (!"treat" %in% names(df)) stop("Falta coluna treat no df.")
    
    f1 <- stats::as.formula(paste0(feature, " ~ treat | code_inep + ano"))
    rhs2 <- if (length(ctr) > 0) paste(c("treat", ctr), collapse = " + ") else "treat"
    f2 <- stats::as.formula(paste0(feature, " ~ ", rhs2, " | code_inep + ano"))
    
    reg1 <- tryCatch(fixest::feols(f1, data = df, cluster = "code_inep"), error = function(e) NULL)
    reg2 <- tryCatch(fixest::feols(f2, data = df, cluster = "code_inep"), error = function(e) NULL)
    
    if (is.null(reg1)) return(.make_blank_mean(feature, keep_label))
    
    t1 <- broom::tidy(reg1)
    r1 <- t1[t1$term == "treat", , drop = FALSE]
    est1 <- if (nrow(r1) == 1) fmt_est(r1$estimate, r1$p.value) else ""
    se1  <- if (nrow(r1) == 1) fmt_se(r1$std.error) else ""
    
    if (is.null(reg2)) {
      est2 <- ""; se2 <- ""; n2 <- ""
    } else {
      t2 <- broom::tidy(reg2)
      r2 <- t2[t2$term == "treat", , drop = FALSE]
      est2 <- if (nrow(r2) == 1) fmt_est(r2$estimate, r2$p.value) else ""
      se2  <- if (nrow(r2) == 1) fmt_se(r2$std.error) else ""
      n2   <- as.character(reg2$nobs)
    }
    
    rows <- c(feature, "", "N", "School FE", "Time FE", "Census Control")
    col1 <- c(est1, se1, as.character(reg1$nobs), "Yes", "Yes", "No")
    col2 <- c(est2, se2, n2, "Yes", "Yes", "Yes")
    
    out <- data.frame(" " = rows, a = col1, b = col2, check.names = FALSE, stringsAsFactors = FALSE)
    names(out)[2:3] <- c(feature, feature)
    
    if (!keep_label) out <- out[, 2:3, drop = FALSE]
    return(out)
  }
  
  # -------------------------
  # TIME EFFECT
  # -------------------------
  if (type == "time_effect") {
    
    treat_terms <- paste0("treat_", 1:9, "yr")
    if (!all(treat_terms %in% names(df))) return(.make_blank_time(feature, keep_label))
    
    f3 <- stats::as.formula(paste0(feature, " ~ ", paste(treat_terms, collapse = " + "), " | code_inep + ano"))
    rhs4 <- if (length(ctr) > 0) paste(c(treat_terms, ctr), collapse = " + ") else paste(treat_terms, collapse = " + ")
    f4 <- stats::as.formula(paste0(feature, " ~ ", rhs4, " | code_inep + ano"))
    
    reg1 <- tryCatch(fixest::feols(f3, data = df, cluster = "code_inep"), error = function(e) NULL)
    reg2 <- tryCatch(fixest::feols(f4, data = df, cluster = "code_inep"), error = function(e) NULL)
    
    if (is.null(reg1)) return(.make_blank_time(feature, keep_label))
    
    tt1 <- broom::tidy(reg1)
    estse1 <- unlist(lapply(treat_terms, function(tm) {
      r <- tt1[tt1$term == tm, , drop = FALSE]
      if (nrow(r) == 0) return(c("", ""))
      c(fmt_est(r$estimate[1], r$p.value[1]), fmt_se(r$std.error[1]))
    }))
    col1 <- c(estse1, as.character(reg1$nobs), "Yes", "Yes", "No")
    
    if (is.null(reg2)) {
      col2 <- c(rep("", 18), "", "Yes", "Yes", "Yes")
    } else {
      tt2 <- broom::tidy(reg2)
      estse2 <- unlist(lapply(treat_terms, function(tm) {
        r <- tt2[tt2$term == tm, , drop = FALSE]
        if (nrow(r) == 0) return(c("", ""))
        c(fmt_est(r$estimate[1], r$p.value[1]), fmt_se(r$std.error[1]))
      }))
      col2 <- c(estse2, as.character(reg2$nobs), "Yes", "Yes", "Yes")
    }
    
    rows <- as.vector(rbind(paste0("Treat ", 1:9), rep("", 9)))
    rows <- c(rows, "N", "School FE", "Time FE", "Census Control")
    
    out <- data.frame(" " = rows, a = col1, b = col2, check.names = FALSE, stringsAsFactors = FALSE)
    names(out)[2:3] <- c(feature, feature)
    
    header <- as.data.frame(as.list(names(out)), stringsAsFactors = FALSE)
    names(header) <- names(out)
    out <- rbind(header, out)
    
    if (!keep_label) out <- out[, 2:3, drop = FALSE]
    return(out)
  }
}
    