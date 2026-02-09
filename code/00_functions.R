process_feature <- function(df, feature, type, cluster_var = "code_inep") {
  if (!type %in% c("mean", "time_effect", "event_study")) {
    stop("Tipo inválido. Escolha entre 'mean', 'time_effect', ou 'event_study'.")
  }
  
  if (length(unique(df[[feature]])) <= 1) {
    warning(paste("A variável", feature, "não tem variação suficiente."))
    return(NULL)
  }
  
  results <- NULL
  expected_terms <- paste0("treat_", 1:9, "yr")
  
  formula_1 <- as.formula(paste(feature, "~ treat | code_inep + ano"))
  formula_2 <- as.formula(paste(feature, paste0("~ treat + ", paste0(controles, collapse = " + "),
                                                "| code_inep + ano")))
  formula_3 <- as.formula(paste(feature, "~", paste0("treat_", 1:9, "yr", collapse = " + "),
                                "| code_inep + ano"))
  formula_4 <- as.formula(paste(feature, "~",
                                paste0(c(paste0("treat_", 1:9, "yr"), controles), collapse = " + "),
                                "| code_inep + ano"))
  formula_5 <- as.formula(paste(feature, "~ i(period, treat_unid, 0) | code_inep + ano"))
  formula_6 <- as.formula(paste(feature, "~ i(period, treat_unid, 0) + ",
                                paste0(controles, collapse = " + "),
                                "| code_inep + ano + fk_cod_municipio * ano"))
  
  pstars <- function(p) {
    ifelse(p <= 0.01, "***",
           ifelse(p <= 0.05, "**",
                  ifelse(p <= 0.10, "*", "")))
  }
  
  empty_time_table <- function(census_yes) {
    rows <- as.vector(rbind(paste0("Treat ", 1:9), rep("", 9)))
    out <- data.frame(
      var   = c(rows, "N", "School FE", "Time FE", "Census Control"),
      value = c(rep("", length(rows)), "", "Yes", "Yes", ifelse(census_yes, "Yes", "No")),
      stringsAsFactors = FALSE
    )
    out
  }
  
  empty_es_table <- function(census_yes) {
    terms <- c(-3, -2, -1, 1:9)
    rows  <- as.vector(rbind(paste0("Treat ", terms), rep("", length(terms))))
    out <- data.frame(
      var   = c(rows, "N", "School FE", "Time FE", "Census Control"),
      value = c(rep("", length(rows)), "", "Yes", "Yes", ifelse(census_yes, "Yes", "No")),
      stringsAsFactors = FALSE
    )
    out
  }
  
  tryCatch({
    if (type == "mean") {
      reg1_sem_tendencia <- fixest::feols(formula_1, data = df, cluster = cluster_var)
      nobs <- reg1_sem_tendencia$nobs
      
      reg1_sem_tendencia <- broom::tidy(reg1_sem_tendencia) %>%
        dplyr::filter(grepl("treat", term)) %>%
        dplyr::mutate(
          term = feature,
          p.value_sig = pstars(p.value),
          estimate = paste0(format(round(estimate, 5), nsmall = 5), p.value_sig),
          std.error = paste0("(", format(round(std.error, 5), nsmall = 5), ")")
        ) %>%
        dplyr::transmute(` ` = term, coef = estimate, `  ` = std.error) %>%
        dplyr::mutate(
          N = nobs,
          `School FE` = "Yes",
          `Time FE` = "Yes",
          `Census Control` = "No"
        ) %>%
        t
      
      reg1_com_tendencia <- tryCatch({
        model <- fixest::feols(formula_2, data = df, cluster = cluster_var)
        nobs2 <- model$nobs
        
        broom::tidy(model) %>%
          dplyr::filter(grepl("treat", term)) %>%
          dplyr::mutate(
            term = feature,
            p.value_sig = pstars(p.value),
            estimate = paste0(format(round(estimate, 5), nsmall = 5), p.value_sig),
            std.error = paste0("(", format(round(std.error, 5), nsmall = 5), ")")
          ) %>%
          dplyr::transmute(` ` = term, coef = estimate, `  ` = std.error) %>%
          dplyr::mutate(
            N = nobs2,
            `School FE` = "Yes",
            `Time FE` = "Yes",
            `Census Control` = "Yes"
          ) %>%
          t
      }, error = function(e) {
        tibble::tibble(
          ` ` = feature, coef = "", `  ` = "", N = "",
          `School FE` = "Yes", `Time FE` = "Yes", `Census Control` = "Yes"
        ) %>% t()
      })
      
      results <- cbind(reg1_sem_tendencia, reg1_com_tendencia)
      results <- data.frame(" " = rownames(results), results, check.names = FALSE)
      rownames(results) <- 1:nrow(results)
      
      if (feature %in% c("fechamento", "id_internet")) {
        names(results) <- c(" ", paste0(feature), paste0(feature))
        return(results)
      } else {
        results <- results[, 2:3, drop = FALSE]
        names(results) <- c(paste0(feature), paste0(feature))
        return(results)
      }
    }
    
    if (type == "time_effect") {
      reg2_sem_tendencia <- fixest::feols(formula_3, data = df, cluster = cluster_var)
      nobs <- reg2_sem_tendencia$nobs
      
      reg2_sem_tendencia <- broom::tidy(reg2_sem_tendencia) %>%
        dplyr::filter(grepl("treat", term)) %>%
        dplyr::mutate(
          p.value_sig = pstars(p.value),
          estimate = paste0(format(round(estimate, 5), nsmall = 5), p.value_sig),
          std.error = paste0("(", format(round(std.error, 5), nsmall = 5), ")")
        ) %>%
        dplyr::transmute(var = term, coef = estimate, se = std.error) %>%
        tidyr::complete(var = expected_terms, fill = list(coef = "", se = "")) %>%
        dplyr::mutate(var = gsub("treat_", "Treat ", gsub("yr", "", var))) %>%
        tidyr::pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
        dplyr::mutate(var = dplyr::if_else(type == "se", "", var)) %>%
        dplyr::select(var, value) %>%
        as.data.frame(stringsAsFactors = FALSE)
      
      reg2_sem_tendencia <- rbind(
        reg2_sem_tendencia,
        data.frame(var = "N", value = as.character(nobs), stringsAsFactors = FALSE),
        data.frame(var = "School FE", value = "Yes", stringsAsFactors = FALSE),
        data.frame(var = "Time FE", value = "Yes", stringsAsFactors = FALSE),
        data.frame(var = "Census Control", value = "No", stringsAsFactors = FALSE)
      )
      
      reg2_com_tendencia <- tryCatch({
        model <- fixest::feols(formula_4, data = df, cluster = cluster_var)
        nobs2 <- model$nobs
        
        out <- broom::tidy(model) %>%
          dplyr::filter(grepl("treat", term)) %>%
          dplyr::mutate(
            p.value_sig = pstars(p.value),
            estimate = paste0(format(round(estimate, 5), nsmall = 5), p.value_sig),
            std.error = paste0("(", format(round(std.error, 5), nsmall = 5), ")")
          ) %>%
          dplyr::transmute(var = term, coef = estimate, se = std.error) %>%
          tidyr::complete(var = expected_terms, fill = list(coef = "", se = "")) %>%
          dplyr::mutate(var = gsub("treat_", "Treat ", gsub("yr", "", var))) %>%
          tidyr::pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
          dplyr::mutate(var = dplyr::if_else(type == "se", "", var)) %>%
          dplyr::select(var, value) %>%
          as.data.frame(stringsAsFactors = FALSE)
        
        out <- rbind(
          out,
          data.frame(var = "N", value = as.character(nobs2), stringsAsFactors = FALSE),
          data.frame(var = "School FE", value = "Yes", stringsAsFactors = FALSE),
          data.frame(var = "Time FE", value = "Yes", stringsAsFactors = FALSE),
          data.frame(var = "Census Control", value = "Yes", stringsAsFactors = FALSE)
        )
        out
      }, error = function(e) {
        empty_time_table(census_yes = TRUE)
      })
      
      results <- cbind(reg2_sem_tendencia, reg2_com_tendencia[, 2, drop = FALSE])
      names(results) <- c(" ", paste0(feature), paste0(feature))
      results <- rbind(names(results), results)
      
      if (feature %in% c("fechamento", "id_internet")) {
        return(results)
      } else {
        results <- results[, 2:3, drop = FALSE]
        return(results)
      }
    }
    
    if (type == "event_study") {
      expected_terms_es <- c(paste0("period::", (3:1) * -1, ":treat_unid"),
                             paste0("period::", 1:9, ":treat_unid"))
      
      reg3_sem_tendencia <- fixest::feols(formula_5, data = df, cluster = cluster_var)
      nobs <- reg3_sem_tendencia$nobs
      
      reg3_sem_tendencia <- broom::tidy(reg3_sem_tendencia) %>%
        dplyr::filter(grepl("treat", term)) %>%
        dplyr::mutate(
          p.value_sig = pstars(p.value),
          estimate = paste0(format(round(estimate, 5), nsmall = 5), p.value_sig),
          std.error = paste0("(", format(round(std.error, 5), nsmall = 5), ")")
        ) %>%
        dplyr::transmute(var = term, coef = estimate, se = std.error) %>%
        tidyr::complete(var = expected_terms_es, fill = list(coef = "", se = "")) %>%
        dplyr::mutate(var = gsub("period::", "Treat ", gsub(":treat_unid", "", var))) %>%
        tidyr::pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
        dplyr::mutate(var = dplyr::if_else(type == "se", "", var)) %>%
        dplyr::select(var, value) %>%
        as.data.frame(stringsAsFactors = FALSE)
      
      reg3_sem_tendencia <- rbind(
        reg3_sem_tendencia,
        data.frame(var = "N", value = as.character(nobs), stringsAsFactors = FALSE),
        data.frame(var = "School FE", value = "Yes", stringsAsFactors = FALSE),
        data.frame(var = "Time FE", value = "Yes", stringsAsFactors = FALSE),
        data.frame(var = "Census Control", value = "No", stringsAsFactors = FALSE)
      )
      
      reg3_com_tendencia <- tryCatch({
        model <- fixest::feols(formula_6, data = df, cluster = cluster_var)
        nobs2 <- model$nobs
        
        out <- broom::tidy(model) %>%
          dplyr::filter(grepl("treat", term)) %>%
          dplyr::mutate(
            p.value_sig = pstars(p.value),
            estimate = paste0(format(round(estimate, 5), nsmall = 5), p.value_sig),
            std.error = paste0("(", format(round(std.error, 5), nsmall = 5), ")")
          ) %>%
          dplyr::transmute(var = term, coef = estimate, se = std.error) %>%
          tidyr::complete(var = expected_terms_es, fill = list(coef = "", se = "")) %>%
          dplyr::mutate(var = gsub("period::", "Treat ", gsub(":treat_unid", "", var))) %>%
          tidyr::pivot_longer(cols = coef:se, names_to = "type", values_to = "value") %>%
          dplyr::mutate(var = dplyr::if_else(type == "se", "", var)) %>%
          dplyr::select(var, value) %>%
          as.data.frame(stringsAsFactors = FALSE)
        
        out <- rbind(
          out,
          data.frame(var = "N", value = as.character(nobs2), stringsAsFactors = FALSE),
          data.frame(var = "School FE", value = "Yes", stringsAsFactors = FALSE),
          data.frame(var = "Time FE", value = "Yes", stringsAsFactors = FALSE),
          data.frame(var = "Census Control", value = "Yes", stringsAsFactors = FALSE)
        )
        out
      }, error = function(e) {
        empty_es_table(census_yes = TRUE)
      })
      
      results <- cbind(reg3_sem_tendencia, reg3_com_tendencia[, 2, drop = FALSE])
      names(results) <- c(" ", paste0(feature), paste0(feature))
      results <- rbind(names(results), results)
      
      if (feature %in% c("fechamento", "id_internet")) {
        return(results)
      } else {
        results <- results[, 2:3, drop = FALSE]
        return(results)
      }
    }
  }, error = function(e) {
    cat(paste("Erro ao processar", feature, ": ", conditionMessage(e), "\n"))
  })
  
  results
}


process_plot_data <- function(df = df_balanceado, feature, type, verbose = TRUE, cluster_var = "code_inep") {
  if (!type %in% c("time_effect", "event_study")) {
    stop("Tipo inválido. Escolha entre 'time_effect' ou 'event_study'.")
  }
  
  if (length(unique(df[[feature]])) <= 1 || sd(df[[feature]], na.rm = TRUE) < 1e-6) {
    if (verbose) warning(paste("A variável", feature, "não tem variação suficiente."))
    return(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA,
                      p.value = NA, type = NA, Regression = NA,
                      conf.low = NA, conf.high = NA, nobs = NA))
  }
  
  results <- NULL
  
  tryCatch({
    z_90 <- 1.645
    
    if (type == "time_effect") {
      formula <- as.formula(paste(feature, "~",
                                  paste0(c(paste0("treat_", 1:9, "yr"), controles), collapse = " + "),
                                  "| code_inep + ano"))
      
      reg <- fixest::feols(formula, data = df, cluster = cluster_var)
      
      results <- broom::tidy(reg) %>%
        dplyr::filter(grepl("treat", term)) %>%
        dplyr::mutate(
          type = "time_effect",
          Regression = feature,
          conf.low = estimate - z_90 * std.error,
          conf.high = estimate + z_90 * std.error,
          nobs = reg$nobs,
          term = as.numeric(gsub("treat_", "", gsub("yr", "", term)))
        )
    }
    
    if (type == "event_study") {
      formula <- as.formula(paste(feature, "~ i(period, treat_unid, 0) | code_inep + ano"))
      
      reg <- fixest::feols(formula, data = df, cluster = cluster_var)
      
      results <- broom::tidy(reg) %>%
        dplyr::filter(grepl("treat_unid", term)) %>%
        dplyr::mutate(
          type = "event_study",
          Regression = feature,
          conf.low = estimate - z_90 * std.error,
          conf.high = estimate + z_90 * std.error,
          nobs = reg$nobs,
          term = as.numeric(gsub("period::", "", gsub(":treat_unid", "", term)))
        ) %>%
        dplyr::bind_rows(
          tibble::tibble(
            term = 0, estimate = 0, std.error = 0, statistic = 0, p.value = 0,
            type = "event_study", Regression = unique(results$Regression)[1],
            conf.low = 0, conf.high = 0, nobs = unique(results$nobs)[1]
          )
        ) %>%
        dplyr::arrange(term)
    }
  }, error = function(e) {
    if (verbose) cat(paste("Erro ao processar", feature, ":", conditionMessage(e), "\n"))
  })
  
  if (is.null(results)) {
    results <- data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA,
                          p.value = NA, type = NA, Regression = NA,
                          conf.low = NA, conf.high = NA, nobs = NA)
  }
  
  results
}


to_df_fix_names <- function(x){
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  cn <- names(x)
  cn[is.na(cn) | cn == ""] <- " "
  names(x) <- cn
  x
}

add_missing_cols <- function(x, cols){
  miss <- setdiff(cols, names(x))
  if(length(miss)){
    for(m in miss) x[[m]] <- ""
  }
  x[, cols, drop = FALSE]
}




plot_event_study <- function(x) {
  output %>%
    filter(type == "event_study") %>%
    filter(Regression == x) %>%
    mutate(parmseq = as.numeric(term)) %>%
    ggplot(aes(x = parmseq, y = estimate, group = 1)) +
    geom_ribbon(aes(ymax = conf.high, ymin = conf.low), fill = "grey90", alpha = 0.5) +
    geom_line(aes(parmseq, conf.high), color = "grey30", size = 0.1) +
    geom_line(aes(parmseq, conf.low), color = "grey30", size = 0.1) + 
    geom_point(size = 2, color = "dodgerblue", fill = "black") +
    geom_line(color = "dodgerblue", size = 1) +
    scale_x_continuous(breaks = c(-3:9), labels = c(-3:9)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = 'Years', y = 'Coefficient', title = label[match(x, outcomes_principais)]) +
    theme_bw() + 
    theme(legend.position = "bottom")
}


# Função para plotar os gráficos
plot_event_time <- function(x){
  output %>% 
    filter(Regression == x) %>% 
    ggplot(aes(y = term, x = estimate, color = tipo)) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      size = 0.5 , position = position_dodge(width=0.5)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    #scale_y_discrete(limits = c( "2011", "2012","2013", "2014", "2015")) + 
    labs(x = 'Coefficient', y = 'Year', title = label[match(x, outcomes_principais)],
         color = "") +
    scale_color_manual(values= paletteer::paletteer_c("ggthemes::Blue", length(unique(output$tipo)) + 1)[-1]) +
    coord_flip() +
    theme_bw() + 
    theme(legend.position = "bottom")
}

