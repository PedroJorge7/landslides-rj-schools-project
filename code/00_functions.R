process_feature <- function(df, feature, type, cluster_var = "code_inep") {
  if (!type %in% c("mean", "time_effect", "event_study")) {
    stop("Tipo invalido. Escolha entre 'mean', 'time_effect', ou 'event_study'.")
  }
  
  if (length(unique(df[[feature]])) <= 1) {
    warning(paste("A variavel", feature, "nao tem variacao suficiente."))
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
    stop("Tipo invalido. Escolha entre 'time_effect' ou 'event_study'.")
  }
  
  if (length(unique(df[[feature]])) <= 1 || sd(df[[feature]], na.rm = TRUE) < 1e-6) {
    if (verbose) warning(paste("A variavel", feature, "nao tem variacao suficiente."))
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
  plot_data <- output %>%
    filter(type == "event_study") %>%
    filter(Regression == x) %>%
    mutate(parmseq = as.numeric(term))

  ggplot(plot_data, aes(x = parmseq, y = estimate)) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.12,
      linewidth = 0.55,
      color = "#2E5E7E"
    ) +
    geom_point(size = 2.8, color = "dodgerblue4") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
    scale_x_continuous(breaks = sort(unique(plot_data$parmseq))) +
    labs(
      x = "Years of exposure",
      y = "Coefficient",
      title = label[match(x, outcomes_principais)]
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.0),
      panel.grid.major.y = element_line(color = "grey88"),
      panel.grid.minor = element_blank()
    )
}


# Funcao para plotar os graficos
event_time_palette <- function(n) {
  base_colors <- c("#0F4C5C", "#E36414", "#6A994E", "#BC4749", "#7A3E9D", "#3A7CA5")
  if (n <= 0) {
    return(character(0))
  }
  if (n <= length(base_colors)) {
    return(base_colors[seq_len(n)])
  }
  grDevices::colorRampPalette(base_colors)(n)
}


event_time_shapes <- function(n) {
  base_shapes <- c(21, 24, 22, 23, 25)
  rep(base_shapes, length.out = n)
}


event_time_title <- function(x) {
  if (exists("label", inherits = TRUE) && exists("outcomes_principais", inherits = TRUE)) {
    plot_labels <- get("label", inherits = TRUE)
    plot_outcomes <- get("outcomes_principais", inherits = TRUE)
    plot_title <- plot_labels[match(x, plot_outcomes)]
    if (length(plot_title) == 1 && !is.na(plot_title) && nzchar(plot_title)) {
      return(plot_title)
    }
  }
  x
}


event_time_regressions <- function(data) {
  regressions <- unique(data$Regression)
  if (exists("outcomes_principais", inherits = TRUE)) {
    ordered <- get("outcomes_principais", inherits = TRUE)
    regressions <- c(intersect(ordered, regressions), setdiff(regressions, ordered))
  }
  regressions
}


plot_event_time <- function(x, data = output, style = getOption("plot_event_time_style", "legacy")){
  plot_data <- data %>%
    filter(Regression == x) %>%
    filter(!is.na(term), !is.na(estimate), !is.na(conf.low), !is.na(conf.high)) %>%
    mutate(tipo = factor(tipo, levels = unique(tipo)))

  if (!nrow(plot_data)) {
    stop(paste("No valid plot data found for regression", x))
  }

  palette <- paletteer::paletteer_c("ggthemes::Blue", length(levels(plot_data$tipo)) + 1)[-1]
  names(palette) <- levels(plot_data$tipo)
  plot_title <- event_time_title(x)

  if (style == "connected") {
    style <- "legacy"
  }

  if (style == "legacy") {
    legacy_dodge <- position_dodge(width = 0.5)

    return(
      plot_data %>%
        mutate(term = factor(term, levels = sort(unique(term)))) %>%
        ggplot(aes(x = term, y = estimate, color = tipo, group = tipo)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.5) +        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high),
          width = 0.18,
          linewidth = 0.55,
          position = legacy_dodge
        ) +
        geom_point(
          size = 2,
          position = legacy_dodge
        ) +
        labs(x = "Year", y = "Coefficient", title = plot_title, color = "") +
        scale_color_manual(values = palette) +
        theme_bw() +
        theme(legend.position = "bottom")
    )
  }

  if (style == "line_ribbon") {
    ribbon_dodge <- position_dodge(width = 0.35)

    return(
      ggplot(plot_data, aes(x = term, y = estimate, color = tipo, group = tipo)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.5) +        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high),
          width = 0.16,
          linewidth = 0.55,
          position = ribbon_dodge
        ) +
        geom_point(size = 2.2, position = ribbon_dodge) +
        scale_x_continuous(breaks = sort(unique(plot_data$term))) +
        scale_color_manual(values = palette) +
        labs(x = "Year", y = "Coefficient", title = plot_title, color = "") +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey90"),
          plot.title = element_text(face = "bold")
        )
    )
  }

  if (style == "lollipop") {
    dodge_width <- if (length(levels(plot_data$tipo)) > 3) 0.7 else 0.55

    return(
      ggplot(plot_data, aes(x = factor(term), y = estimate, color = tipo)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.5) +
        geom_segment(
          aes(xend = factor(term), y = 0, yend = estimate),
          position = position_dodge(width = dodge_width),
          linewidth = 0.55, alpha = 0.55
        ) +
        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high),
          width = 0.16,
          position = position_dodge(width = dodge_width),
          linewidth = 0.75
        ) +
        geom_point(
          position = position_dodge(width = dodge_width),
          size = 2.4
        ) +
        scale_color_manual(values = palette) +
        labs(x = "Year", y = "Coefficient", title = plot_title, color = "") +
        theme_light(base_size = 11) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold")
        )
    )
  }

  stop(paste("Unknown plot style:", style))
}


plot_event_time_grid <- function(data = output,
                                 style = getOption("plot_event_time_style", "legacy"),
                                 nrow = 2,
                                 ncol = 2,
                                 legend = NULL) {
  regressions <- event_time_regressions(data)
  plot_list <- lapply(regressions, function(reg) {
    plot_event_time(reg, data = data, style = style)
  })

  if (is.null(legend)) {
    legend <- if (style == "line_ribbon") "top" else "bottom"
  }

  ggpubr::ggarrange(
    plotlist = plot_list,
    nrow = nrow,
    ncol = ncol,
    common.legend = TRUE,
    legend = legend
  )
}
