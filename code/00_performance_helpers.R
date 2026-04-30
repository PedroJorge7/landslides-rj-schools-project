performance_controls <- c(
  "income_total",
  "pop_per_household",
  "pop_branca",
  "urban",
  "favela"
)

performance_main_outcomes <- c(
  "aband_media",
  "tdi_media"
)

performance_event_outcomes <- performance_main_outcomes
performance_plot_outcomes <- performance_event_outcomes

performance_labels <- c(
  aband_media = "A. School Abandonment Rate",
  tdi_media = "B. Age-Grade Distortion Rate"
)

performance_numeric_col <- function(df, col) {
  if (col %in% names(df)) {
    suppressWarnings(as.numeric(df[[col]]))
  } else {
    rep(NA_real_, nrow(df))
  }
}

performance_mean_cols <- function(df, cols) {
  mat <- do.call(cbind, lapply(cols, function(col) performance_numeric_col(df, col)))
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

performance_fill_or_derive <- function(df, output_col, cols) {
  existing <- performance_numeric_col(df, output_col)
  derived <- performance_mean_cols(df, cols)
  df[[output_col]] <- dplyr::coalesce(existing, derived)
  df
}

performance_ensure_columns <- function(df) {
  df$had_fund <- performance_numeric_col(df, "had_fund")
  df$had_medio <- performance_numeric_col(df, "had_medio")
  df$tdi_fund <- performance_numeric_col(df, "tdi_fund")
  df$tdi_medio <- performance_numeric_col(df, "tdi_medio")

  df <- performance_fill_or_derive(df, "aprov_media", c("aprov_cat_fund", "aprov_cat_medio"))
  df <- performance_fill_or_derive(df, "reprov_media", c("reprov_cat_fund", "reprov_cat_medio"))
  df <- performance_fill_or_derive(df, "aband_media", c("aband_cat_fund", "aband_cat_medio"))
  df <- performance_fill_or_derive(df, "ideb_media", c("ideb_ai", "ideb_af", "ideb_em"))
  df <- performance_fill_or_derive(df, "portugues_media", c("ideb_pt_ai", "ideb_pt_af", "ideb_pt_em"))
  df <- performance_fill_or_derive(df, "matematica_media", c("ideb_mat_ai", "ideb_mat_af", "ideb_mat_em"))
  df <- performance_fill_or_derive(df, "had_media", c("had_fund", "had_medio"))
  df <- performance_fill_or_derive(df, "tdi_media", c("tdi_fund", "tdi_medio"))

  df
}

performance_prepare_panel <- function(limit_year = 2015, fill_controls = FALSE, add_period = FALSE) {
  df <- arrow::read_parquet("./output/painel_escolas.parquet") %>%
    dplyr::filter(ano <= limit_year) %>%
    dplyr::filter(raio == 1 | data.table::between(min_dist, 20, 30))

  if (fill_controls) {
    df <- df %>%
      dplyr::arrange(code_inep, ano) %>%
      dplyr::group_by(code_inep) %>%
      tidyr::fill(
        lat,
        lon,
        income_total,
        pop_total,
        pop_per_household,
        urban,
        favela,
        pop_water_network,
        .direction = "downup"
      ) %>%
      dplyr::ungroup()
  }

  df <- performance_ensure_columns(df)

  if (add_period) {
    df$period <- df$ano - 2011 + 1
  }

  df
}

performance_add_time_controls <- function(df) {
  df$pop_branca <- suppressWarnings(as.numeric(df$pop_branca)) * df$ano
  df$income_total <- suppressWarnings(as.numeric(df$income_total)) * df$ano
  df$pop_per_household <- suppressWarnings(as.numeric(df$pop_per_household)) * df$ano
  df$pop_total <- suppressWarnings(as.numeric(df$pop_total)) * df$ano
  df$urban <- suppressWarnings(as.numeric(df$urban)) * df$ano
  df$favela <- suppressWarnings(as.numeric(df$favela)) * df$ano
  df
}

performance_has_variation <- function(df, feature) {
  if (!feature %in% names(df)) {
    return(FALSE)
  }

  vals <- suppressWarnings(as.numeric(df[[feature]]))
  vals <- vals[!is.na(vals)]
  length(unique(vals)) > 1
}

performance_available_outcomes <- function(df, outcomes = performance_main_outcomes) {
  outcomes <- outcomes[outcomes %in% names(df)]
  outcomes[vapply(outcomes, function(feature) performance_has_variation(df, feature), logical(1))]
}

performance_labels_for <- function(outcomes, labels = performance_labels) {
  out <- unname(labels[outcomes])
  out[is.na(out)] <- outcomes[is.na(out)]
  out
}

performance_get_model_column <- function(tbl, feature, position = "last") {
  idx <- if (identical(position, "first")) 1L else ncol(tbl)
  col <- tbl[, idx, drop = FALSE]
  names(col) <- feature
  col
}

performance_build_columns <- function(df, outcomes, type, model_position = "last", anchor_feature = "fechamento") {
  row_label <- process_feature(df = df, feature = anchor_feature, type = type)[, 1, drop = FALSE]
  model_list <- lapply(outcomes, function(feature) {
    tbl <- process_feature(df = df, feature = feature, type = type)
    performance_get_model_column(tbl, feature, position = model_position)
  })

  results <- do.call(cbind, c(list(row_label), model_list))
  names(results) <- c("", paste0("(", 1:(ncol(results) - 1), ")"))
  to_df_fix_names(results)
}


performance_build_results_table <- function(df, outcomes, type, anchor_feature = "fechamento") {
  row_label <- process_feature(df = df, feature = anchor_feature, type = type)[, 1, drop = FALSE]
  model_list <- lapply(outcomes, function(feature) {
    process_feature(df = df, feature = feature, type = type)
  })

  results <- do.call(cbind, c(list(row_label), model_list))
  names(results) <- c("", paste0("(", 1:(ncol(results) - 1), ")"))
  to_df_fix_names(results)
}
performance_bind_with_blank <- function(top, bottom) {
  all_cols <- union(names(top), names(bottom))
  top <- add_missing_cols(top, all_cols)
  bottom <- add_missing_cols(bottom, all_cols)

  blank_row <- as.data.frame(
    as.list(rep("", length(all_cols))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(blank_row) <- all_cols

  out <- rbind(top, blank_row, bottom)
  rownames(out) <- NULL
  out
}

performance_latex_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  replacements <- c(
    "&" = "\\\\&",
    "%" = "\\\\%",
    "$" = "\\\\$",
    "#" = "\\\\#",
    "_" = "\\\\_",
    "{" = "\\\\{",
    "}" = "\\\\}"
  )

  for (pat in names(replacements)) {
    x <- gsub(pat, replacements[[pat]], x, fixed = TRUE)
  }

  x
}

write_latex_table <- function(df, path, caption = NULL, label = NULL, align = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  df[is.na(df)] <- ""

  header <- names(df)
  header[is.na(header)] <- ""

  if (is.null(align)) {
    align <- paste0("l", paste(rep("c", max(ncol(df) - 1, 0)), collapse = ""))
  }

  header_line <- paste(performance_latex_escape(header), collapse = " & ")
  row_lines <- apply(df, 1, function(row) {
    paste(performance_latex_escape(row), collapse = " & ")
  })

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering"
  )

  if (!is.null(caption)) {
    lines <- c(lines, paste0("\\caption{", performance_latex_escape(caption), "}"))
  }

  if (!is.null(label)) {
    lines <- c(lines, paste0("\\label{", label, "}"))
  }

  lines <- c(
    lines,
    paste0("\\begin{tabular}{", align, "}"),
    "\\hline",
    paste0(header_line, " \\\\"),
    "\\hline",
    paste0(row_lines, " \\\\"),
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(lines, path)
}

performance_strip_label_prefix <- function(x) {
  sub("^[A-Z]\\.\\s*", "", x)
}

performance_table_labels_for <- function(outcomes, labels = performance_labels_for(outcomes)) {
  performance_strip_label_prefix(labels)
}

write_grouped_latex_table <- function(panels,
                                      path,
                                      outcome_labels,
                                      caption = NULL,
                                      label = NULL,
                                      panel_titles = NULL) {
  if (!length(panels)) {
    stop("Nenhum painel foi informado para exportacao em LaTeX.")
  }

  panels <- lapply(panels, function(df) {
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    df[is.na(df)] <- ""
    df
  })

  first_panel <- panels[[1]]
  total_models <- ncol(first_panel) - 1L
  n_outcomes <- length(outcome_labels)

  if (n_outcomes == 0L) {
    stop("Nenhum outcome foi informado para exportacao em LaTeX.")
  }

  if (total_models %% n_outcomes != 0L) {
    stop("O numero de colunas-modelo nao e multiplo do numero de outcomes.")
  }

  models_per_outcome <- total_models / n_outcomes
  align <- paste0("l", paste(rep("c", total_models), collapse = ""))

  header_top <- c(
    "",
    vapply(
      performance_latex_escape(outcome_labels),
      function(lbl) paste0("\\multicolumn{", models_per_outcome, "}{c}{", lbl, "}"),
      character(1)
    )
  )

  header_bottom <- c("", performance_latex_escape(names(first_panel)[-1]))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering"
  )

  if (!is.null(caption)) {
    lines <- c(lines, paste0("\\caption{", performance_latex_escape(caption), "}"))
  }

  if (!is.null(label)) {
    lines <- c(lines, paste0("\\label{", label, "}"))
  }

  lines <- c(
    lines,
    paste0("\\begin{tabular}{", align, "}"),
    "\\hline",
    paste0(paste(header_top, collapse = " & "), " \\\\"),
    paste0(paste(header_bottom, collapse = " & "), " \\\\"),
    "\\hline"
  )

  for (i in seq_along(panels)) {
    panel <- panels[[i]][-1, , drop = FALSE]

    if (!is.null(panel_titles) && length(panel_titles) >= i) {
      lines <- c(
        lines,
        paste0(
          "\\multicolumn{", total_models + 1L, "}{l}{\\textit{",
          performance_latex_escape(panel_titles[[i]]),
          "}} \\\\"
        )
      )
    }

    panel_lines <- apply(panel, 1, function(row) {
      paste(performance_latex_escape(row), collapse = " & ")
    })

    lines <- c(lines, paste0(panel_lines, " \\\\") )

    if (i < length(panels)) {
      lines <- c(lines, "\\hline")
    }
  }

  lines <- c(
    lines,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(lines, path)
}
