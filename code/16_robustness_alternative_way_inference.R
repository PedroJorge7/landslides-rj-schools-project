# =========================================================
# robust_time_effect_clusters.R
# - FIGURA IGUAL ao teu padrão: plot_event_time + ggarrange 2x2 + legend bottom
# - Sample FIXO: (raio == 1) OU (min_dist 20-30)
# - TWFE: | code_inep + ano
# - RHS: treat_1yr..treat_9yr + controles
# - Só muda o VCOV (clusters): two-way, CEP, município, UF
# =========================================================

library(arrow)
library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(ggpubr)
library(writexl)

rm(list = ls())

source("./code/00_functions.R")

outcomes_principais <- c("fechamento","log_docente","log_salas","log_num_funcionarios")

label <- c(
  "A. School Closure (0/1)",
  "B. Log of Number of Teachers",
  "C. Log of Number of Class",
  "D. Log of Number of Staff"
)

# -------------------------
# Load + sample FIXO
# -------------------------
df_robustez <- arrow::read_parquet("./output/painel_escolas.parquet") %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep) %>%
  filter(ano <= 2015) %>%
  ungroup() %>%
  filter(raio == 1 | data.table::between(min_dist, 20, 30))

# Controls (teu padrão: * ano)
df_robustez$pop_branca        <- df_robustez$pop_branca * df_robustez$ano
df_robustez$income_total      <- df_robustez$income_total * df_robustez$ano
df_robustez$pop_per_household <- df_robustez$pop_per_household * df_robustez$ano
df_robustez$pop_total         <- df_robustez$pop_total * df_robustez$ano
df_robustez$urban             <- df_robustez$urban * df_robustez$ano
df_robustez$favela            <- df_robustez$favela * df_robustez$ano

controles <- c("income_total","pop_per_household","pop_branca","urban","favela")
tt <- paste0("treat_", 1:9, "yr")

# sanitiza clusters (sem NA)
df_robustez <- df_robustez %>%
  mutate(
    cep_cl  = if ("cep" %in% names(.)) as.character(cep) else NA_character_,
    cep_cl  = if_else(is.na(cep_cl) | cep_cl == "", "MISSING", cep_cl),
    
    muni_cl = if ("code_muni" %in% names(.)) as.character(code_muni) else NA_character_,
    muni_cl = if_else(is.na(muni_cl) | muni_cl == "", "MISSING", muni_cl),
    
    uf_cl   = if ("code_muni" %in% names(.)) substr(as.character(code_muni), 1, 2) else NA_character_,
    uf_cl   = if_else(is.na(uf_cl) | uf_cl == "", "MISSING", uf_cl)
  )

# VCOV specs (nome vira "tipo" no gráfico)
vcov_specs <- list(
  "Two-way\n(School x Year)" = ~ code_inep + ano,
  "Adress"                   = ~ cep_cl,
  "City"                     = ~ muni_cl,
  "State"                    = ~ uf_cl
)

# ---- helper: sempre retorna 1..9 mesmo se quebrar
fit_time_effect <- function(feature, vcov_fml, tipo) {
  
  # mesma checagem que você usa
  if (length(unique(df_robustez[[feature]])) <= 1 || sd(df_robustez[[feature]], na.rm = TRUE) < 1e-6) {
    return(tibble(
      term = 1:9, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
      type = "time_effect", Regression = feature,
      conf.low = NA_real_, conf.high = NA_real_, nobs = NA_real_, tipo = tipo
    ))
  }
  
  fml <- as.formula(paste(
    feature, "~",
    paste(c(tt, controles), collapse = " + "),
    "| code_inep + ano"
  ))
  
  est <- tryCatch(
    fixest::feols(fml, data = df_robustez),
    error = function(e) NULL
  )
  
  if (is.null(est)) {
    return(tibble(
      term = 1:9, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
      type = "time_effect", Regression = feature,
      conf.low = NA_real_, conf.high = NA_real_, nobs = NA_real_, tipo = tipo
    ))
  }
  
  ct <- tryCatch(
    fixest::coeftable(est, vcov = vcov_fml),
    error = function(e) NULL
  )
  
  out0 <- tibble(
    term = 1:9, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
    type = "time_effect", Regression = feature,
    conf.low = NA_real_, conf.high = NA_real_, nobs = est$nobs, tipo = tipo
  )
  
  if (is.null(ct)) return(out0)
  
  rn <- rownames(ct)
  keep <- rn %in% tt
  if (!any(keep)) return(out0)
  
  b  <- ct[keep, 1]
  se <- ct[keep, 2]
  pv <- ct[keep, ncol(ct)]
  st <- ct[keep, 3]
  
  # coloca na ordem treat_1yr..treat_9yr
  ord <- match(tt, rn[keep])
  b  <- b[ord]; se <- se[ord]; pv <- pv[ord]; st <- st[ord]
  
  z_90 <- 1.645
  
  tibble(
    term = 1:9,
    estimate = as.numeric(b),
    std.error = as.numeric(se),
    statistic = as.numeric(st),
    p.value = as.numeric(pv),
    type = "time_effect",
    Regression = feature,
    conf.low = as.numeric(b) - z_90 * as.numeric(se),
    conf.high = as.numeric(b) + z_90 * as.numeric(se),
    nobs = est$nobs,
    tipo = tipo
  )
}

# -------------------------
# OUTPUT no formato do teu plot_event_time
# -------------------------
output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(lapply(names(vcov_specs), function(nm) {
    fit_time_effect(feature, vcov_specs[[nm]], nm)
  }))
}))

# teu ajuste de ano (fica igual)
output$term <- output$term + 2010

output <- subset(output, !is.na(estimate))
output$term <- as.character(output$term)
output <- subset(output, tipo != "State")

# FIGURA IGUAL
lapply(unique(output$Regression), plot_event_time) %>%
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")

ggsave(filename = "./results/robust_clusters.jpg",
       dpi = 300, width = 30, height = 15, units = "cm")
