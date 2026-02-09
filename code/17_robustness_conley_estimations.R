# =========================================================
# robust_time_effect_conley.R
# - MESMO estilo do teu gráfico (plot_event_time + ggarrange 2x2 + legend bottom)
# - Sample FIXO: raio == 1 OU min_dist 20-30
# - TWFE: | code_inep + ano
# - RHS: treat_1yr..treat_9yr + controles
# - VCOV: Conley usando lat/lon (4 cutoffs "chutados")
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

# 4 cutoffs (km) chutados
cutoffs_km <- c(2.5, 5, 10, 20)

# -------------------------
# Load + sample FIXO
# -------------------------
df <- arrow::read_parquet("./output/painel_escolas.parquet") %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep) %>%
  tidyr::fill(lat,lon, .direction = 'downup') |> 
  filter(ano <= 2015) %>%
  ungroup() %>%
  filter(raio == 1 | data.table::between(min_dist, 20, 30)) %>%
  filter(!is.na(lat), !is.na(lon))  # Conley precisa disso

# Controls (teu padrão: * ano)
df$pop_branca        <- df$pop_branca * df$ano
df$income_total      <- df$income_total * df$ano
df$pop_per_household <- df$pop_per_household * df$ano
df$pop_total         <- df$pop_total * df$ano
df$urban             <- df$urban * df$ano
df$favela            <- df$favela * df$ano

controles <- c("income_total","pop_per_household","pop_branca","urban","favela")
tt <- paste0("treat_", 1:9, "yr")

# -------------------------
# Fit: time_effect + Conley vcov
# (sempre retorna termo 1..9)
# -------------------------
fit_time_effect_conley <- function(feature, cutoff_km) {
  
  tipo <- paste0("Conley ", cutoff_km, "km")
  
  # mesma checagem que tu usa
  if (length(unique(df[[feature]])) <= 1 || sd(df[[feature]], na.rm = TRUE) < 1e-6) {
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
  
  est <- tryCatch(fixest::feols(fml, data = df), error = function(e) NULL)
  
  out0 <- tibble(
    term = 1:9, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
    type = "time_effect", Regression = feature,
    conf.low = NA_real_, conf.high = NA_real_, nobs = if (is.null(est)) NA_real_ else est$nobs,
    tipo = tipo
  )
  
  if (is.null(est)) return(out0)
  
  # Conley vcov
  vc <- as.formula(paste0("conley(", cutoff_km, ") ~ lat + lon"))
  
  ct <- tryCatch(fixest::coeftable(est, vcov = vc), error = function(e) NULL)
  if (is.null(ct)) return(out0)
  
  rn <- rownames(ct)
  keep <- rn %in% tt
  if (!any(keep)) return(out0)
  
  b  <- ct[keep, 1]
  se <- ct[keep, 2]
  st <- ct[keep, 3]
  pv <- ct[keep, ncol(ct)]
  
  # coloca exatamente na ordem treat_1yr..treat_9yr
  ord <- match(tt, rn[keep])
  b  <- b[ord]; se <- se[ord]; st <- st[ord]; pv <- pv[ord]
  
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
# OUTPUT (igual teu formato pro plot_event_time)
# -------------------------
output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(lapply(cutoffs_km, function(km) fit_time_effect_conley(feature, km)))
}))

# teu ajuste de ano (igualzinho)
output <- subset(output, !is.na(estimate))
output$term <- output$term + 2010
output$term <- as.character(output$term)

output$tipo <- factor(output$tipo, levels = c('Conley 2.5km',
                                              'Conley 5km',
                                              'Conley 10km',
                                              'Conley 20km'))

# FIGURA IGUAL
lapply(unique(output$Regression), plot_event_time) %>%
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")

ggsave(filename = "./results/robust_conley_time_effect.jpg",
       dpi = 300, width = 30, height = 15, units = "cm")
