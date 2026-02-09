library(arrow)
library(dplyr)
library(data.table)
library(fixest)
library(HonestDiD)
library(ggplot2)
library(patchwork)

rm(list = ls())
source("./code/00_functions.R")

outcomes_principais <- c("fechamento","log_docente","log_salas","log_num_funcionarios")

label <- c(
  "A. School Closure (0/1)",
  "B. Log of Number of Teachers",
  "C. Log of Number of Class",
  "D. Log of Number of Staff"
)
names(label) <- outcomes_principais

df <- arrow::read_parquet("./output/painel_escolas.parquet") %>%
  filter(ano <= 2015) %>%
  filter(raio == 1 | data.table::between(min_dist, 20, 30))

df$pop_branca         <- df$pop_branca * df$ano
df$income_total       <- df$income_total * df$ano
df$pop_per_household  <- df$pop_per_household * df$ano
df$pop_total          <- df$pop_total * df$ano
df$urban              <- df$urban * df$ano
df$favela             <- df$favela * df$ano

controles <- c("income_total","pop_per_household","pop_branca","urban","favela")

base_line <- 2010
# se quiser igual ao exemplo da imagem:
Mbarvec <- seq(0, 1, by = 0.1)
# Mbarvec <- seq(0.5, 1, by = 0.25)

dir.create("./results", showWarnings = FALSE)

style_honest <- function(p, ttl) {
  for (i in seq_along(p$layers)) {
    g <- p$layers[[i]]$geom
    if (inherits(g, "GeomErrorbar") ||
        inherits(g, "GeomLinerange") ||
        inherits(g, "GeomSegment") ||
        inherits(g, "GeomPointrange") ||
        inherits(g, "GeomPoint")) {
      p$layers[[i]]$aes_params$colour <- "red"
    }
  }
  
  p +
    labs(title = ttl, x = "M", y = "Coefficient") +
    guides(color = "none", fill = "none") +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "black") +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0, face = "plain", size = 18),
      panel.grid.major = element_line(color = "grey85"),
      panel.grid.minor = element_line(color = "grey93"),
      panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.6)
    )
}

plot_list <- vector("list", length(outcomes_principais))
names(plot_list) <- outcomes_principais

honest_list <- vector("list", length(outcomes_principais))
names(honest_list) <- outcomes_principais

for (feature in outcomes_principais) {
  
  fml <- as.formula(paste0(
    feature, " ~ i(ano, treat_unid, ref = ", base_line, ") + ",
    paste(controles, collapse = " + "),
    " | code_inep + ano"
  ))
  
  m <- fixest::feols(fml, data = df, cluster = "code_inep")
  
  b <- coef(m)
  V <- vcov(m)
  
  pat <- "^ano::([0-9]+):treat_unid$"
  idx <- grepl(pat, names(b))
  
  tt <- as.integer(sub(pat, "\\1", names(b)[idx]))
  k  <- tt - base_line
  
  ord <- order(k)
  betahat <- as.numeric(b[idx][ord])
  sigma   <- V[idx, idx][ord, ord]
  
  numPrePeriods  <- sum(k[ord] < 0)
  numPostPeriods <- sum(k[ord] > 0)
  
  orig <- HonestDiD::constructOriginalCS(
    betahat = betahat,
    sigma = sigma,
    numPrePeriods = numPrePeriods,
    numPostPeriods = numPostPeriods
  )
  
  sens <- HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat,
    sigma = sigma,
    numPrePeriods = numPrePeriods,
    numPostPeriods = numPostPeriods,
    Mbarvec = Mbarvec
  )
  
  p_raw <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sens, orig)
  p <- style_honest(p_raw, label[[feature]])
  
  plot_list[[feature]] <- p
  honest_list[[feature]] <- list(model = m, orig = orig, sens = sens)
  

}

fig_final <- wrap_plots(plot_list, ncol = 2)

ggsave(
  filename = "./results/honestdid.jpg",
  plot = fig_final,
  width = 32, height = 18, units = "cm", dpi = 300
)
