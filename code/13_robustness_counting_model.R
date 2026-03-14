# =========================================================
# H3 x ano (counts) + Poisson / NegBin / ZIP
# + COMPLETA H3xANO com zeros quando não há escola no ano
# + 2 TABELAS (closure e schools) com treat_1yr..treat_5yr
# + SE abaixo do coef (linha de baixo)
# =========================================================

rm(list = ls()); gc()

library(arrow)
library(dplyr)
library(data.table)
library(sf)
library(tidyr)
library(h3jsr)
library(MASS)   # glm.nb
library(pscl)   # zeroinfl

# -------------------------
# CONFIG
# -------------------------
path_panel <- "./output/painel_escolas.parquet"
H3_RES <- 8
ANO_MAX <- 2015

controles <- c("income_total","pop_total","pop_per_household","urban","favela","pop_water_network")
tt_all    <- paste0("treat_", 1:9, "yr")
tt_keep   <- paste0("treat_", 1:5, "yr")

OUT_DIR <- "./output/tabelas_h3"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# -------------------------
# 1) LER + FILTRAR + FILL
# -------------------------
vars_needed <- unique(c(
  "code_inep","ano","fechamento","treat",tt_all,
  "lat","lon","code_muni",
  controles,
  "min_dist","raio"
))

df0 <- arrow::read_parquet(path_panel) %>%
  dplyr::select(dplyr::any_of(vars_needed)) %>%
  dplyr::filter(raio == 1 | data.table::between(min_dist, 20, 30)) %>%
  dplyr::filter(ano <= ANO_MAX) %>%
  dplyr::arrange(code_inep, ano) %>%
  dplyr::group_by(code_inep) %>%
  tidyr::fill(
    lat, lon, income_total, code_muni,
    pop_total, pop_per_household,
    urban, favela, pop_water_network,
    .direction = "downup"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(lon), !is.na(lat)) %>%
  dplyr::filter(!is.na(pop_water_network), !is.na(income_total), !is.na(pop_per_household)) %>%
  as.data.frame()

df0$ano        <- as.integer(df0$ano)
df0$fechamento <- as.integer(df0$fechamento)

# -------------------------
# 2) H3
# -------------------------
pts <- sf::st_as_sf(df0, coords = c("lon","lat"), crs = 4326, remove = FALSE)
xy  <- sf::st_coordinates(pts)

df0$h3 <- h3jsr::point_to_cell(
  data.frame(lng = xy[,1], lat = xy[,2]),
  res = H3_RES
)

df0 <- df0[!is.na(df0$h3) & !is.na(df0$ano), ]

# -------------------------
# 3) AGREGA H3 x ANO
# -------------------------
safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

agg <- df0 %>%
  dplyr::group_by(h3, ano) %>%
  dplyr::summarise(
    y_close   = sum(fechamento, na.rm = TRUE),
    n_schools = dplyr::n(),
    
    treat = as.integer(any(treat > 0, na.rm = TRUE)),
    dplyr::across(all_of(tt_all), ~ as.integer(any(.x > 0, na.rm = TRUE))),
    
    dplyr::across(any_of(controles), ~ safe_max(.x)),
    code_muni = suppressWarnings(max(code_muni, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  as.data.frame()

agg$ano <- as.integer(agg$ano)
agg$code_muni[is.infinite(agg$code_muni)] <- NA

# -------------------------
# 4) COMPLETA H3 x ANO (anos sem escola => 0)
# -------------------------
years_all <- sort(unique(agg$ano))
h3_all    <- sort(unique(agg$h3))

panel <- agg %>%
  tidyr::complete(h3 = h3_all, ano = years_all) %>%
  dplyr::arrange(h3, ano) %>%
  dplyr::group_by(h3) %>%
  tidyr::fill(code_muni, any_of(controles), .direction = "downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    y_close   = tidyr::replace_na(y_close,   0L),
    n_schools = tidyr::replace_na(n_schools, 0L),
    treat     = tidyr::replace_na(treat,     0L)
  ) %>%
  dplyr::mutate(dplyr::across(all_of(tt_all), ~ tidyr::replace_na(.x, 0L))) %>%
  as.data.frame()

# -------------------------
# 5) DROP anos com soma do outcome = 0 (só pra estimação)
# -------------------------
drop_zero_years <- function(df, y) {
  s <- tapply(df[[y]], df$ano, sum, na.rm = TRUE)
  yrs_drop <- as.integer(names(s)[s == 0])
  df[!(df$ano %in% yrs_drop), , drop = FALSE]
}

df_close  <- drop_zero_years(panel, "y_close")
df_school <- drop_zero_years(panel, "n_schools")

# -------------------------
# 6) FÓRMULAS (treat_1yr..treat_5yr)
# -------------------------
rhs <- paste(c(tt_keep, controles, "factor(ano)", "factor(code_muni)"), collapse = " + ")
f_close  <- as.formula(paste0("y_close ~ ", rhs))
f_school <- as.formula(paste0("n_schools ~ ", rhs))

# -------------------------
# 7) ESTIMAÇÕES (6)
# -------------------------
m_pois_close <- glm(f_close, data = df_close, family = poisson("log"),
                    control = glm.control(maxit = 200, epsilon = 1e-10))
m_nb_close   <- MASS::glm.nb(f_close, data = df_close,
                             control = glm.control(maxit = 500, epsilon = 1e-12))
m_zip_close  <- pscl::zeroinfl(as.formula(paste0("y_close ~ ", rhs, " | 1")),
                               data = df_close, dist = "poisson", link = "logit",
                               control = pscl::zeroinfl.control(maxit = 20000, EM = FALSE, reltol = 1e-12))

m_pois_school <- glm(f_school, data = df_school, family = poisson("log"),
                     control = glm.control(maxit = 200, epsilon = 1e-10))
m_nb_school   <- MASS::glm.nb(f_school, data = df_school,
                              control = glm.control(maxit = 500, epsilon = 1e-12))
m_zip_school  <- pscl::zeroinfl(as.formula(paste0("n_schools ~ ", rhs, " | 1")),
                                data = df_school, dist = "poisson", link = "logit",
                                control = pscl::zeroinfl.control(maxit = 20000, EM = FALSE, reltol = 1e-12))

# -------------------------
# 8) TABELAS (SE abaixo) — termos treat_1yr..treat_5yr
# -------------------------
stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*",
                              ifelse(p < 0.10,  ".", "")))))
}

extract_one <- function(m, term) {
  if (inherits(m, "zeroinfl")) {
    cc <- summary(m)$coefficients$count
    if (!(term %in% rownames(cc))) return(list(est=NA_real_, se=NA_real_, p=NA_real_))
    pcol <- if ("Pr(>|z|)" %in% colnames(cc)) "Pr(>|z|)" else NA_character_
    list(
      est = unname(cc[term, "Estimate"]),
      se  = unname(cc[term, "Std. Error"]),
      p   = if (!is.na(pcol)) unname(cc[term, pcol]) else NA_real_
    )
  } else {
    cm <- summary(m)$coefficients
    if (!(term %in% rownames(cm))) return(list(est=NA_real_, se=NA_real_, p=NA_real_))
    list(
      est = unname(cm[term, 1]),
      se  = unname(cm[term, 2]),
      p   = if (ncol(cm) >= 4) unname(cm[term, 4]) else NA_real_
    )
  }
}

fmt_est <- function(x) if (is.na(x$est)) NA_character_ else paste0(sprintf("%.4f", x$est), stars(x$p))
fmt_se  <- function(x) if (is.na(x$se))  NA_character_ else paste0("(", sprintf("%.4f", x$se), ")")

get_nobs <- function(m) if (!is.null(m$y)) length(m$y) else NA_integer_

make_table_terms <- function(m_pois, m_nb, m_zip, terms = tt_keep) {
  term_col <- c(as.vector(rbind(terms, rep("", length(terms)))), "N")
  
  build_col <- function(m) {
    vals <- unlist(lapply(terms, function(t) {
      x <- extract_one(m, t)
      c(fmt_est(x), fmt_se(x))
    }))
    c(vals, as.character(get_nobs(m)))
  }
  
  data.frame(
    term    = term_col,
    Poisson = build_col(m_pois),
    NegBin  = build_col(m_nb),
    ZIP     = build_col(m_zip),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

tab_close  <- make_table_terms(m_pois_close,  m_nb_close,  m_zip_close,  terms = tt_keep)
tab_school <- make_table_terms(m_pois_school, m_nb_school, m_zip_school, terms = tt_keep)

xlsx::write.xlsx(tab_close,  file.path("./results/counting_time_treatment.xlsx"), row.names = FALSE)
