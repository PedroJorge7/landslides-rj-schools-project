# =========================================================
# H3 x ano (counts) + Poisson / NegBin / ZIP (MANUAL, LIMPO)
# =========================================================

rm(list = ls()); gc()

library(arrow)
library(dplyr)
library(data.table)
library(sf)
library(h3jsr)
library(MASS)   # glm.nb
library(pscl)   # zeroinfl

# -------------------------
# CONFIG
# -------------------------
path_panel <- "./output/painel_escolas.parquet"
H3_RES <- 8

controles <- c("income_total","pop_total","pop_per_household","urban","favela","pop_water_network")
tt <- paste0("treat_", 1:9, "yr")

# -------------------------
# 1) LER + FILTRAR + EVENTO FECHAMENTO + RISK SET
# -------------------------
df0 <- arrow::read_parquet(path_panel) %>%
  filter(raio == 1 | data.table::between(min_dist, 20, 30)) %>%
  filter(ano <= 2015) %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep) |> 
  tidyr::fill(lat,lon,income_total,code_muni,
              pop_total,pop_per_household,
              urban,favela,pop_water_network, .direction = 'downup') |> 
  as.data.frame() |> 
  filter(!is.na(lon)) |> 
  filter(!is.na(pop_water_network)) |> 
  filter(!is.na(income_total)) |> 
  filter(!is.na(pop_per_household))


# -------------------------
# 3) H3
# -------------------------
pts <- sf::st_as_sf(df0, coords = c("lon","lat"), crs = 4326, remove = FALSE)
xy  <- sf::st_coordinates(pts)

df0$h3 <- h3jsr::point_to_cell(
  data.frame(lng = xy[,1], lat = xy[,2]),
  res = H3_RES
)

df0 <- df0[!is.na(df0$h3) & !is.na(df0$ano), ]

# -------------------------
# 4) AGREGA H3 x ANO (CONTAGENS + INDICADORES)
# -------------------------
agg <- df0 %>%
  group_by(h3, ano) %>%
  summarise(
    y_close   = sum(fechamento , na.rm = TRUE),
    n_schools = n(),
    
    treat = as.integer(any(treat > 0, na.rm = TRUE)),
    across(all_of(tt), ~ as.integer(any(.x > 0, na.rm = TRUE))),
    
    across(any_of(controles), ~ max(.x, na.rm = TRUE)),
    
    code_muni = max(code_muni, na.rm = T),
    .groups = "drop"
  ) %>%
  as.data.frame()

agg$ano <- as.integer(agg$ano)
summary(agg$y_close)

# -------------------------
# 5) CRIA df_att E df_tt ANTES (pra não “sumir” se der erro)
# -------------------------
df_att <- agg
df_tt  <- agg

# remove anos com total de y_close == 0 (só para estimação)
sum_by_year_att <- tapply(df_att$y_close, df_att$ano, sum, na.rm = TRUE)
years_drop_att  <- as.integer(names(sum_by_year_att)[sum_by_year_att == 0])
df_att <- df_att[!(df_att$ano %in% years_drop_att), ]

sum_by_year_tt <- tapply(df_tt$y_close, df_tt$ano, sum, na.rm = TRUE)
years_drop_tt  <- as.integer(names(sum_by_year_tt)[sum_by_year_tt == 0])
df_tt <- df_tt[!(df_tt$ano %in% years_drop_tt), ]


#
# --------------
# 8) ESTIMAÇÕES 
# --------------
m_pois_closure <- glm(
  as.formula(paste("y_close ~", paste(c("treat", controles, year_term_att, "factor(code_muni)"), collapse = " + "))),
  data = df_tt,
    family = poisson("log"),
    control = glm.control(maxit = 200, epsilon = 1e-10)
  )

m_pois_school <- glm(
  as.formula(paste("y_close ~", paste(c("treat", controles, year_term_att, "factor(code_muni)"), collapse = " + "))),
  data = df_tt,
  family = poisson("log"),
  control = glm.control(maxit = 200, epsilon = 1e-10)
)

m_nb_closure <- MASS::glm.nb(
  as.formula(paste("n_schools ~", paste(c("treat", controles, year_term_att, "factor(code_muni)"), collapse = " + "))),
  data = df_tt,
    control = glm.control(maxit = 500, epsilon = 1e-12)
  )

m_nb_school <- MASS::glm.nb(
  as.formula(paste("n_schools ~", paste(c("treat", controles, year_term_att, "factor(code_muni)"), collapse = " + "))),
  data = df_tt,
  control = glm.control(maxit = 500, epsilon = 1e-12)
)



m_zip_closure <- pscl::zeroinfl(
  as.formula(paste("y_close ~", paste(c("treat", controles, year_term_att, "factor(code_muni)"), collapse = " + "), "| 1")),
  data = df_tt,
    dist = "poisson", link = "logit",
    control = pscl::zeroinfl.control(maxit = 20000, EM = FALSE, reltol = 1e-12)
  )

m_zip_school <- pscl::zeroinfl(
  as.formula(paste("n_schools ~", paste(c("treat", controles, year_term_att, "factor(code_muni)"), collapse = " + "), "| 1")),
  data = df_tt,
  dist = "poisson", link = "logit",
  control = pscl::zeroinfl.control(maxit = 20000, EM = FALSE, reltol = 1e-12)
)
