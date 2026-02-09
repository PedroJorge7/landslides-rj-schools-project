library(geobr)
library(sf)
library(dplyr)
library(stringi)

# ------------------------------------------------------------
# Helper: normalizar strings (tirar acento, caixa alta, etc.)
# ------------------------------------------------------------
norm_nm <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    gsub("[^A-Z0-9 ]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}

# ------------------------------------------------------------
# 1) Municípios (polígonos)
# ------------------------------------------------------------
year_ref <- 2020
muni <- geobr::read_municipality(year = year_ref, simplified = TRUE) %>%
  mutate(name_muni_norm = norm_nm(name_muni))

# ------------------------------------------------------------
# 2) Tabela UF -> Capital (nome)
#    (aqui é o único "manual": o resto é join automático)
# ------------------------------------------------------------
capitais <- tibble::tribble(
  ~abbrev_state, ~capital,
  "AC","Rio Branco",
  "AL","Maceio",
  "AP","Macapa",
  "AM","Manaus",
  "BA","Salvador",
  "CE","Fortaleza",
  "DF","Brasilia",
  "ES","Vitoria",
  "GO","Goiania",
  "MA","Sao Luis",
  "MT","Cuiaba",
  "MS","Campo Grande",
  "MG","Belo Horizonte",
  "PA","Belem",
  "PB","Joao Pessoa",
  "PR","Curitiba",
  "PE","Recife",
  "PI","Teresina",
  "RJ","Rio de Janeiro",
  "RN","Natal",
  "RS","Porto Alegre",
  "RO","Porto Velho",
  "RR","Boa Vista",
  "SC","Florianopolis",
  "SP","Sao Paulo",
  "SE","Aracaju",
  "TO","Palmas"
) %>%
  mutate(capital_norm = norm_nm(capital))

# ------------------------------------------------------------
# 3) Identificar o município-capital dentro do geobr
# ------------------------------------------------------------
cap_muni <- capitais %>%
  left_join(
    muni %>% st_drop_geometry() %>% select(abbrev_state, code_muni, name_muni_norm),
    by = c("abbrev_state", "capital_norm" = "name_muni_norm")
  )

# checagem: alguma capital não casou?
if (any(is.na(cap_muni$code_muni))) {
  print(cap_muni %>% filter(is.na(code_muni)))
  stop("Alguma capital não foi encontrada nos municípios do geobr. Veja o print acima (ajuste o nome).")
}

cap_sf <- muni %>%
  semi_join(cap_muni, by = "code_muni") %>%
  left_join(cap_muni %>% select(abbrev_state, code_muni)) %>%
  select(abbrev_state, code_muni_capital = code_muni, geom)

# ------------------------------------------------------------
# 4) Centroide município e capital, em CRS métrico (metros)
# ------------------------------------------------------------
CRS_DIST <- 5880  # SIRGAS 2000 / Brazil Polyconic (metros). Bom p/ distâncias no Brasil.

muni_pt <- muni %>%
  st_transform(CRS_DIST) %>%
  mutate(geom_muni_pt = st_point_on_surface(geom)) %>%
  st_drop_geometry()

cap_pt <- cap_sf %>%
  st_transform(CRS_DIST) %>%
  mutate(geom_cap_pt = st_point_on_surface(geom)) %>%
  st_drop_geometry()

# ------------------------------------------------------------
# 5) Juntar capital por UF e calcular distância (by_element = TRUE agora OK)
# ------------------------------------------------------------
base_dist <- muni_pt %>%
  left_join(cap_pt %>% select(abbrev_state, code_muni_capital, geom_cap_pt), by = "abbrev_state") %>%
  mutate(
    dist_m = as.numeric(st_distance(geom_muni_pt, geom_cap_pt, by_element = TRUE)),
    dist_km = dist_m / 1000
  ) %>%
  select(code_muni, name_muni, abbrev_state, code_state,
         code_muni_capital, dist_m, dist_km)

# Resultado final (data.frame/tibble)
arrow::write_parquet(base_dist,'dist_capital.parquet')
