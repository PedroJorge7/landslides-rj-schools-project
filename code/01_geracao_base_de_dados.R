# ============================================================
# JOIN BASES (ESCOLAS) + FECHAMENTO + GEO RUNNOUT (SEM IFS)
# + Fallback GEO via geobr::read_schools (lat/lon fixo por escola)
# + Painel H3 (res 10) com outcomes + min_dist + treat/treat_yr
# ============================================================

library(dplyr)
library(arrow)
library(sf)
library(data.table)
library(nngeo)
library(tidyr)
library(geobr)

# H3
# install.packages("h3jsr")
library(h3jsr)

# ----------------------------
# 0) Paths
# ----------------------------
DIR_PARQUET <- "input/"      # << ajuste
DIR_BASE    <- "./input/"    # << ajuste (cicatrizes/runnout + csv census)
DIR_OUT     <- "./output/"   # << ajuste

# ----------------------------
# 1) Ler parquets (em R; filtra RJ por prefixo "33")
# ----------------------------
censo <- arrow::read_parquet(file.path(DIR_PARQUET, "censo_escolar_geolocalizado.parquet")) |>
  filter(substr(as.character(co_entidade), 1, 2) == "33")

atu <- arrow::read_parquet(file.path(DIR_PARQUET, "atu_gold_escolas.parquet")) |>
  filter(substr(as.character(code_inep), 1, 2) == "33")

dsi <- arrow::read_parquet(file.path(DIR_PARQUET, "dsi_gold_escolas.parquet")) |>
  filter(substr(as.character(code_inep), 1, 2) == "33")

dsu <- arrow::read_parquet(file.path(DIR_PARQUET, "dsu_gold_escolas.parquet")) |>
  filter(substr(as.character(code_inep), 1, 2) == "33")

had <- arrow::read_parquet(file.path(DIR_PARQUET, "had_gold_escolas.parquet")) |>
  filter(substr(as.character(code_school), 1, 2) == "33")

txr <- arrow::read_parquet(file.path(DIR_PARQUET, "tx_rendimento_gold_escolas.parquet")) |>
  filter(substr(as.character(code_inep), 1, 2) == "33")

# ----------------------------
# 1.1) IDEB — renomeia + cria nota_fund + remove code_muni
# ----------------------------
ideb <- arrow::read_parquet(file.path(DIR_PARQUET, "ideb_notas_escola_ano.parquet")) |>
  filter(substr(as.character(code_inep), 1, 2) == "33")

stopifnot(all(c("ano","code_inep") %in% names(ideb)))

ideb <- ideb |>
  mutate(
    code_inep = as.character(code_inep),
    ano       = as.integer(ano)
  ) |>
  select(-code_muni) |>
  rename(
    ideb_mat_ai   = vl_nota_matematica_anos_iniciais,
    ideb_mat_af   = vl_nota_matematica_anos_finais,
    ideb_mat_em   = vl_nota_matematica_ensino_medio,
    ideb_pt_ai    = vl_nota_portugues_anos_iniciais,
    ideb_pt_af    = vl_nota_portugues_anos_finais,
    ideb_pt_em    = vl_nota_portugues_ensino_medio,
    ideb_ai       = vl_nota_media_anos_iniciais,
    ideb_af       = vl_nota_media_anos_finais,
    ideb_em       = vl_nota_media_ensino_medio
  ) |>
  mutate(
    ideb_fund = rowMeans(cbind(as.numeric(ideb_ai), as.numeric(ideb_af)), na.rm = TRUE),
    ideb_fund = if_else(is.nan(ideb_fund), NA_real_, ideb_fund)
  )

# ----------------------------
# 2) Padronizar chaves
# ----------------------------
stopifnot(all(c("ano","co_entidade","lat","lon","dep_cat","loc_cat") %in% names(censo)))
stopifnot(all(c("ano","code_inep") %in% names(dsi)))
stopifnot(all(c("ano","code_inep") %in% names(dsu)))
stopifnot(all(c("ano","code_inep") %in% names(txr)))
stopifnot(all(c("ano","code_inep") %in% names(atu)))
stopifnot(all(c("ano","code_school") %in% names(had)))

censo <- censo |>
  rename(code_inep = co_entidade) |>
  mutate(code_inep = as.character(code_inep),
         ano       = as.integer(ano))

atu  <- atu |> mutate(code_inep = as.character(code_inep), ano = as.integer(ano))
dsi  <- dsi |> mutate(code_inep = as.character(code_inep), ano = as.integer(ano))
dsu  <- dsu |> mutate(code_inep = as.character(code_inep), ano = as.integer(ano))
txr  <- txr |> mutate(code_inep = as.character(code_inep), ano = as.integer(ano))

had <- had |>
  rename(code_inep = code_school) |>
  mutate(code_inep = as.character(code_inep),
         ano       = as.integer(ano))

# ----------------------------
# 3) Reduzir colunas das bases gold
# ----------------------------
atu <- atu |> select(ano, code_inep, atu_unificada, atu_fund, atu_medio)
dsi <- dsi |> select(ano, code_inep, tdi_fund, tdi_medio)
dsu <- dsu |> select(ano, code_inep, dsu_fund, dsu_medio)
had <- had |> select(ano, code_inep, had_fund, had_medio)

txr <- txr |>
  select(
    ano, code_inep,
    aprov_cat_fund, aprov_cat_medio,
    reprov_cat_fund, reprov_cat_medio,
    aband_cat_fund, aband_cat_medio
  )

# ----------------------------
# 4) Join final (censo manda) + IDEB
# ----------------------------
dados_escolas <- censo |>
  left_join(atu,  by = c("ano","code_inep")) |>
  left_join(dsi,  by = c("ano","code_inep")) |>
  left_join(dsu,  by = c("ano","code_inep")) |>
  left_join(had,  by = c("ano","code_inep")) |>
  left_join(txr,  by = c("ano","code_inep")) |>
  left_join(ideb, by = c("ano","code_inep"))

# ============================================================
# 4.1) GEO FIX (1 par lat/lon por escola) + fallback geobr::read_schools
# - prioridade:
#   (1) geocodebr "bom" (geo_ok) com menor desvio_metros
#   (2) qualquer lat/lon existente na base
#   (3) geobr::read_schools (RJ)
# ============================================================

GOOD_PREC <- c("logradouro", "numero", "numero_aproximado")
GOOD_TIPO <- c("dc01")

# Geobr schools (RJ)
schools_geo <- geobr::read_schools(year = 2020) |>
  filter(abbrev_state == "RJ") |>
  st_transform(4326) 

coord_mat <- sf::st_coordinates(schools_geo$geom)
schools_geo <- schools_geo |>
  mutate(
    code_inep  = as.character(code_school),
    lon_geobr  = as.numeric(coord_mat[, 1]),
    lat_geobr  = as.numeric(coord_mat[, 2])
  ) |>
  st_drop_geometry() |>
  select(code_inep, lat_geobr, lon_geobr) |>
  distinct(code_inep, .keep_all = TRUE) |> 
  filter(!is.na(lat_geobr))

geo_tag <- dados_escolas |>
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    geo_ok = (!is.na(lat) & !is.na(lon)) &
      (tolower(geocodebr_precisao) %in% GOOD_PREC |
         tolower(geocodebr_tipo_resultado) %in% GOOD_TIPO)
  )

geo_best <- geo_tag |>
  filter(geo_ok) |>
  group_by(code_inep) |>
  arrange(is.na(desvio_metros), desvio_metros) |>
  slice(1) |>
  ungroup() |>
  transmute(code_inep, lat_best = lat, lon_best = lon)

geo_any <- geo_tag |>
  filter(!is.na(lat), !is.na(lon)) |>
  group_by(code_inep) |>
  slice(1) |>
  ungroup() |>
  transmute(code_inep, lat_any = lat, lon_any = lon)

geo_master <- geo_tag |>
  distinct(code_inep) |>
  left_join(geo_best,   by = "code_inep") |>
  left_join(geo_any,    by = "code_inep") |>
  left_join(schools_geo,by = "code_inep") |>
  mutate(
    lat_fix = dplyr::coalesce(lat_best, lat_geobr, lat_any),
    lon_fix = dplyr::coalesce(lon_best, lon_geobr, lon_any),
    coords_source_fix = dplyr::case_when(
      !is.na(lat_best) ~ "geocodebr_best",
      is.na(lat_best) & !is.na(lat_geobr) ~ "geobr",
      is.na(lat_geobr) & is.na(lat_geobr) & !is.na(lat_any) ~ "geocodebr_any",

      TRUE ~ NA_character_
    )
  ) |>
  select(code_inep, lat_fix, lon_fix, coords_source_fix)

geo_master <- subset(geo_master, coords_source_fix != 'geocodebr_any')

dados_escolas2 <- geo_tag |>
  left_join(geo_master, by = "code_inep") |>
  mutate(
    lat = lat_fix,
    lon = lon_fix,
    coords_source = dplyr::coalesce(coords_source_fix, coords_source)
  ) |>
  select(-geo_ok, -lat_fix, -lon_fix, -coords_source_fix)

# ----------------------------
# 5) Indicador simples de fechamento
# ----------------------------
stopifnot("tp_situacao_funcionamento" %in% names(dados_escolas2))
dados_escolas2 <- dados_escolas2 |>
  mutate(fechamento_escola = if_else(tp_situacao_funcionamento == 1, 0L, 1L))

# ============================================================
# 6) GEO (FAÇA UMA VEZ POR ESCOLA, NÃO POR ANO!)
# ============================================================

geo_base <- dados_escolas2 |>
  filter(!is.na(lat), !is.na(lon)) |>
  distinct(code_inep, lat, lon)

geocode_sf <- geo_base |>
  st_as_sf(coords = c("lon","lat"), crs = 4326, remove = FALSE)

# ---- 6.1) Runnout (shp) + csv com raios
raio_otimo <- sf::st_read(dsn = file.path(DIR_BASE, "runnout/Deslizamentos_Runnout.shp"), quiet = TRUE)

temp <- data.table::fread(file.path(DIR_BASE, "runnout/Runnout.csv")) |>
  select(Runnout3) |>
  mutate(
    controle_1000_runnout3 = Runnout3 + 1000,
    controle_2000_runnout3 = Runnout3 + 2000,
    controle_2500_runnout3 = Runnout3 + 2500,
    controle_3000_runnout3 = Runnout3 + 3000
  )

raio_otimo <- cbind(raio_otimo, temp) |>
  st_make_valid() |>
  st_transform(32723)

geocode_sf <- st_transform(geocode_sf, st_crs(raio_otimo))

raios_cols  <- c("Runnout3","controle_1000_runnout3","controle_2000_runnout3","controle_2500_runnout3","controle_3000_runnout3")
raios_names <- c("raio","raio_controle_1000","raio_controle_2000","raio_controle_2500","raio_controle_3000")

for (k in seq_along(raios_cols)) {
  buf <- st_buffer(raio_otimo$geometry, dist = raio_otimo[[raios_cols[k]]])
  aux <- st_intersects(geocode_sf$geometry, buf)
  geocode_sf[[raios_names[k]]] <- as.integer(lengths(aux) > 0)
}

# ---- 6.2) Cicatrizes (pontos_desastres) e raios fixos
petropolis <- st_read(file.path(DIR_BASE, "cicatrizes/Cicatriz_Pet_2011_UTM.shp"), quiet = TRUE) |>
  mutate(name_muni = "Petropolis") |>
  select(name_muni, geometry) |>
  st_make_valid()

teresopolis <- st_read(file.path(DIR_BASE, "cicatrizes/Cicatriz_Ter_2011_UTM.shp"), quiet = TRUE) |>
  mutate(name_muni = "Teresopolis") |>
  select(name_muni, geometry) |>
  st_make_valid() |>
  st_transform(st_crs(petropolis))

nova_friburgo <- st_read(file.path(DIR_BASE, "cicatrizes/Cicatriz_Nov_2011_UTM.shp"), quiet = TRUE) |>
  mutate(name_muni = "Nova_Friburgo") |>
  select(name_muni, geometry) |>
  st_make_valid() |>
  st_transform(st_crs(petropolis))

pontos_desastres <- rbind(petropolis, teresopolis, nova_friburgo) |>
  st_make_valid() |>
  st_transform(st_crs(raio_otimo))

raios_km <- c(2.5,5,7.5,10,12.5,15,20,22.5,25,27.5,30,70)

for (r in raios_km) {
  buf <- st_buffer(pontos_desastres$geometry, dist = r * 1000)
  aux <- st_intersects(geocode_sf$geometry, buf)
  nm  <- paste0("raio_", gsub("\\.", "_", r), "km")
  geocode_sf[[nm]] <- as.integer(lengths(aux) > 0)
}

# ---- 6.3) min_dist (SEM matriz gigante)
nn <- st_nn(geocode_sf, pontos_desastres, k = 1, returnDist = TRUE)
geocode_sf$min_dist <- vapply(nn$dist, function(x) if (length(x)) x[1] else NA_real_, numeric(1))
geocode_sf$min_dist <- as.numeric(geocode_sf$min_dist)

# ============================================================
# 7) Junta GEO de volta no painel ano-escola
# ============================================================
geo_vars <- geocode_sf |>
  st_drop_geometry() |>
  select(code_inep, min_dist, all_of(raios_names), starts_with("raio_")) |>
  distinct(code_inep, .keep_all = TRUE)

dados_escolas_final <- dados_escolas2 |>
  left_join(geo_vars, by = "code_inep")

# ============================================================
# 7.05) BALANCEAMENTO BRUTO (SEM BURACOS) — antes do census tract
# ============================================================
dados_escolas_final <- dados_escolas_final |>
  arrange(code_inep, ano) |>
  group_by(code_inep, ano) |>
  summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")

dados_escolas_final <- dados_escolas_final |>
  group_by(code_inep) |>
  tidyr::complete(ano = seq(min(ano, na.rm = TRUE), max(ano, na.rm = TRUE), by = 1L)) |>
  arrange(code_inep, ano) |>
  tidyr::fill(-code_inep, -ano, .direction = "updown") |>
  ungroup() |>
  mutate(code_inep = as.character(code_inep),
         ano       = as.integer(ano))

# ============================================================
# 7.1) Censo 2010 (setores) — setor MAIS PRÓXIMO por escola (1x)
# ============================================================
census_df <- data.table::fread(
  file.path(DIR_BASE, "census_tracts2010_brazil.csv"),
  select = c("code_tract","pop_branca","income_total","pop_per_household",
             "households_total","pop_water_network","pop_total","urban","favela","men")
)
census_df$code_tract <- as.character(census_df$code_tract)

census_geo <- geobr::read_census_tract(code_tract = "RJ", year = 2010, simplified = F) |>
  select(code_tract, zone, geom) |>
  mutate(code_tract = as.character(code_tract)) |>
  left_join(census_df, by = "code_tract")

census_geo <- st_transform(census_geo, st_crs(geocode_sf))

schools_census_sf <- st_join(geocode_sf, census_geo, join = st_nearest_feature, left = TRUE)

schools_census_vars <- schools_census_sf |>
  st_drop_geometry() |>
  select(
    code_inep, zone,
    pop_branca, income_total, pop_per_household,
    households_total, pop_water_network, pop_total,
    urban, favela, men
  ) |>
  distinct(code_inep, .keep_all = TRUE)

dados_escolas_final <- dados_escolas_final |>
  left_join(schools_census_vars, by = "code_inep")

# ============================================================
# 7.2) FECHAMENTO com 2 colunas + NA em tudo no período fechado
# ============================================================
ref_fech <- dados_escolas_final |>
  group_by(code_inep) |>
  summarise(
    ano_min = suppressWarnings(min(ano, na.rm = TRUE)),
    ano_max = suppressWarnings(max(ano, na.rm = TRUE)),
    ano_primeiro_fechado = {
      y <- suppressWarnings(min(ano[fechamento_escola == 1L], na.rm = TRUE))
      if (is.finite(y)) as.integer(y) else NA_integer_
    },
    ultimo_ano_aberto = {
      if (!is.na(ano_primeiro_fechado)) {
        y2 <- suppressWarnings(max(ano[ano < ano_primeiro_fechado], na.rm = TRUE))
        if (is.finite(y2)) as.integer(y2) else as.integer(ano_min)
      } else {
        as.integer(ano_max)
      }
    },
    .groups = "drop"
  )

dados_escolas_final <- dados_escolas_final |>
  left_join(ref_fech |> select(code_inep, ano_primeiro_fechado, ultimo_ano_aberto), by = "code_inep") |>
  mutate(
    fechamento_anos_fechado  = if_else(fechamento_escola == 1L, 1L, 0L),
    fechamento_ultimo_aberto = if_else(ano == ultimo_ano_aberto, 1L, 0L),
    fechamento_ultimo_aberto = if_else(fechamento_escola == 1L, NA_integer_, fechamento_ultimo_aberto)
  ) |>
  select(-ano_primeiro_fechado, -ultimo_ano_aberto)

keep_cols <- c(
  "code_inep", "ano",
  "tp_situacao_funcionamento", "fechamento_escola",
  "fechamento_ultimo_aberto", "fechamento_anos_fechado"
)
cols_to_na <- setdiff(names(dados_escolas_final), keep_cols)

dados_escolas_final <- dados_escolas_final |>
  mutate(across(all_of(cols_to_na), ~ if_else(fechamento_escola == 1L, NA, .x)))

# ============================================================
# 7.3) Derivadas finais
# ============================================================
dados_escolas_final <- dados_escolas_final |>
  mutate(
    fechamento           = fechamento_escola,
    log_docente          = log(as.numeric(n_docentes_total) + 1),
    log_salas            = log(as.numeric(qt_salas_existentes) + 1),
    income_total         = log(as.numeric(income_total) + 1),
    pop_total            = log(as.numeric(pop_total) + 1),
    pop_water_network    = log(as.numeric(pop_water_network) + 1),
    # FIX: aqui era qt_salas_utilizadas; funcionário é n_funcionarios_total
    log_num_funcionarios = log(as.numeric(n_funcionarios_total) + 1),
    log_aluno            = log(as.numeric(n_alunos_total) + 1)
  )

# ============================================================
# CHECKS: agora não pode ter buracos
# ============================================================
gaps_resumo <- dados_escolas_final |>
  distinct(code_inep, ano) |>
  group_by(code_inep) |>
  summarise(
    ano_min = min(ano, na.rm = TRUE),
    ano_max = max(ano, na.rm = TRUE),
    n_obs   = n(),
    n_esp   = ano_max - ano_min + 1L,
    tem_buraco = n_obs < n_esp,
    .groups = "drop"
  ) |>
  summarise(escolas_com_buraco = sum(tem_buraco), .groups = "drop")

# ============================================================
# Tratamento (escola-ano)
# ============================================================
dados_escolas_final  <- dados_escolas_final  |>
  group_by(code_inep) |> 
  arrange(code_inep, ano) |>
  tidyr::fill(min_dist,raio, .direction = 'downup') |> 
  tidyr::fill(pop_branca,income_total,pop_per_household,
              pop_total,urban,favela, .direction = 'downup') |> 
  filter(ano >= 2007) |>
  filter(ano < 2020) |>
  mutate(
    treat      = ifelse(raio == 1 & ano >= 2011, 1, 0),
    treat_unid = ifelse(raio == 1, 1, 0),
    treat_1yr  = ifelse(raio == 1 & ano == 2011, 1, 0),
    treat_2yr  = ifelse(raio == 1 & ano == 2012, 1, 0),
    treat_3yr  = ifelse(raio == 1 & ano == 2013, 1, 0),
    treat_4yr  = ifelse(raio == 1 & ano == 2014, 1, 0),
    treat_5yr  = ifelse(raio == 1 & ano == 2015, 1, 0),
    treat_6yr  = ifelse(raio == 1 & ano == 2016, 1, 0),
    treat_7yr  = ifelse(raio == 1 & ano == 2017, 1, 0),
    treat_8yr  = ifelse(raio == 1 & ano == 2018, 1, 0),
    treat_9yr  = ifelse(raio == 1 & ano == 2019, 1, 0)
  )

# Min dist em Km
dados_escolas_final$min_dist <- dados_escolas_final$min_dist / 1000
dados_escolas_final$fechamento = ifelse(is.na(dados_escolas_final$n_alunos_total),1,0)

# colunas "fixas" por escola (ajuste o padrão se quiser incluir/excluir)
cols_fixas <- names(dados_escolas_final)[grepl("^is_|^in_", names(df))]

dados_escolas_final <- dados_escolas_final %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep) %>%
  tidyr::fill(all_of(cols_fixas), .direction = "down") %>%  # preenche pra frente
  tidyr::fill(all_of(cols_fixas), .direction = "up") %>%    # e pra trás
  ungroup()

dados_escolas_final <- dados_escolas_final %>%
arrange(code_inep, ano) |> 
  group_by(code_inep) |> 
  mutate(fechamento = ifelse(is.na(n_alunos_total),1,0)) |> 
  mutate(ano_fechamento = min(ifelse(tp_situacao_funcionamento != 1,ano,NA), na.rm = T),
         fechamento = ifelse(ano == ano_fechamento,1,0),
         fechamento = ifelse(ano > ano_fechamento,NA,fechamento)) 

# ============================================================
# 8) Salvar painel escola-ano
# ============================================================
out_escolas <- file.path(DIR_OUT, "painel_escolas.parquet")
arrow::write_parquet(dados_escolas_final, out_escolas)

# ============================================================
# 9) H3 (res 10) — painel ano-hex
# - agrega outcomes e refaz min_dist/raios/treat no nível do hex
# ============================================================

# mean_na <- function(x) {
#   m <- mean(as.numeric(x), na.rm = TRUE)
#   if (is.nan(m)) NA_real_ else m
# }
# 
# mode_na <- function(x) {
#   x <- x[!is.na(x)]
#   if (!length(x)) return(NA_character_)
#   ux <- unique(as.character(x))
#   ux[which.max(tabulate(match(as.character(x), ux)))]
# }
# 
# # 9.1) Atribuir H3 a cada escola (usa lat/lon fixo já preenchido)
# base_h3 <- dados_escolas_final |>
#   filter(!is.na(lat), !is.na(lon)) |>
#   mutate(
#     lat = as.numeric(lat),
#     lon = as.numeric(lon),
#     h3_10 = h3jsr::point_to_cell(
#       input  = data.frame(lon = lon, lat = lat),
#       res    = 10,
#       simple = TRUE
#     )
#   )
# 
# # 9.2) Agregar outcomes (mantendo nomes) + total de escolas
# painel_h3_out <- base_h3 |>
#   group_by(ano, h3_10) |>
#   summarise(
#     n_escolas_total   = n(),
#     is_privada     = sum(is_privada == 1, na.rm = TRUE),
#     is_estadual     = sum(is_estadual == 1, na.rm = TRUE),
#     is_federal     = sum(is_federal == 1, na.rm = TRUE),
#     is_publica     = sum(is_publica == 1, na.rm = TRUE),
#     is_urbana     = sum(is_urbana == 1, na.rm = TRUE),
#     is_rural     = sum(is_rural == 1, na.rm = TRUE),
#     is_fundamental     = sum(is_fundamental == 1, na.rm = TRUE),
#     is_medio     = sum(is_medio == 1, na.rm = TRUE),
#     in_laboratorio_informatica     = sum(in_laboratorio_informatica == 1, na.rm = TRUE),
#     in_computador     = sum(in_computador == 1, na.rm = TRUE),
#     in_energia_rede_publica     = sum(in_energia_rede_publica == 1, na.rm = TRUE),
#     in_agua_rede_publica     = sum(in_agua_rede_publica == 1, na.rm = TRUE),
#     in_esgoto_rede_publica     = sum(in_esgoto_rede_publica == 1, na.rm = TRUE),
#     is_fundamental     = sum(is_fundamental == 1, na.rm = TRUE),
#     in_quadra_esportes     = sum(in_quadra_esportes == 1, na.rm = TRUE),
#     in_biblioteca     = sum(in_biblioteca == 1, na.rm = TRUE),
#     n_docentes_total  = sum(as.numeric(n_docentes_total), na.rm = TRUE),
#     n_alunos_total  = sum(as.numeric(n_alunos_total), na.rm = TRUE),
#     n_funcionarios_total  = sum(as.numeric(n_funcionarios_total), na.rm = TRUE),
#     fechamento_escola = sum(fechamento_ultimo_aberto  == 1L, na.rm = TRUE),
#     
#     # rendimento: média no hex
#     aprov_cat_fund  = mean_na(aprov_cat_fund),
#     aprov_cat_medio = mean_na(aprov_cat_medio),
#     reprov_cat_fund  = mean_na(reprov_cat_fund),
#     reprov_cat_medio = mean_na(reprov_cat_medio),
#     aband_cat_fund  = mean_na(aband_cat_fund),
#     aband_cat_medio = mean_na(aband_cat_medio),
#     
#     # ideb: média no hex
#     ideb_ai   = mean_na(ideb_ai),
#     ideb_af   = mean_na(ideb_af),
#     ideb_em   = mean_na(ideb_em),
#     ideb_fund = mean_na(ideb_fund),
#     
#     # -----------------------
#     # CENSO 2010 (setor mais próximo) — AGREGADO NO H3
#     # (zona: moda; demais: média entre escolas do hex)
#     # -----------------------
#     zone              = mode_na(zone),
#     pop_branca        = mean_na(pop_branca),
#     income_total      = mean_na(income_total),
#     pop_per_household = mean_na(pop_per_household),
#     households_total  = mean_na(households_total),
#     pop_water_network = mean_na(pop_water_network),
#     pop_total         = mean_na(pop_total),
#     urban             = mean_na(urban),
#     favela            = mean_na(favela),
#     men               = mean_na(men),
#     
#     .groups = "drop"
#   )
# 
# # 9.3) Geo do hex: centro do H3 -> recalcula raios + min_dist
# h3_cells <- painel_h3_out |> distinct(h3_10)
# 
# # centro do hex (WGS84)
# h3_centers_sfc <- h3jsr::cell_to_point(h3_address = h3_cells$h3_10, simple = TRUE)
# 
# h3_sf <- sf::st_sf(
#   h3_10 = h3_cells$h3_10,
#   geometry = h3_centers_sfc,
#   crs = 4326
# )
# 
# # guardar lat/lon do centro
# h3_xy <- sf::st_coordinates(h3_sf)
# h3_sf$lon <- as.numeric(h3_xy[, 1])
# h3_sf$lat <- as.numeric(h3_xy[, 2])
# 
# # trabalhar no mesmo CRS do runnout/cicatrizes (já é 32723)
# h3_sf <- st_transform(h3_sf, st_crs(raio_otimo))
# 
# # runnout (mesma lógica)
# for (k in seq_along(raios_cols)) {
#   buf <- st_buffer(raio_otimo$geometry, dist = raio_otimo[[raios_cols[k]]])
#   aux <- st_intersects(h3_sf$geometry, buf)
#   h3_sf[[raios_names[k]]] <- as.integer(lengths(aux) > 0)
# }
# 
# # cicatrizes (mesmos raios fixos)
# for (r in raios_km) {
#   buf <- st_buffer(pontos_desastres$geometry, dist = r * 1000)
#   aux <- st_intersects(h3_sf$geometry, buf)
#   nm  <- paste0("raio_", gsub("\\.", "_", r), "km")
#   h3_sf[[nm]] <- as.integer(lengths(aux) > 0)
# }
# 
# # min_dist do hex ao ponto desastre mais próximo (metros)
# nn_h3 <- st_nn(h3_sf, pontos_desastres, k = 1, returnDist = TRUE)
# h3_sf$min_dist <- vapply(nn_h3$dist, function(x) if (length(x)) x[1] else NA_real_, numeric(1))
# h3_sf$min_dist <- as.numeric(h3_sf$min_dist)
# 
# # 9.4) Juntar geo do hex no painel h3-ano
# h3_geo_vars <- h3_sf |>
#   st_drop_geometry() |>
#   select(h3_10, lat, lon, min_dist, all_of(raios_names), starts_with("raio_")) |>
#   distinct(h3_10, .keep_all = TRUE)
# 
# painel_h3_final <- painel_h3_out |>
#   left_join(h3_geo_vars, by = "h3_10")
# 
# # min_dist em km
# painel_h3_final$min_dist <- painel_h3_final$min_dist / 1000
# 
# # 9.5) treat/treat_yr no nível do hex
# painel_h3_final <- painel_h3_final |>
#   filter(ano >= 2007, ano < 2020) |>
#   mutate(
#     treat      = ifelse(raio == 1 & ano >= 2011, 1, 0),
#     treat_unid = ifelse(raio == 1, 1, 0),
#     treat_1yr  = ifelse(raio == 1 & ano == 2011, 1, 0),
#     treat_2yr  = ifelse(raio == 1 & ano == 2012, 1, 0),
#     treat_3yr  = ifelse(raio == 1 & ano == 2013, 1, 0),
#     treat_4yr  = ifelse(raio == 1 & ano == 2014, 1, 0),
#     treat_5yr  = ifelse(raio == 1 & ano == 2015, 1, 0),
#     treat_6yr  = ifelse(raio == 1 & ano == 2016, 1, 0),
#     treat_7yr  = ifelse(raio == 1 & ano == 2017, 1, 0),
#     treat_8yr  = ifelse(raio == 1 & ano == 2018, 1, 0),
#     treat_9yr  = ifelse(raio == 1 & ano == 2019, 1, 0)
#   )
# 
# painel_h3_final <- painel_h3_final |>
#   mutate(
#     fechamento           = fechamento_escola,
#     log_docente          = log(as.numeric(n_docentes_total) + 1),
#     log_num_funcionarios = log(as.numeric(n_funcionarios_total) + 1),
#     log_aluno            = log(as.numeric(n_alunos_total) + 1)
#   ) 
# 
# # ============================================================
# # 10) Salvar painel H3 (res 10)
# # ============================================================
# out_h3 <- file.path(DIR_OUT, "painel_h3_res10.parquet")
# arrow::write_parquet(painel_h3_final, out_h3)
