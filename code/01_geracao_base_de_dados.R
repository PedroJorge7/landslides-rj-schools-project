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
DIR_PARQUET <- "G:/.shortcut-targets-by-id/1K-TPEsFyx_miIiVaxXX3sKwYBAxHWeUj/Natural Disasters and Educational outcomes  Evidence from the 2011 Rio de Janeiro Landslides/input/"      # << ajuste
DIR_BASE    <- "G:/.shortcut-targets-by-id/1K-TPEsFyx_miIiVaxXX3sKwYBAxHWeUj/Natural Disasters and Educational outcomes  Evidence from the 2011 Rio de Janeiro Landslides/input/"    # << ajuste (cicatrizes/runnout + csv census)
DIR_TEMP    <- file.path(DIR_BASE, "temp")
DIR_OUT     <- "./output/"   # << ajuste

# ----------------------------
# 0.1) Complemento escola-ano: QT_DOC_*/QT_TUR_*/QT_FUNCIONARIOS
# - os parquets geolocalizados nao carregam todas as contagens do Censo Escolar
# - esta rotina busca os CSVs antigos em input/temp e reaproveita somente o bloco
#   necessario para docentes/turmas/funcionarios
# ----------------------------
TEMP_DOC_SELECT <- c(
  "NU_ANO_CENSO", "CO_UF", "CO_ENTIDADE", "QT_FUNCIONARIOS",
  "QT_DOC_BAS", "QT_DOC_INF", "QT_DOC_INF_CRE", "QT_DOC_INF_PRE",
  "QT_DOC_FUND", "QT_DOC_FUND_AI", "QT_DOC_FUND_AF", "QT_DOC_MED",
  "QT_DOC_PROF", "QT_DOC_PROF_TEC", "QT_DOC_EJA", "QT_DOC_EJA_FUND",
  "QT_DOC_EJA_MED", "QT_DOC_ESP", "QT_DOC_ESP_CC", "QT_DOC_ESP_CE",
  "QT_TUR_BAS", "QT_TUR_INF", "QT_TUR_INF_CRE", "QT_TUR_INF_PRE",
  "QT_TUR_FUND", "QT_TUR_FUND_AI", "QT_TUR_FUND_AF", "QT_TUR_MED",
  "QT_TUR_PROF", "QT_TUR_PROF_TEC", "QT_TUR_EJA", "QT_TUR_EJA_FUND",
  "QT_TUR_EJA_MED", "QT_TUR_ESP", "QT_TUR_ESP_CC", "QT_TUR_ESP_CE"
)

load_temp_docente_counts <- function(dir_temp) {
  if (!dir.exists(dir_temp)) {
    warning(sprintf(
      "Diretorio '%s' nao encontrado. As colunas QT_DOC_*/QT_TUR_*/QT_FUNCIONARIOS nao serao integradas.",
      dir_temp
    ))
    return(NULL)
  }

  files <- list.files(
    path = dir_temp,
    pattern = "^microdados_ed_basica_[0-9]{4}\\.csv$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning(sprintf(
      "Nenhum arquivo microdados_ed_basica_*.csv foi encontrado em '%s'.",
      dir_temp
    ))
    return(NULL)
  }

  files <- files[order(files)]

  temp_list <- lapply(files, function(path_file) {
    file_name <- basename(path_file)
    header_cols <- names(data.table::fread(
      path_file,
      nrows = 0,
      sep = ";",
      showProgress = FALSE
    ))

    select_cols <- intersect(TEMP_DOC_SELECT, header_cols)
    required_cols <- c("NU_ANO_CENSO", "CO_ENTIDADE")

    if (!all(required_cols %in% select_cols)) {
      warning(sprintf(
        "[temp_docentes] %s ignorado porque nao contem as chaves obrigatorias (%s).",
        file_name,
        paste(required_cols, collapse = ", ")
      ))
      return(NULL)
    }

    missing_cols <- setdiff(TEMP_DOC_SELECT, select_cols)
    if (length(missing_cols) > 0) {
      message(sprintf(
        "[temp_docentes] %s: %s colunas nao estavam presentes no layout e foram ignoradas.",
        file_name,
        length(missing_cols)
      ))
    }

    message(sprintf("[temp_docentes] lendo %s", file_name))

    dt <- data.table::fread(
      path_file,
      sep = ";",
      select = select_cols,
      showProgress = FALSE
    )

    data.table::setnames(dt, tolower(names(dt)))

    if ("co_uf" %in% names(dt)) {
      dt <- dt[
        as.character(co_uf) == "33" |
          substr(as.character(co_entidade), 1, 2) == "33"
      ]
    } else {
      dt <- dt[substr(as.character(co_entidade), 1, 2) == "33"]
    }

    if (nrow(dt) == 0) {
      return(NULL)
    }

    dt[, ano := as.integer(nu_ano_censo)]
    dt[, code_inep := as.character(co_entidade)]
    drop_cols <- intersect(c("nu_ano_censo", "co_entidade", "co_uf"), names(dt))
    if (length(drop_cols) > 0) {
      dt[, (drop_cols) := NULL]
    }

    unique(dt, by = c("code_inep", "ano"))
  })

  temp_list <- Filter(Negate(is.null), temp_list)

  if (length(temp_list) == 0) {
    warning("Nenhum microdado complementar de docentes foi carregado a partir de input/temp.")
    return(NULL)
  }

  temp_df <- data.table::rbindlist(temp_list, use.names = TRUE, fill = TRUE)
  temp_df <- unique(temp_df, by = c("code_inep", "ano"))

  dplyr::as_tibble(temp_df)
}

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
# 1.1) IDEB ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â renomeia + cria nota_fund + remove code_muni
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

docentes_temp <- load_temp_docente_counts(DIR_TEMP)

if (!is.null(docentes_temp)) {
  join_keys <- c("ano", "code_inep")
  temp_payload <- setdiff(names(docentes_temp), join_keys)
  overlap_cols <- intersect(temp_payload, names(dados_escolas))

  dados_escolas <- dados_escolas |>
    left_join(docentes_temp, by = join_keys, suffix = c("", "_temp"))

  if (length(overlap_cols) > 0) {
    for (nm in overlap_cols) {
      nm_temp <- paste0(nm, "_temp")
      dados_escolas[[nm]] <- dplyr::coalesce(dados_escolas[[nm]], dados_escolas[[nm_temp]])
    }

    dados_escolas <- dados_escolas |>
      select(-all_of(paste0(overlap_cols, "_temp")))
  }

  dados_escolas <- dados_escolas |>
    mutate(
      n_docentes_total = dplyr::coalesce(as.numeric(n_docentes_total), as.numeric(qt_doc_bas)),
      n_funcionarios_total = dplyr::coalesce(as.numeric(n_funcionarios_total), as.numeric(qt_funcionarios))
    )

  message(sprintf(
    "[temp_docentes] %s pares escola-ano integrados ao painel principal.",
    format(nrow(docentes_temp), big.mark = ".", decimal.mark = ",")
  ))
}

docentes_profile_path <- file.path(DIR_OUT, "docentes_escola_ano.parquet")

if (file.exists(docentes_profile_path)) {
  docentes_profile <- arrow::read_parquet(docentes_profile_path) |>
    mutate(
      ano = as.integer(ano),
      code_inep = as.character(code_inep)
    )

  join_keys <- c("ano", "code_inep")
  profile_payload <- setdiff(names(docentes_profile), join_keys)
  overlap_cols <- intersect(profile_payload, names(dados_escolas))

  dados_escolas <- dados_escolas |>
    left_join(docentes_profile, by = join_keys, suffix = c("", "_doc"))

  if (length(overlap_cols) > 0) {
    for (nm in overlap_cols) {
      nm_doc <- paste0(nm, "_doc")
      dados_escolas[[nm]] <- dplyr::coalesce(dados_escolas[[nm]], dados_escolas[[nm_doc]])
    }

    dados_escolas <- dados_escolas |>
      select(-all_of(paste0(overlap_cols, "_doc")))
  }

  dados_escolas <- dados_escolas |>
    mutate(
      n_docentes_total = dplyr::coalesce(as.numeric(n_docentes_total), as.numeric(n_docentes_total_microdados), as.numeric(qt_doc_bas))
    )

  message(sprintf(
    "[docentes_profile] %s pares escola-ano integrados ao painel principal.",
    format(nrow(docentes_profile), big.mark = ".", decimal.mark = ",")
  ))
} else {
  message(sprintf(
    "[docentes_profile] arquivo nao encontrado em '%s'. O painel seguira sem o agregado dos microdados de docentes.",
    docentes_profile_path
  ))
}

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
# 6) GEO (FAÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¡A UMA VEZ POR ESCOLA, NÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â‚¬Å¾Ã‚Â¢O POR ANO!)
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
# 7.05) BALANCEAMENTO BRUTO (SEM BURACOS) ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â antes do census tract
# ============================================================
dados_escolas_final <- dados_escolas_final |>
  arrange(code_inep, ano) |>
  group_by(code_inep, ano) |>
  summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")

cols_fill_balance <- setdiff(
  names(dados_escolas_final),
  c("code_inep", "ano", grep("^ideb_", names(dados_escolas_final), value = TRUE))
)

dados_escolas_final <- dados_escolas_final |>
  group_by(code_inep) |>
  tidyr::complete(ano = seq(min(ano, na.rm = TRUE), max(ano, na.rm = TRUE), by = 1L)) |>
  arrange(code_inep, ano) |>
  tidyr::fill(all_of(cols_fill_balance), .direction = "updown") |>
  ungroup() |>
  mutate(code_inep = as.character(code_inep),
         ano       = as.integer(ano))

# ============================================================
# 7.1) Censo 2010 (setores) ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â setor MAIS PRÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Â¦ÃƒÂ¢Ã¢â€šÂ¬Ã…â€œXIMO por escola (1x)
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
# 7.2) FECHAMENTO com 2 colunas + NA em tudo no perÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â­odo fechado
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
    # FIX: aqui era qt_salas_utilizadas; funcionÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¡rio ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â© n_funcionarios_total
    log_num_funcionarios = log(as.numeric(n_funcionarios_total) + 1),
    log_aluno            = log(as.numeric(n_alunos_total) + 1),
    dsu_media            = rowMeans(cbind(as.numeric(dsu_fund), as.numeric(dsu_medio)), na.rm = TRUE),
    had_media            = rowMeans(cbind(as.numeric(had_fund), as.numeric(had_medio)), na.rm = TRUE),
    tdi_media            = rowMeans(cbind(as.numeric(tdi_fund), as.numeric(tdi_medio)), na.rm = TRUE),
    docentes_por_100_alunos_media = if_else(
      as.numeric(n_alunos_total) > 0,
      100 * as.numeric(n_docentes_total) / as.numeric(n_alunos_total),
      NA_real_
    ),
    docentes_por_sala_media = if_else(
      as.numeric(qt_salas_utilizadas) > 0,
      as.numeric(n_docentes_total) / as.numeric(qt_salas_utilizadas),
      NA_real_
    ),
    dsu_media = if_else(is.nan(dsu_media), NA_real_, dsu_media),
    had_media = if_else(is.nan(had_media), NA_real_, had_media),
    tdi_media = if_else(is.nan(tdi_media), NA_real_, tdi_media),
    n_docentes_superior = if_else(
      !is.na(dsu_media) & !is.na(as.numeric(n_docentes_total)),
      pmax(0, pmin(1, dsu_media / 100) * as.numeric(n_docentes_total)),
      NA_real_
    ),
    n_docentes_nao_superior = if_else(
      !is.na(dsu_media) & !is.na(as.numeric(n_docentes_total)),
      pmax(0, (1 - pmin(1, dsu_media / 100)) * as.numeric(n_docentes_total)),
      NA_real_
    ),
    log_docentes_superior = log(n_docentes_superior + 1),
    log_docentes_nao_superior = log(n_docentes_nao_superior + 1),
    log_doc_superior = log_docentes_superior,
    log_nao_doc_superior = log_docentes_nao_superior
  )

# ============================================================
# 7.3.1) Perfil docente (quando colunas brutas existirem)
# ============================================================
pick_first_existing <- function(data, candidates) {
  nms <- names(data)
  nms_low <- tolower(nms)
  cand_low <- tolower(candidates)
  idx <- match(cand_low, nms_low)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(NA_character_)
  nms[idx[1]]
}

to_num_or_na <- function(data, col) {
  if (is.na(col) || !col %in% names(data)) {
    return(rep(NA_real_, nrow(data)))
  }
  as.numeric(data[[col]])
}

col_doc_total <- pick_first_existing(dados_escolas_final, c("n_docentes_total", "qt_doc_bas", "docente"))
col_doc_lic <- pick_first_existing(dados_escolas_final, c(
  "n_docentes_licenciatura", "n_docentes_com_licenciatura", "qt_docentes_licenciatura",
  "qt_doc_licenciatura", "docentes_licenciatura", "qt_doc_bas_licenciatura",
  "qt_doc_bas_com_licenciatura"
))
col_doc_efetivo <- pick_first_existing(dados_escolas_final, c(
  "n_docentes_efetivos", "n_docentes_concursados", "qt_docentes_efetivos",
  "qt_docentes_concursados", "concursado", "docentes_efetivos",
  "qt_doc_bas_contr_efetiv", "qt_doc_bas_contr_efetivo"
))
col_doc_temporario <- pick_first_existing(dados_escolas_final, c(
  "n_docentes_temporarios", "qt_docentes_temporarios", "temporario", "docentes_temporarios",
  "qt_doc_bas_contr_temp", "qt_doc_bas_temporario"
))
col_doc_pos <- pick_first_existing(dados_escolas_final, c(
  "n_docentes_pos_graduacao", "n_docentes_com_pos_graduacao", "qt_docentes_pos_graduacao",
  "qt_doc_pos_graduacao", "docentes_pos_graduacao", "qt_doc_bas_pos"
))

# Distribuicao de docentes por etapa
col_doc_fund <- pick_first_existing(dados_escolas_final, c("qt_doc_fund", "n_docentes_fund", "n_docentes_fundamental"))
col_doc_medio <- pick_first_existing(dados_escolas_final, c("qt_doc_med", "n_docentes_medio", "n_docentes_ensino_medio"))
col_doc_eja <- pick_first_existing(dados_escolas_final, c("qt_doc_eja", "n_docentes_eja"))
col_doc_esp_etapa <- pick_first_existing(dados_escolas_final, c("qt_doc_esp", "n_docentes_esp", "n_docentes_educacao_especial"))

# Razoes docentes
col_tur_total <- pick_first_existing(dados_escolas_final, c("qt_tur_bas", "qtd_turmas", "qt_turmas_total", "qtd_turmas_total"))
col_func_total <- pick_first_existing(dados_escolas_final, c("n_funcionarios_total", "qt_funcionarios", "qt_funcionarios_total"))

# fallback pos-graduacao: soma de especializacao + mestrado + doutorado
col_doc_esp_form <- pick_first_existing(dados_escolas_final, c("n_docentes_especializacao", "qt_docentes_especializacao", "docentes_especializacao", "qt_doc_bas_especializacao"))
col_doc_mes <- pick_first_existing(dados_escolas_final, c("n_docentes_mestrado", "qt_docentes_mestrado", "docentes_mestrado", "qt_doc_bas_mestrado"))
col_doc_dou <- pick_first_existing(dados_escolas_final, c("n_docentes_doutorado", "qt_docentes_doutorado", "docentes_doutorado", "qt_doc_bas_doutorado"))

report_col_source <- function(label, col_name) {
  if (is.na(col_name)) {
    message(sprintf("[perfil_docente] %s: coluna fonte nao encontrada (indicador pode ficar NA).", label))
  } else {
    message(sprintf("[perfil_docente] %s: usando coluna '%s'.", label, col_name))
  }
}

report_col_source("doc_total", col_doc_total)
report_col_source("doc_lic", col_doc_lic)
report_col_source("doc_efetivo", col_doc_efetivo)
report_col_source("doc_temporario", col_doc_temporario)
report_col_source("doc_pos", col_doc_pos)
report_col_source("doc_fund", col_doc_fund)
report_col_source("doc_medio", col_doc_medio)
report_col_source("doc_eja", col_doc_eja)
report_col_source("doc_esp_etapa", col_doc_esp_etapa)
report_col_source("tur_total", col_tur_total)
report_col_source("func_total", col_func_total)

if (all(is.na(c(col_doc_fund, col_doc_medio, col_doc_eja, col_doc_esp_etapa, col_tur_total)))) {
  warning(paste0(
    "Nenhuma coluna QT_DOC_*/QT_TUR_* foi encontrada no painel principal. ",
    "Sem integrar os microdados escola-ano de input/temp, os indicadores por etapa e a razao docentes/turma ficarao NA."
  ))
}

if (all(is.na(c(col_doc_lic, col_doc_efetivo, col_doc_temporario, col_doc_pos, col_doc_esp_form, col_doc_mes, col_doc_dou)))) {
  message(paste0(
    "[perfil_docente] Nao foram encontradas colunas de formacao/vinculo docente. ",
    "Os arquivos microdados_ed_basica_*.csv usados aqui trazem QT_DOC_* por etapa, mas nao trazem ",
    "licenciatura, tipo de contrato ou pos-graduacao; esses indicadores permanecerao NA sem outra fonte."
  ))
}

doc_total <- to_num_or_na(dados_escolas_final, col_doc_total)
doc_lic <- to_num_or_na(dados_escolas_final, col_doc_lic)
doc_efetivo <- to_num_or_na(dados_escolas_final, col_doc_efetivo)
doc_temporario <- to_num_or_na(dados_escolas_final, col_doc_temporario)
doc_pos <- to_num_or_na(dados_escolas_final, col_doc_pos)
doc_fund <- to_num_or_na(dados_escolas_final, col_doc_fund)
doc_medio <- to_num_or_na(dados_escolas_final, col_doc_medio)
doc_eja <- to_num_or_na(dados_escolas_final, col_doc_eja)
doc_esp_etapa <- to_num_or_na(dados_escolas_final, col_doc_esp_etapa)
tur_total <- to_num_or_na(dados_escolas_final, col_tur_total)
func_total <- to_num_or_na(dados_escolas_final, col_func_total)

if (all(is.na(doc_pos))) {
  comp_cols <- c(col_doc_esp_form, col_doc_mes, col_doc_dou)
  comp_cols <- comp_cols[!is.na(comp_cols)]

  if (length(comp_cols) > 0) {
    pos_mat <- as.data.frame(lapply(comp_cols, function(x) as.numeric(dados_escolas_final[[x]])))
    doc_pos <- rowSums(pos_mat, na.rm = TRUE)
    all_na <- apply(is.na(pos_mat), 1, all)
    doc_pos[all_na] <- NA_real_
  }
}

doc_efetivo_temporario <- dplyr::coalesce(doc_efetivo, 0) + dplyr::coalesce(doc_temporario, 0)
doc_efetivo_temporario[is.na(doc_efetivo) & is.na(doc_temporario)] <- NA_real_

dados_escolas_final <- dados_escolas_final |>
  mutate(
    pct_docentes_licenciados = if_else(doc_total > 0, 100 * doc_lic / doc_total, NA_real_),
    pct_docentes_efetivos = if_else(doc_total > 0, 100 * doc_efetivo / doc_total, NA_real_),
    pct_docentes_temporarios = if_else(doc_total > 0, 100 * doc_temporario / doc_total, NA_real_),
    pct_docentes_efetivos_temporarios = if_else(doc_total > 0, 100 * doc_efetivo_temporario / doc_total, NA_real_),
    pct_docentes_pos_graduacao = if_else(doc_total > 0, 100 * doc_pos / doc_total, NA_real_),
    pct_docentes_fundamental = if_else(doc_total > 0, 100 * doc_fund / doc_total, NA_real_),
    pct_docentes_medio = if_else(doc_total > 0, 100 * doc_medio / doc_total, NA_real_),
    pct_docentes_eja = if_else(doc_total > 0, 100 * doc_eja / doc_total, NA_real_),
    pct_docentes_educ_especial = if_else(doc_total > 0, 100 * doc_esp_etapa / doc_total, NA_real_),
    docentes_por_turma_media = if_else(tur_total > 0, doc_total / tur_total, NA_real_),
    pct_docentes_no_total_funcionarios = if_else(func_total > 0, 100 * doc_total / func_total, NA_real_),
    alunos_por_turma_media = if_else(tur_total > 0, as.numeric(n_alunos_total) / tur_total, NA_real_)
  )
# CHECKS: agora nÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â£o pode ter buracos
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

# colunas "fixas" por escola (ajuste o padrÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â£o se quiser incluir/excluir)
cols_fixas <- names(dados_escolas_final)[grepl("^is_|^in_", names(dados_escolas_final))]

dados_escolas_final <- dados_escolas_final %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep) %>%
  tidyr::fill(all_of(cols_fixas), .direction = "down") %>%  # preenche pra frente
  tidyr::fill(all_of(cols_fixas), .direction = "up") %>%    # e pra trÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¡s
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
# 9) H3 (res 10) ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â painel ano-hex
# - agrega outcomes e refaz min_dist/raios/treat no nÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â­vel do hex
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
# # 9.1) Atribuir H3 a cada escola (usa lat/lon fixo jÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¡ preenchido)
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
#     # rendimento: mÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â©dia no hex
#     aprov_cat_fund  = mean_na(aprov_cat_fund),
#     aprov_cat_medio = mean_na(aprov_cat_medio),
#     reprov_cat_fund  = mean_na(reprov_cat_fund),
#     reprov_cat_medio = mean_na(reprov_cat_medio),
#     aband_cat_fund  = mean_na(aband_cat_fund),
#     aband_cat_medio = mean_na(aband_cat_medio),
#     
#     # ideb: mÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â©dia no hex
#     ideb_ai   = mean_na(ideb_ai),
#     ideb_af   = mean_na(ideb_af),
#     ideb_em   = mean_na(ideb_em),
#     ideb_fund = mean_na(ideb_fund),
#     
#     # -----------------------
#     # CENSO 2010 (setor mais prÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³ximo) ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â AGREGADO NO H3
#     # (zona: moda; demais: mÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â©dia entre escolas do hex)
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
# # trabalhar no mesmo CRS do runnout/cicatrizes (jÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¡ ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â© 32723)
# h3_sf <- st_transform(h3_sf, st_crs(raio_otimo))
# 
# # runnout (mesma lÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³gica)
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
# # min_dist do hex ao ponto desastre mais prÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³ximo (metros)
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
# # 9.5) treat/treat_yr no nÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Â ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â­vel do hex
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

