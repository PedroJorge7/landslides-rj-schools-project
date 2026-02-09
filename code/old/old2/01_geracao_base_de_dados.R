# ============================================================
# JOIN BASES (ESCOLAS) + FECHAMENTO + GEO RUNNOUT (SEM IFS)
# - Join por: ano + code_inep
# - Mantém do Censo: dep_cat, loc_cat, lat/lon, etc.
# - Nas bases gold: fica só agregado (fundamental, médio, escola)
# - BALANCEIA (sem buracos) ANTES do census tract
# - IDEB: renomeia colunas, cria nota_fund (média iniciais+finais),
#         remove code_muni
# - Regras de fechamento:
#     * fechamento_anos_fechado  = 1 nos anos fechados, 0 nos abertos
#     * fechamento_ultimo_aberto = 1 só no último ano aberto, 0 nos outros abertos, NA nos fechados
#     * nos anos fechados: TODAS as colunas viram NA, exceto (ano, code_inep e colunas de fechamento/status)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(sf)
  library(data.table)
  library(nngeo)
  library(tidyr)
  library(geobr)
})

# ----------------------------
# 0) Paths
# ----------------------------
DIR_PARQUET <- "input/"      # << ajuste
DIR_BASE    <- "./input/"    # << ajuste (cicatrizes/runnout + csv census)

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
# 1.1) IDEB (base normal) — renomeia + cria nota_fund + remove code_muni
# ----------------------------
ideb <- arrow::read_parquet(file.path(DIR_PARQUET, "ideb_notas_escola_ano.parquet")) |>
  filter(substr(as.character(code_inep), 1, 2) == "33")

stopifnot(all(c("ano","code_inep") %in% names(ideb)))

ideb <- ideb %>%
  mutate(
    code_inep = as.character(code_inep),
    ano       = as.integer(ano)
  ) %>%
  select(-code_muni) %>%  
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
  ) %>%
  mutate(
    # nota fundamental = média (anos iniciais e finais)
    ideb_fund = rowMeans(cbind(as.numeric(ideb_ai), as.numeric(ideb_af)), na.rm = TRUE),
    ideb_fund = if_else(is.nan(ideb_fund), NA_real_, ideb_fund)
  )

# ----------------------------
# 2) Padronizar chaves (SEM IF)
# ----------------------------
stopifnot(all(c("ano","co_entidade","lat","lon","dep_cat","loc_cat") %in% names(censo)))
stopifnot(all(c("ano","code_inep") %in% names(dsi)))
stopifnot(all(c("ano","code_inep") %in% names(dsu)))
stopifnot(all(c("ano","code_inep") %in% names(txr)))
stopifnot(all(c("ano","code_inep") %in% names(atu)))
stopifnot(all(c("ano","code_school") %in% names(had)))

censo <- censo %>%
  rename(code_inep = co_entidade) %>%
  mutate(
    code_inep = as.character(code_inep),
    ano       = as.integer(ano)
  )

atu  <- atu %>% mutate(code_inep = as.character(code_inep), ano = as.integer(ano))
dsi  <- dsi %>% mutate(code_inep = as.character(code_inep), ano = as.integer(ano))
dsu  <- dsu %>% mutate(code_inep = as.character(code_inep), ano = as.integer(ano))
txr  <- txr %>% mutate(code_inep = as.character(code_inep), ano = as.integer(ano))

had <- had %>%
  rename(code_inep = code_school) %>%
  mutate(
    code_inep = as.character(code_inep),
    ano       = as.integer(ano)
  )

# ----------------------------
# 3) Reduzir colunas das bases gold
# ----------------------------
atu <- atu %>% select(ano, code_inep, atu_unificada, atu_fund, atu_medio)
dsi <- dsi %>% select(ano, code_inep, tdi_fund, tdi_medio)
dsu <- dsu %>% select(ano, code_inep, dsu_fund, dsu_medio)
had <- had %>% select(ano, code_inep, had_fund, had_medio)

txr <- txr %>%
  select(
    ano, code_inep,
    aprov_cat_fund, aprov_cat_medio,
    reprov_cat_fund, reprov_cat_medio,
    aband_cat_fund, aband_cat_medio
  )

# ----------------------------
# 4) Join final (censo manda) + IDEB (já tratado)
# ----------------------------
dados_escolas <- censo %>%
  left_join(atu,  by = c("ano","code_inep")) %>%
  left_join(dsi,  by = c("ano","code_inep")) %>%
  left_join(dsu,  by = c("ano","code_inep")) %>%
  left_join(had,  by = c("ano","code_inep")) %>%
  left_join(txr,  by = c("ano","code_inep")) %>%
  left_join(ideb, by = c("ano","code_inep"))


GOOD_PREC <- c("logradouro", "numero", "numero_aproximado")
GOOD_TIPO <- c("dc01")

dados_escolas2 <- dados_escolas %>%
  mutate(
    geo_ok = (!is.na(lat) & !is.na(lon)) &
      (tolower(geocodebr_precisao) %in% GOOD_PREC |
         tolower(geocodebr_tipo_resultado) %in% GOOD_TIPO)
  ) %>%
  group_by(code_inep) %>%
  mutate(has_ok = any(geo_ok, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(has_ok) %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep) %>%
  mutate(
    lat = if_else(geo_ok, lat, as.numeric(NA)),
    lon = if_else(geo_ok, lon, as.numeric(NA))
  ) %>%
  tidyr::fill(lat, lon, .direction = "down") %>%  # última boa (passado -> futuro)
  ungroup() %>%
  select(-geo_ok, -has_ok)


# ----------------------------
# 5) Indicador simples de fechamento
# ----------------------------
stopifnot("tp_situacao_funcionamento" %in% names(dados_escolas2))
dados_escolas2 <- dados_escolas2 %>%
  mutate(fechamento_escola = if_else(tp_situacao_funcionamento == 1, 0L, 1L))

# ============================================================
# 6) GEO (FAÇA UMA VEZ POR ESCOLA, NÃO POR ANO!)
# ============================================================

geo_base <- dados_escolas2 %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  distinct(code_inep, lat, lon)

geocode_sf <- geo_base %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326, remove = FALSE)

# ---- 6.1) Runnout (shp) + csv com raios
raio_otimo <- sf::st_read(dsn = file.path(DIR_BASE, "runnout/Deslizamentos_Runnout.shp"), quiet = TRUE)

temp <- data.table::fread(file.path(DIR_BASE, "runnout/Runnout.csv")) %>%
  select(Runnout3) %>%
  mutate(
    controle_1000_runnout3 = Runnout3 + 1000,
    controle_2000_runnout3 = Runnout3 + 2000,
    controle_2500_runnout3 = Runnout3 + 2500,
    controle_3000_runnout3 = Runnout3 + 3000
  )

raio_otimo <- cbind(raio_otimo, temp) %>%
  st_make_valid() %>%
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
petropolis <- st_read(file.path(DIR_BASE, "cicatrizes/Cicatriz_Pet_2011_UTM.shp"), quiet = TRUE) %>%
  mutate(name_muni = "Petropolis") %>%
  select(name_muni, geometry) %>%
  st_make_valid()

teresopolis <- st_read(file.path(DIR_BASE, "cicatrizes/Cicatriz_Ter_2011_UTM.shp"), quiet = TRUE) %>%
  mutate(name_muni = "Teresopolis") %>%
  select(name_muni, geometry) %>%
  st_make_valid() %>%
  st_transform(st_crs(petropolis))

nova_friburgo <- st_read(file.path(DIR_BASE, "cicatrizes/Cicatriz_Nov_2011_UTM.shp"), quiet = TRUE) %>%
  mutate(name_muni = "Nova_Friburgo") %>%
  select(name_muni, geometry) %>%
  st_make_valid() %>%
  st_transform(st_crs(petropolis))

pontos_desastres <- rbind(petropolis, teresopolis, nova_friburgo) %>%
  st_make_valid() %>%
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
geo_vars <- geocode_sf %>%
  st_drop_geometry() %>%
  select(code_inep, min_dist, all_of(raios_names), starts_with("raio_")) %>%
  distinct(code_inep, .keep_all = TRUE)

dados_escolas_final <- dados_escolas2 %>%
  left_join(geo_vars, by = "code_inep")

# ============================================================
# 7.05) BALANCEAMENTO BRUTO (SEM BURACOS) — antes do census tract
# - cria anos faltantes entre min e max de cada escola
# - preenche tudo copiando ANO SEGUINTE e depois anterior
# ============================================================

dados_escolas_final <- dados_escolas_final %>%
  arrange(code_inep, ano) %>%
  group_by(code_inep, ano) %>%
  summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")

dados_escolas_final <- dados_escolas_final %>%
  group_by(code_inep) %>%
  tidyr::complete(ano = seq(min(ano, na.rm = TRUE), max(ano, na.rm = TRUE), by = 1L)) %>%
  arrange(code_inep, ano) %>%
  tidyr::fill(-code_inep, -ano, .direction = "updown") %>%
  ungroup() %>%
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

census_geo <- geobr::read_census_tract(code_tract = "RJ", year = 2010) %>%
  select(code_tract, zone, geom) %>%
  mutate(code_tract = as.character(code_tract)) %>%
  left_join(census_df, by = "code_tract")

census_geo <- st_transform(census_geo, st_crs(geocode_sf))

schools_census_sf <- st_join(geocode_sf, census_geo, join = st_nearest_feature, left = TRUE)

schools_census_vars <- schools_census_sf %>%
  st_drop_geometry() %>%
  select(
    code_inep, zone,
    pop_branca, income_total, pop_per_household,
    households_total, pop_water_network, pop_total,
    urban, favela, men
  ) %>%
  distinct(code_inep, .keep_all = TRUE)

dados_escolas_final <- dados_escolas_final %>%
  left_join(schools_census_vars, by = "code_inep")

# ============================================================
# 7.2) FECHAMENTO com 2 colunas + NA em tudo no período fechado
# ============================================================

ref_fech <- dados_escolas_final %>%
  group_by(code_inep) %>%
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

dados_escolas_final <- dados_escolas_final %>%
  left_join(ref_fech %>% select(code_inep, ano_primeiro_fechado, ultimo_ano_aberto), by = "code_inep") %>%
  mutate(
    fechamento_anos_fechado  = if_else(fechamento_escola == 1L, 1L, 0L),
    fechamento_ultimo_aberto = if_else(ano == ultimo_ano_aberto, 1L, 0L),
    fechamento_ultimo_aberto = if_else(fechamento_escola == 1L, NA_integer_, fechamento_ultimo_aberto)
  ) %>%
  select(-ano_primeiro_fechado, -ultimo_ano_aberto)

keep_cols <- c(
  "code_inep", "ano",
  "tp_situacao_funcionamento", "fechamento_escola",
  "fechamento_ultimo_aberto", "fechamento_anos_fechado"
)
cols_to_na <- setdiff(names(dados_escolas_final), keep_cols)

dados_escolas_final <- dados_escolas_final %>%
  mutate(across(all_of(cols_to_na), ~ if_else(fechamento_escola == 1L, NA, .x)))

# ============================================================
# 7.3) Derivadas finais
# ============================================================

dados_escolas_final <- dados_escolas_final %>%
  mutate(
    fechamento           = fechamento_escola,
    log_docente          = log(as.numeric(n_docentes_total) + 1),
    log_num_funcionarios = log(as.numeric(qt_salas_utilizadas) + 1),
    log_aluno            = log(as.numeric(n_alunos_total) + 1)
  )

# ============================================================
# CHECKS: agora não pode ter buracos
# ============================================================

gaps_resumo <- dados_escolas_final %>%
  distinct(code_inep, ano) %>%
  group_by(code_inep) %>%
  summarise(
    ano_min = min(ano, na.rm = TRUE),
    ano_max = max(ano, na.rm = TRUE),
    n_obs   = n(),
    n_esp   = ano_max - ano_min + 1L,
    tem_buraco = n_obs < n_esp,
    .groups = "drop"
  ) %>%
  summarise(escolas_com_buraco = sum(tem_buraco), .groups = "drop")

##

## Variavel de tratamento
dados_escolas_final  <- dados_escolas_final  %>% 
  filter(ano >= 2007) %>%
  filter(ano < 2020) %>%
  mutate(treat      = ifelse(raio == 1 & ano >= 2011,1,0),
         treat_unid = ifelse(raio == 1,1,0),
         treat_1yr  = ifelse(raio == 1 & ano == 2011,1,0),
         treat_2yr  = ifelse(raio == 1 & ano == 2012,1,0),
         treat_3yr  = ifelse(raio == 1 & ano == 2013,1,0),
         treat_4yr  = ifelse(raio == 1 & ano == 2014,1,0),
         treat_5yr  = ifelse(raio == 1 & ano == 2015,1,0),
         treat_6yr  = ifelse(raio == 1 & ano == 2016,1,0),
         treat_7yr  = ifelse(raio == 1 & ano == 2017,1,0),
         treat_8yr  = ifelse(raio == 1 & ano == 2018,1,0),
         treat_9yr  = ifelse(raio == 1 & ano == 2019,1,0)
  )


# ============================================================
# 8) Salvar
# ============================================================
arrow::write_parquet(dados_escolas_final, "./output/painel_escolas.parquet")
