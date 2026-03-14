library(arrow)
library(data.table)
library(dplyr)

rm(list = ls())

# ----------------------------
# Paths
# ----------------------------
DIR_DOCENTES <- "G:/.shortcut-targets-by-id/1K-TPEsFyx_miIiVaxXX3sKwYBAxHWeUj/Natural Disasters and Educational outcomes  Evidence from the 2011 Rio de Janeiro Landslides/input/docentes"
DIR_OUT <- "./output"
OUT_FILE <- file.path(DIR_OUT, "docentes_escola_ano.parquet")

# Opcional: limite os anos se ainda estiver convertendo os parquets
YEARS_TO_PROCESS <- NULL

# ----------------------------
# Helpers
# ----------------------------
first_existing <- function(candidates, available) {
  found <- candidates[candidates %in% available]
  if (length(found) == 0) return(NA_character_)
  found[1]
}

normalize_text <- function(x) {
  out <- trimws(as.character(x))
  out[out %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  out
}

id_to_char <- function(x) {
  if (inherits(x, "integer64")) {
    out <- as.character(x)
  } else if (is.numeric(x)) {
    out <- format(x, scientific = FALSE, trim = TRUE)
  } else {
    out <- as.character(x)
  }

  out <- trimws(out)
  out[out %in% c("", "NA", "NaN")] <- NA_character_
  out
}

num_or_na <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

binary_from_any <- function(dt, candidates, yes_values = c("1", "S", "SIM", "TRUE", "T", "Y")) {
  cols <- intersect(candidates, names(dt))
  if (length(cols) == 0) {
    return(rep(NA_integer_, nrow(dt)))
  }

  mats <- lapply(cols, function(col_name) normalize_text(dt[[col_name]]))
  observed <- Reduce(`|`, lapply(mats, function(x) !is.na(x)))
  yes <- Reduce(`|`, lapply(mats, function(x) !is.na(x) & x %in% yes_values))

  out <- ifelse(!observed, NA_integer_, ifelse(yes, 1L, 0L))
  as.integer(out)
}

combine_binary_flags <- function(...) {
  mats <- list(...)
  observed <- Reduce(`|`, lapply(mats, function(x) !is.na(x)))
  yes <- Reduce(`|`, lapply(mats, function(x) !is.na(x) & x == 1L))
  out <- ifelse(!observed, NA_integer_, ifelse(yes, 1L, 0L))
  as.integer(out)
}

collapse_binary <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  as.integer(any(x == 1L))
}

first_non_missing_num <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[1]
}

sum_flag <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(sum(x == 1L, na.rm = TRUE))
}

pct_flag <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(100 * mean(x == 1L))
}

mean_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(mean(x))
}

median_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(median(x))
}

sd_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  as.numeric(sd(x))
}

# ----------------------------
# Dicionario de flags comparaveis ao longo do tempo
# ----------------------------
FORMACAO_FLAGS <- list(
  docentes_licenciatura = c(
    "id_licenciatura_1", "id_licenciatura_2", "id_licenciatura_3",
    "in_licenciatura_1", "in_licenciatura_2", "in_licenciatura_3"
  ),
  docentes_complementacao_pedagogica = c(
    "id_com_pedagogica_1", "id_com_pedagogica_2", "id_com_pedagogica_3",
    "in_complementacao_pedagogica"
  ),
  docentes_especializacao = c("id_especializacao", "in_especializacao"),
  docentes_mestrado = c("id_mestrado", "in_mestrado"),
  docentes_doutorado = c("id_doutorado", "in_doutorado"),
  docentes_sem_pos_graduacao = c("id_pos_graduacao_nenhum", "in_pos_nenhum")
)

FORMACAO_ESPECIFICA_FLAGS <- list(
  docentes_formacao_creche = c("id_especifico_creche", "in_especifico_creche"),
  docentes_formacao_pre_escola = c("id_especifico_pre_escola", "in_especifico_pre_escola"),
  docentes_formacao_anos_iniciais = c("id_especifico_anos_iniciais", "in_especifico_anos_iniciais"),
  docentes_formacao_anos_finais = c("id_especifico_anos_finais", "in_especifico_anos_finais"),
  docentes_formacao_ens_medio = c("id_especifico_ens_medio", "in_especifico_ens_medio"),
  docentes_formacao_eja = c("id_especifico_eja", "in_especifico_eja"),
  docentes_formacao_educ_especial = c("id_especifico_nec_esp", "in_especifico_ed_especial"),
  docentes_formacao_ed_indigena = c("id_especifico_ed_indigena", "in_especifico_ed_indigena"),
  docentes_formacao_campo = c("id_especifico_campo", "in_especifico_campo"),
  docentes_formacao_ambiental = c("id_especifico_ambiental", "in_especifico_ambiental"),
  docentes_formacao_direitos_humanos = c("id_especifico_dir_humanos", "in_especifico_dir_humanos"),
  docentes_formacao_diversidade_sexual = c("id_especifico_div_sexual", "in_especifico_div_sexual"),
  docentes_formacao_direito_adolescente = c("id_especifico_dir_adolesc", "in_especifico_dir_adolesc"),
  docentes_formacao_afro = c("id_especifico_afro", "in_especifico_afro"),
  docentes_formacao_gestao = c("in_especifico_gestao"),
  docentes_formacao_intercultural_outros = c("id_intercultural_outros"),
  docentes_formacao_especifica_outros = c("id_especifico_outros", "in_especifico_outros"),
  docentes_formacao_especifica_nenhuma = c("id_especifico_nenhum", "in_especifico_nenhum")
)

DISCIPLINA_FLAGS <- list(
  docentes_disc_portugues = c("id_lingua_literat_portuguesa", "in_disc_lingua_portuguesa"),
  docentes_disc_ingles = c("id_lingua_literat_ingles", "in_disc_lingua_ingles"),
  docentes_disc_espanhol = c("id_lingua_literat_espanhol", "in_disc_lingua_espanhol"),
  docentes_disc_frances = c("in_disc_lingua_frances"),
  docentes_disc_outra_lingua = c("id_lingua_literat_outra", "in_disc_lingua_outra"),
  docentes_disc_lingua_indigena = c("id_lingua_literat_indigena", "in_disc_lingua_indigena"),
  docentes_disc_port_segunda_lingua = c("in_disc_port_segunda_lingua"),
  docentes_disc_matematica = c("id_matematica", "in_disc_matematica"),
  docentes_disc_ciencias = c("id_ciencias", "in_disc_ciencias"),
  docentes_disc_fisica = c("id_fisica", "in_disc_fisica"),
  docentes_disc_quimica = c("id_quimica", "in_disc_quimica"),
  docentes_disc_biologia = c("id_biologia", "in_disc_biologia"),
  docentes_disc_artes = c("id_artes", "in_disc_artes"),
  docentes_disc_educacao_fisica = c("id_educacao_fisica", "in_disc_educacao_fisica"),
  docentes_disc_historia = c("id_historia", "in_disc_historia"),
  docentes_disc_geografia = c("id_geografia", "in_disc_geografia"),
  docentes_disc_filosofia = c("id_filosofia", "in_disc_filosofia"),
  docentes_disc_sociologia = c("id_sociologia", "in_disc_sociologia"),
  docentes_disc_estudos_sociais = c(
    "id_estudos_sociais", "in_disc_estudos_sociais", "in_disc_est_sociais_sociologia"
  ),
  docentes_disc_informatica = c("id_informatica_computacao", "in_disc_informatica_computacao"),
  docentes_disc_ensino_religioso = c("id_ensino_religioso", "in_disc_ensino_religioso"),
  docentes_disc_profissionalizante = c("id_profissionalizante", "in_disc_profissionalizante"),
  docentes_disc_libras = c("id_libras", "in_disc_libras"),
  docentes_disc_pedagogicas = c("id_disciplinas_pedag", "in_disc_pedagogicas"),
  docentes_disc_atendimento_especial = c("id_disc_atendimento_especiais"),
  docentes_disc_diversidade_sociocultural = c("id_disc_diversidade_socio_cult"),
  docentes_disc_estagio_supervisionado = c("in_disc_estagio_supervisionado"),
  docentes_disc_outras = c("id_outras_disciplinas", "in_disc_outras")
)

NEE_FLAGS <- list(
  docentes_nec_especial = c("id_possui_nec_especial", "in_necessidade_especial"),
  docentes_baixa_visao = c("id_baixa_visao", "in_baixa_visao"),
  docentes_cegueira = c("id_cegueira", "in_cegueira"),
  docentes_surdez = c("id_surdez", "in_surdez"),
  docentes_def_auditiva = c("id_def_auditiva", "in_def_auditiva"),
  docentes_surdocegueira = c("id_surdocegueira", "in_surdocegueira"),
  docentes_def_fisica = c("id_def_fisica", "in_def_fisica"),
  docentes_def_intelectual = c("id_def_intelectual", "in_def_intelectual"),
  docentes_def_multipla = c("id_def_multipla", "in_def_multipla"),
  docentes_autismo = c("in_autismo"),
  docentes_superdotacao = c("in_superdotacao")
)

ALL_FLAG_MAPS <- c(
  FORMACAO_FLAGS,
  FORMACAO_ESPECIFICA_FLAGS,
  DISCIPLINA_FLAGS,
  NEE_FLAGS
)

DOCENTE_BASE_FLAGS <- c(
  "docentes_mulheres", "docentes_homens",
  "docentes_brancos", "docentes_pretos", "docentes_pardos",
  "docentes_amarelos", "docentes_indigenas", "docentes_cor_nao_declarada",
  "docentes_negros",
  "docentes_ate_29", "docentes_30_39", "docentes_40_49",
  "docentes_50_59", "docentes_60_mais",
  "docentes_pos_graduacao", "docentes_formacao_pedagogica",
  "docentes_pos_stricto_sensu",
  "docentes_efetivos", "docentes_temporarios",
  "docentes_terceirizados", "docentes_clt"
)

DOCENTE_PARTIAL_FLAG_NAMES <- unique(c(DOCENTE_BASE_FLAGS, names(ALL_FLAG_MAPS)))

empty_docente_partial_df <- function() {
  out <- data.frame(
    ano = integer(),
    code_inep = character(),
    id_docente = character(),
    idade = numeric(),
    n_registros_docente = integer(),
    check.names = FALSE
  )

  for (nm in DOCENTE_PARTIAL_FLAG_NAMES) {
    out[[nm]] <- integer()
  }

  out
}

DOCENTE_PARTIAL_SCHEMA <- do.call(
  arrow::schema,
  c(
    list(
      ano = arrow::int32(),
      code_inep = arrow::utf8(),
      id_docente = arrow::utf8(),
      idade = arrow::float64(),
      n_registros_docente = arrow::int32()
    ),
    setNames(rep(list(arrow::int32()), length(DOCENTE_PARTIAL_FLAG_NAMES)), DOCENTE_PARTIAL_FLAG_NAMES)
  )
)


# ----------------------------
# Batch -> docente-escola-ano parcial
# ----------------------------
process_docente_batch <- function(
    batch,
    year_col,
    school_col,
    teacher_col,
    state_col,
    age_col,
    sex_col,
    race_col,
    contract_col
) {
  dt <- as.data.table(as.data.frame(batch))

  if (nrow(dt) == 0) {
    return(empty_docente_partial_df())
  }

  dt[, ano := as.integer(num_or_na(dt[[year_col]]))]
  dt[, code_inep := id_to_char(dt[[school_col]])]
  dt[, id_docente := id_to_char(dt[[teacher_col]])]

  if (!is.na(state_col) && state_col %in% names(dt)) {
    dt[, uf_docente := id_to_char(dt[[state_col]])]
    dt <- dt[uf_docente == "33" | substr(code_inep, 1, 2) == "33"]
  } else {
    dt <- dt[substr(code_inep, 1, 2) == "33"]
  }

  dt <- dt[!is.na(ano) & !is.na(code_inep) & !is.na(id_docente)]

  if (nrow(dt) == 0) {
    return(empty_docente_partial_df())
  }

  dt[, idade := if (!is.na(age_col) && age_col %in% names(dt)) num_or_na(dt[[age_col]]) else NA_real_]

  if (!is.na(sex_col) && sex_col %in% names(dt)) {
    sexo_raw <- normalize_text(dt[[sex_col]])
    dt[, docentes_mulheres := ifelse(is.na(sexo_raw), NA_integer_, as.integer(sexo_raw %in% c("F", "2")))]
    dt[, docentes_homens := ifelse(is.na(sexo_raw), NA_integer_, as.integer(sexo_raw %in% c("M", "1")))]
  } else {
    dt[, docentes_mulheres := NA_integer_]
    dt[, docentes_homens := NA_integer_]
  }

  if (!is.na(race_col) && race_col %in% names(dt)) {
    raca_raw <- normalize_text(dt[[race_col]])
    dt[, docentes_brancos := ifelse(is.na(raca_raw), NA_integer_, as.integer(raca_raw == "1"))]
    dt[, docentes_pretos := ifelse(is.na(raca_raw), NA_integer_, as.integer(raca_raw == "2"))]
    dt[, docentes_pardos := ifelse(is.na(raca_raw), NA_integer_, as.integer(raca_raw == "3"))]
    dt[, docentes_amarelos := ifelse(is.na(raca_raw), NA_integer_, as.integer(raca_raw == "4"))]
    dt[, docentes_indigenas := ifelse(is.na(raca_raw), NA_integer_, as.integer(raca_raw == "5"))]
    dt[, docentes_cor_nao_declarada := ifelse(is.na(raca_raw), NA_integer_, as.integer(raca_raw == "0"))]
  } else {
    dt[, c(
      "docentes_brancos", "docentes_pretos", "docentes_pardos",
      "docentes_amarelos", "docentes_indigenas", "docentes_cor_nao_declarada"
    ) := lapply(1:6, function(...) NA_integer_)]
  }

  dt[, docentes_negros := combine_binary_flags(docentes_pretos, docentes_pardos)]

  dt[, docentes_ate_29 := ifelse(is.na(idade), NA_integer_, as.integer(idade <= 29))]
  dt[, docentes_30_39 := ifelse(is.na(idade), NA_integer_, as.integer(idade >= 30 & idade <= 39))]
  dt[, docentes_40_49 := ifelse(is.na(idade), NA_integer_, as.integer(idade >= 40 & idade <= 49))]
  dt[, docentes_50_59 := ifelse(is.na(idade), NA_integer_, as.integer(idade >= 50 & idade <= 59))]
  dt[, docentes_60_mais := ifelse(is.na(idade), NA_integer_, as.integer(idade >= 60))]

  for (nm in names(ALL_FLAG_MAPS)) {
    dt[, (nm) := binary_from_any(dt, ALL_FLAG_MAPS[[nm]])]
  }

  dt[, docentes_pos_graduacao := combine_binary_flags(
    docentes_especializacao,
    docentes_mestrado,
    docentes_doutorado,
    if ("docentes_sem_pos_graduacao" %in% names(dt)) 1L - docentes_sem_pos_graduacao else NA_integer_
  )]

  dt[, docentes_formacao_pedagogica := combine_binary_flags(
    docentes_licenciatura,
    docentes_complementacao_pedagogica
  )]

  dt[, docentes_pos_stricto_sensu := combine_binary_flags(
    docentes_mestrado,
    docentes_doutorado
  )]

  if (!is.na(contract_col) && contract_col %in% names(dt)) {
    contrato_raw <- normalize_text(dt[[contract_col]])
    dt[, docentes_efetivos := ifelse(is.na(contrato_raw), NA_integer_, as.integer(contrato_raw == "1"))]
    dt[, docentes_temporarios := ifelse(is.na(contrato_raw), NA_integer_, as.integer(contrato_raw == "2"))]
    dt[, docentes_terceirizados := ifelse(is.na(contrato_raw), NA_integer_, as.integer(contrato_raw == "3"))]
    dt[, docentes_clt := ifelse(is.na(contrato_raw), NA_integer_, as.integer(contrato_raw == "4"))]
  } else {
    dt[, c("docentes_efetivos", "docentes_temporarios", "docentes_terceirizados", "docentes_clt") := lapply(1:4, function(...) NA_integer_)]
  }

  flag_cols <- DOCENTE_PARTIAL_FLAG_NAMES

  dt_partial <- dt[
    ,
    c(
      list(
        idade = first_non_missing_num(idade),
        n_registros_docente = .N
      ),
      setNames(lapply(.SD, collapse_binary), flag_cols)
    ),
    by = .(ano, code_inep, id_docente),
    .SDcols = flag_cols
  ]

  as.data.frame(dt_partial)
}

# ----------------------------
# Arquivo -> escola-ano agregado
# ----------------------------
process_docente_file <- function(path_file) {
  file_name <- basename(path_file)
  message(sprintf("[docentes] lendo %s", file_name))

  ds <- open_dataset(sources = path_file, format = "parquet")
  available <- names(ds)

  year_col <- first_existing(c("ano", "ano_censo", "nu_ano_censo"), available)
  school_col <- first_existing(c("pk_cod_entidade", "co_entidade"), available)
  teacher_col <- first_existing(c("fk_cod_docente", "id_docente"), available)
  state_col <- first_existing(c("fk_cod_estado", "co_uf"), available)
  age_col <- first_existing(c("num_idade", "nu_idade"), available)
  sex_col <- first_existing(c("tp_sexo"), available)
  race_col <- first_existing(c("tp_cor_raca"), available)
  contract_col <- first_existing(c("id_tipo_contratacao", "tp_tipo_contratacao"), available)

  if (any(is.na(c(year_col, school_col, teacher_col)))) {
    warning(sprintf(
      "[docentes] %s ignorado porque nao contem ano/escola/docente.",
      file_name
    ))
    return(NULL)
  }

  selected_cols <- unique(c(
    year_col, school_col, teacher_col, state_col, age_col, sex_col, race_col, contract_col,
    unlist(ALL_FLAG_MAPS, use.names = FALSE)
  ))
  selected_cols <- selected_cols[!is.na(selected_cols)]
  selected_cols <- intersect(selected_cols, available)

  partial_reader <- ds |>
    select(all_of(selected_cols)) |>
    map_batches(
      ~ process_docente_batch(
        batch = .x,
        year_col = year_col,
        school_col = school_col,
        teacher_col = teacher_col,
        state_col = state_col,
        age_col = age_col,
        sex_col = sex_col,
        race_col = race_col,
        contract_col = contract_col
      ),
      .schema = DOCENTE_PARTIAL_SCHEMA,
      .lazy = FALSE
    )

  partial_dt <- as.data.table(partial_reader$read_table())

  if (nrow(partial_dt) == 0) {
    warning(sprintf("[docentes] %s nao gerou observacoes RJ.", file_name))
    return(NULL)
  }

  flag_cols <- names(partial_dt)[grepl("^docentes_", names(partial_dt))]

  docente_escola_ano <- partial_dt[
    ,
    c(
      list(
        idade = first_non_missing_num(idade),
        n_registros_docente = sum(n_registros_docente, na.rm = TRUE)
      ),
      setNames(lapply(.SD, collapse_binary), flag_cols)
    ),
    by = .(ano, code_inep, id_docente),
    .SDcols = flag_cols
  ]

  docente_ano <- docente_escola_ano[
    ,
    .(n_escolas_docente_ano = uniqueN(code_inep)),
    by = .(ano, id_docente)
  ]

  docente_escola_ano <- docente_ano[
    docente_escola_ano,
    on = .(ano, id_docente)
  ]

  docente_escola_ano[, docentes_multiplas_escolas := as.integer(n_escolas_docente_ano > 1L)]
  docente_escola_ano[, docentes_mais_de_um_registro := as.integer(n_registros_docente > 1L)]

  flag_cols <- names(docente_escola_ano)[grepl("^docentes_", names(docente_escola_ano))]

  escola_ano <- docente_escola_ano[
    ,
    c(
      list(
        n_docentes_total = .N,
        n_docentes_total_microdados = .N,
        idade_media_docente = mean_or_na(idade),
        idade_mediana_docente = median_or_na(idade),
        idade_sd_docente = sd_or_na(idade),
        n_registros_docente_media = mean_or_na(n_registros_docente),
        n_registros_docente_mediana = median_or_na(n_registros_docente),
        n_registros_docente_sd = sd_or_na(n_registros_docente),
        n_escolas_docente_ano_media = mean_or_na(n_escolas_docente_ano),
        n_escolas_docente_ano_mediana = median_or_na(n_escolas_docente_ano),
        n_escolas_docente_ano_sd = sd_or_na(n_escolas_docente_ano)
      ),
      setNames(lapply(.SD, sum_flag), paste0("n_", flag_cols)),
      setNames(lapply(.SD, pct_flag), paste0("pct_", flag_cols))
    ),
    by = .(ano, code_inep),
    .SDcols = flag_cols
  ]

  message(sprintf(
    "[docentes] %s -> %s pares escola-ano RJ",
    file_name,
    format(nrow(escola_ano), big.mark = ".", decimal.mark = ",")
  ))

  escola_ano[]
}

# ----------------------------
# Main
# ----------------------------
if (!dir.exists(DIR_OUT)) {
  dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)
}

files <- list.files(
  path = DIR_DOCENTES,
  pattern = "^DOCENTE_[0-9]{4}\\.parquet$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop(sprintf("Nenhum parquet DOCENTE_*.parquet encontrado em '%s'.", DIR_DOCENTES))
}

files <- files[order(files)]

if (!is.null(YEARS_TO_PROCESS)) {
  year_in_file <- as.integer(gsub("^DOCENTE_([0-9]{4})\\.parquet$", "\\1", basename(files)))
  files <- files[year_in_file %in% YEARS_TO_PROCESS]
}

if (length(files) == 0) {
  stop("Nenhum arquivo sobrou apos filtrar YEARS_TO_PROCESS.")
}

result_list <- lapply(files, process_docente_file)
result_list <- Filter(Negate(is.null), result_list)

if (length(result_list) == 0) {
  stop("Nenhum agregado docente-escola-ano foi gerado.")
}

docentes_escola_ano <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
setorder(docentes_escola_ano, code_inep, ano)

arrow::write_parquet(docentes_escola_ano, OUT_FILE)

message(sprintf(
  "[docentes] arquivo final salvo em %s com %s pares escola-ano.",
  OUT_FILE,
  format(nrow(docentes_escola_ano), big.mark = ".", decimal.mark = ",")
))





