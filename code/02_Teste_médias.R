library(arrow)
library(dplyr)
library(tidyr)
library(data.table)
library(openxlsx)

# ----------------------------
# VARIÁVEIS
# ----------------------------
outcomes_principais <- c(
  "fechamento","log_docente","log_salas","log_num_funcionarios",
  "income_total","pop_total","pop_per_household",
  "urban","favela","pop_water_network"
)

# Labels em inglês (ajuste aqui se quiser mudar algum texto)
label_map <- c(
  fechamento           = "School Closure",
  log_docente          = "Log of Number of Teachers",
  log_salas            = "Log of Number of Class",
  log_num_funcionarios = "Log of Number of Staff",
  income_total         = "Total Income",
  pop_total            = "Total Population",
  pop_per_household    = "Population per Household",
  urban                = "Urban",
  favela               = "Favela",
  pop_water_network    = "Population with Water Network"
)

# ----------------------------
# DADOS (pré-tratamento: antes de 2011)
# ----------------------------
df_pre <- arrow::read_parquet("./output/painel_escolas.parquet") %>%
  filter(raio == 1 | data.table::between(min_dist, 20, 30)) %>%
  mutate(grupo = ifelse(raio == 1, "Treatment", "Control")) %>%
  filter(ano < 2011)

# Observations (linhas escola-ano)
n_all  <- nrow(df_pre)
n_ctrl <- nrow(filter(df_pre, grupo == "Control"))
n_trt  <- nrow(filter(df_pre, grupo == "Treatment"))

# ----------------------------
# Estatísticas: mean e sd
# ----------------------------
long_pre <- df_pre %>%
  select(grupo, all_of(outcomes_principais)) %>%
  pivot_longer(all_of(outcomes_principais),
               names_to = "outcome",
               values_to = "value")

stats_groups <- long_pre %>%
  group_by(outcome, grupo) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    .groups = "drop"
  )

stats_all <- long_pre %>%
  group_by(outcome) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(grupo = "All schools")

stats <- bind_rows(stats_all, stats_groups)

# Formatadores
fmt_mean <- function(x) sprintf("%.3f", x)
fmt_sd   <- function(x) paste0("(", sprintf("%.3f", x), ")")

# Tabelas wide: mean e sd (separadas)
tab_mean <- stats %>%
  mutate(value = fmt_mean(mean)) %>%
  select(outcome, grupo, value) %>%
  pivot_wider(names_from = grupo, values_from = value)

tab_sd <- stats %>%
  mutate(value = fmt_sd(sd)) %>%
  select(outcome, grupo, value) %>%
  pivot_wider(names_from = grupo, values_from = value)

# Garante ordem das variáveis e aplica label
ordem <- outcomes_principais

tab_mean <- tab_mean %>%
  mutate(outcome = factor(outcome, levels = ordem)) %>%
  arrange(outcome) %>%
  mutate(Variable = unname(label_map[as.character(outcome)])) %>%
  select(Variable, `All schools`, Control, Treatment)

tab_sd <- tab_sd %>%
  mutate(outcome = factor(outcome, levels = ordem)) %>%
  arrange(outcome) %>%
  mutate(Variable = "") %>%  # linha abaixo sem repetir label
  select(Variable, `All schools`, Control, Treatment)

# Intercala: mean row + sd row
tabela_final <- bind_rows(
  do.call(rbind, lapply(seq_len(nrow(tab_mean)), function(i) {
    bind_rows(tab_mean[i, ], tab_sd[i, ])
  }))
)

# Linha final: Observations (1x por coluna)
tabela_final <- bind_rows(
  tabela_final,
  tibble(
    Variable     = "Observations",
    `All schools` = as.character(n_all),
    Control       = as.character(n_ctrl),
    Treatment     = as.character(n_trt)
  )
)

print(tabela_final)

openxlsx::write.xlsx(
  tabela_final,
  file = "./results/balance_pre_2011.xlsx",
  overwrite = TRUE
)
