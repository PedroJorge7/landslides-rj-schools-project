library(fixest)
library(dplyr)
library(plm)
library(broom)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(writexl)

rm(list = ls())


####### FUNCTIONS
source('./code/00_functions.R')


outcomes_principais     <- c('fechamento','log_docente',
                             'log_salas','log_num_funcionarios')

label <- c(
  "A. School Closure (0/1)",
  "B. Log of Number of Teachers",
  "C. Log of Number of Class",
  "D. Log of Number of Staff"
)


# label <- c(
#   "Fechamento da Escola",
#   "Logaritmo do Número de Docentes",
#   "Logaritmo do Número de Salas Existentes",
#   "Logaritmo do Número de Funcionários"
# )



# Load dataset
df <- arrow::read_parquet('./output/painel_escolas.parquet') %>% 
  arrange(code_inep, ano) |> 
  group_by(code_inep) |> 
  filter(ano <= 2015) |> 
  filter(raio == 1 | data.table::between(min_dist,25,30))

# REGRESSÃO

df$pop_branca <- df$pop_branca*df$ano
df$income_total <- df$income_total*df$ano
df$pop_per_household <- df$pop_per_household*df$ano
df$pop_total <- df$pop_total*df$ano
df$urban <- df$urban*df$ano
df$favela <- df$favela*df$ano

controles <- c("income_total","pop_per_household","pop_branca","urban","favela")

# Remover até 20 (até 30km)
# renda media e escolaridade
# fechamento. Alunos fundamental = 0 e existe fundamental

# Heterogeneidade: Regressões Fundamental/Médio/Ambos ----------------------------

output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
  bind_rows(
    process_plot_data(df = subset(df,is_fundamental == 1 & is_medio == 0), feature, type = "time_effect") %>% mutate(tipo = 'Apenas Fundamental'),
    process_plot_data(df = subset(df,is_fundamental == 0 & is_medio == 1), feature, type = "time_effect") %>% mutate(tipo = 'Apenas Médio'),
    process_plot_data(df = subset(df,is_fundamental == 1 & is_medio == 1), feature, type = "time_effect") %>% mutate(tipo = 'Ambos'),
  )
}))

output$term <- output$term + 2010

# Usando cowplot para arranjar os gráficos
lapply(unique(output$Regression), plot_event_time) %>% 
  ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
                    common.legend = TRUE, legend = "bottom")


# Salvando o gráfico
ggsave(filename  = './results/etapa_ensino.jpg',
       dpi = 300, width = 30, height = 15, units = 'cm')