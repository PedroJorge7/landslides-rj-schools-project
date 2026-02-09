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
    'A. School Closure (0/1)',
    'B. Log of Number of Teachers',
    'C. Log of Number of Class',
    'D. Log of Number of Staff'
  )
  
  
  # label <- c(
  #   "Fechamento da Escola",
  #   "Logaritmo do Número de Docentes",
  #   "Logaritmo do Número de Salas Existentes",
  #   "Logaritmo do Número de Funcionários"
  # )
  
  
  # Load dataset
  df <- arrow::read_parquet('./output/painel_escolas.parquet') %>% 
    filter(ano <= 2015) |> 
    # filter(sum(n_alunos_total, na.rm = T) != 0) |> 
    filter(raio == 1 | data.table::between(min_dist,20,30))
  
  
  base_line <- 2010
  df$period <- df$ano - 2011 + 1
  
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
  
  
  ## Gráfico event study ----------------------------
  
  
  # Aplicando para gerar os resultados
  output <- do.call(rbind, lapply(outcomes_principais, function(feature) {
      process_plot_data(df = df, feature, type = "event_study")
  }))
  
  
  output <- output %>% tidyr::fill(Regression, .direction = 'downup')
  
  # Criar múltiplos gráficos para `event_study`
  event_study_plots <- lapply(unique(output$Regression), plot_event_study)
  cowplot::plot_grid(plotlist = event_study_plots, ncol = 2)
  
  # Salvar o gráfico
  ggsave(filename  = './results/event_study.jpg',
         dpi=300, width = 40, height = 20, units='cm')
  
