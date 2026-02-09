library(dplyr)
library(tidyr)
library(broom)

# Adicione uma coluna de período ao dataframe

df_balanceado <- readRDS('./data/df_final.rds') %>% 
  filter(ano <= 2015) %>% 
  #filter(ano %in% c(2010,2011)) %>% 
  mutate(period = ifelse(ano <= 2020, "Total", "pos_treatment"),
         #period = ifelse(ano <= 2010, "pre_treatment", "pos_treatment"),
         treat_ds = ifelse(treat_unid_rj == 1, "Afetado", "Não Afetado")) %>% 
  filter(raio_otimo_rj == 1 | data.table::between(min_dist_rj,20,30))

outcomes_principais     <- c('fechamento','docente','aluno','num_funcionarios',
                             "income_total","pop_total",
                             "pop_per_household",
                             "urban","favela","pop_water_network")




# Função para adicionar estrelas de significância
add_significance_stars <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Lista para armazenar os resultados
resultados_t_test <- list()

# Loop para cada variável em outcomes_principais
for (outcome in outcomes_principais) {
  
  # Realizar o teste para cada período
  for (p in unique(df_balanceado$period)) {
    
    # Filtrar dados pelo período
    data_period <- df_balanceado %>% filter(period == p)
    
    # Filtrar valores por grupo
    valores_afetado <- data_period %>% filter(treat_ds == "Afetado") %>% pull(!!sym(outcome))
    valores_nao_afetado <- data_period %>% filter(treat_ds == "Não Afetado") %>% pull(!!sym(outcome))
    
    # Calcular as médias e desvios padrão
    mean_afetado <- mean(valores_afetado, na.rm = TRUE)
    sd_afetado <- sd(valores_afetado, na.rm = TRUE)
    
    mean_nao_afetado <- mean(valores_nao_afetado, na.rm = TRUE)
    sd_nao_afetado <- sd(valores_nao_afetado, na.rm = TRUE)
    
    # Diferença de médias
    diff_mean <- mean_afetado - mean_nao_afetado
    
    # Teste t para comparar médias entre Afetado e Não Afetado
    t_test <- t.test(valores_afetado, valores_nao_afetado)
    
    # Adicionar estrelas de significância ao diff_mean
    significance <- add_significance_stars(t_test$p.value)
    diff_mean_with_stars <- paste0(round(diff_mean, 3), significance)
    
    # Formatando média e erro padrão: "média\n(sd = ...)"
    formatted_mean_afetado <- paste0(round(mean_afetado, 3), "\n(sd = ", round(sd_afetado, 3), ")")
    formatted_mean_nao_afetado <- paste0(round(mean_nao_afetado, 3), "\n(sd = ", round(sd_nao_afetado, 3), ")")
    
    # Armazenar os resultados
    resultados_t_test[[paste(outcome, p, sep = "_")]] <- list(
      outcome = outcome,
      period = p,
      mean_afetado = formatted_mean_afetado,
      mean_nao_afetado = formatted_mean_nao_afetado,
      diff_mean = diff_mean_with_stars
    )
  }
}

# Transformar os resultados em um data frame para visualização
df_resultados <- do.call(rbind, lapply(resultados_t_test, as.data.frame)) %>% 
  pivot_wider(
    names_from = 'period', 
    values_from = c('mean_afetado','mean_nao_afetado','diff_mean')
  ) %>% 
  select(c(outcome, ends_with('Total'),ends_with('pre_treatment'), ends_with('pos_treatment')))

# Exibir os resultados
print(df_resultados)
xlsx::write.xlsx(df_resultados,"./output/infraestrutura/teste_diferenca_medias.xlsx")

outcomes_principais     <- c('aprov_media','reprov_media','aband_media','ideb_media')
# Transformar os resultados em um data frame para visualização
df_resultados <- do.call(rbind, lapply(resultados_t_test, as.data.frame)) %>% 
  pivot_wider(
    names_from = 'period', 
    values_from = c('mean_afetado','mean_nao_afetado','diff_mean')
  ) %>% 
  select(c(outcome, ends_with('pre_treatment'), ends_with('pos_treatment')))

# Exibir os resultados
print(df_resultados)
xlsx::write.xlsx(df_resultados,"./output/rendimento/teste_diferenca_medias.xlsx")