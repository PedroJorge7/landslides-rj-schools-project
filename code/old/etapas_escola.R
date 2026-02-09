

list.files <- list.files(path = "./temp/", full.names = T, pattern = "*.csv")
df <- lapply(list.files,data.table::fread, select = c('NU_ANO_CENSO','CO_ENTIDADE',
                                                      "IN_FUND_AI","IN_FUND_AF",
                                                      'IN_FUND','IN_MED'))
df <- dplyr::bind_rows(df)
names(df) <- c('ano','pk_cod_entidade','in_fun','in_med')

saveRDS(df,'etapas_escola.rds')
haven::write_dta(df,'etapas_escola.dta')
