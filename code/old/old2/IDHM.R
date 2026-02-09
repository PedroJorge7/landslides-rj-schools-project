

df <- abjData::pnud_muni
arrow::write_parquet(df,'idhm.parquet')
