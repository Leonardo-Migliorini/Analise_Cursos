# Carregando dados -------------------------------------------------------------

TB_AUX_CINE_BRASIL_2019 <- arrow::read_parquet(
  "Dados//TB_AUX_CINE_BRASIL_2019.parquet"
  )

# Escolhendo curso(s) pela área CINE
CINE <- TB_AUX_CINE_BRASIL_2019 |> 
  dplyr::filter( # Escolhendo área geral
    NO_CINE_AREA_GERAL == "Engenharia, produção e construção"
  ) |> 
  dplyr::select(
    -c(NO_CINE_AREA_GERAL, CO_CINE_AREA_GERAL)
  ) |> 
  dplyr::filter( # Escolhendo área especifica
    NO_CINE_AREA_ESPECIFICA == "Engenharia e profissões correlatas"
  ) |> 
  dplyr::select(
    -c(NO_CINE_AREA_ESPECIFICA, CO_CINE_AREA_ESPECIFICA)
  )

table(CINE$NO_CINE_AREA_ESPECIFICA)