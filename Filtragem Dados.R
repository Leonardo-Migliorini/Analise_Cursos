# Carregando dados -------------------------------------------------------------

TB_AUX_CINE_BRASIL_2019 <- arrow::read_parquet(
  "Dados//TB_AUX_CINE_BRASIL_2019.parquet"
  )

ingressantes <- arrow::read_parquet("Dados//ingressantes.parquet") |>
  dplyr::select( # selecionando as variáveis de maior interesse
    2:20
  )

DM_CURSO2019 <- arrow::read_parquet("Dados//DM_CURSO2019.parquet") |> 
  dplyr::select( # selecionando as variáveis de maior interesse
    c(2,5:10,12,16:18)
  )

# Filtrando dados --------------------------------------------------------------

# Escolhendo curso(s) pela área CINE
CINE <- TB_AUX_CINE_BRASIL_2019 |> 
  dplyr::filter( # Escolhendo área geral
    NO_CINE_AREA_GERAL == "Engenharia, produção e construção"
  ) |> 
  dplyr::select(
    -c(NO_CINE_AREA_GERAL, CO_CINE_AREA_GERAL)
  ) |> 
  dplyr::filter( # Escolhendo área específica
    NO_CINE_AREA_ESPECIFICA == "Engenharia e profissões correlatas"
  ) |> 
  dplyr::select( 
    -c(NO_CINE_AREA_ESPECIFICA, CO_CINE_AREA_ESPECIFICA)
  ) |> 
  dplyr::filter( # Escolhendo área detalhada
    NO_CINE_AREA_DETALHADA == "Eletricidade e energia"
  )

table(CINE$NO_CINE_AREA_DETALHADA)
# CURSOS ESCOLHIDOS: Engenharia Elétrica / CO_CINE_ROTULO = 0713E05



# Filtrando o banco de dados dos ingressantes

ingressantes <- ingressantes |> 
  dplyr::mutate(
    TMP_DESFECHO = 2019 - NU_ANO_CENSO, # Calculando tempo de desfecho
    TP_SITUACAO = dplyr::case_when(
      TP_SITUACAO == 6 ~ 1, # 1 indica conclusão (desfecho de interesse)
      TRUE ~ 0 # 0 indica censura por: encerramento do período de acompanhamento,
               # tracamento, desistência ou transferência
    )
  ) |> 
  dplyr::select( # removendo variáveis desnecessárias
    -NU_ANO_CENSO
  ) |> 
  dplyr::relocate( # colocando a variável TMP_DESFECHO como primeira variável do dataframe
    TMP_DESFECHO
  )

#  Juntando as tabelas com as informações dos ingressantes com a das informações 
# dos cursos.
dados_analise <- dplyr::left_join( # Juntando as tabelas
  ingressantes, DM_CURSO2019, by = dplyr::join_by(CO_IES, CO_CURSO)
  ) |>
  dplyr::filter( # Filtrando os cursos de engenhaia elétrica
    CO_CINE_ROTULO == "0713E05"
  )

summary(dados_analise$TMP_DESFECHO)

table(dados_analise$NO_CURSO)
