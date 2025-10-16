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

# Filtrando o banco de dados dos ingressantes

ingressantes <- ingressantes |> 
  dplyr::mutate(
    TMP_DESFECHO = 2019 - NU_ANO_CENSO, # Calculando tempo de desfecho
    TP_SITUACAO = dplyr::case_when(
      TP_SITUACAO == 6 ~ 1, # 1 indica conclusão (desfecho de interesse)
      TRUE ~ 0 # 0 indica censura por: encerramento do período de acompanhamento,
               # tracamento, desistência ou transferência
    ),
    TP_SITUACAO = as.factor(TP_SITUACAO)
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
  dplyr::filter(
    TP_CATEGORIA_ADMINISTRATIVA == 7,
    # TP_CATEGORIA_ADMINISTRATIVA == c(1,2,3),
    # CO_UF == 43
  ) |>
  dplyr::filter( # Filtrando os cursos relações internacionais
    CO_CINE_ROTULO == "0312R01",
  ) 

# Calculo da proporção de censuras:
summary(dados_analise$TP_SITUACAO)[1]/nrow(dados_analise)

table(dados_analise$NO_CURSO)
colnames(dados_analise)

arrow::write_parquet(dados_analise, "dados_modelagem.parquet")
