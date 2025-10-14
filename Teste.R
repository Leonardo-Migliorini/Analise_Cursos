# Carregando bancos de dados ---------------------------------------------------

ingressantes <- arrow::read_parquet("Dados//ingressantes.parquet")

DM_CURSO2010 <- arrow::read_parquet("Dados//DM_CURSO2010.parquet")

DM_CURSO2019 <- arrow::read_parquet("Dados//DM_CURSO2019.parquet")

DM_DOCENTE_2019 <- arrow::read_parquet("Dados//DM_DOCENTE_2019.parquet")

DM_LOCAL_OFERTA_2019 <- arrow::read_parquet("Dados//DM_LOCAL_OFERTA_2019.parquet")

DM_IES_2019 <- arrow::read_parquet("Dados//DM_IES_2019.parquet")

TB_AUX_CINE_BRASIL_2019 <- arrow::read_parquet("Dados//TB_AUX_CINE_BRASIL_2019.parquet")