# Carregando dados no formato csv:

ALUNO_ING2010 <- readr::read_csv("ALUNO_ING2010-surv.csv")

DM_CURSO2010 <- readr::read_delim("DM_CURSO2010.csv", delim = "|", 
                                  locale = readr::locale(encoding = "ISO-8859-1"))

DM_CURSO2019 <- readr::read_delim("DM_CURSO2019.csv", delim = "|", 
                                 locale = readr::locale(encoding = "ISO-8859-1"))

DM_DOCENTE_2019 <- readr::read_delim("DM_DOCENTE_2019.csv", delim = "|", 
                                    locale = readr::locale(encoding = "ISO-8859-1"))

DM_LOCAL_OFERTA_2019 <- readr::read_delim("DM_LOCAL_OFERTA_2019.csv", delim = "|", 
                                          locale = readr::locale(encoding = "ISO-8859-1"))

DM_IES_2019 <- readr::read_delim("DM_IES_2019.csv", delim = "|", 
                                 locale = readr::locale(encoding = "ISO-8859-1"))

TB_AUX_CINE_BRASIL_2019 <- readr::read_delim("TB_AUX_CINE_BRASIL_2019.csv", delim = "|", 
                                             locale = readr::locale(encoding = "ISO-8859-1"))

# Salvando os dados em formato parquet:

ingressantes <- arrow::write_parquet(ALUNO_ING2010, "ingressantes.parquet")

DM_CURSO2010 <- arrow::write_parquet(DM_CURSO2010, "DM_CURSO2010.parquet")

DM_CURSO2019 <- arrow::write_parquet(DM_CURSO2019, "DM_CURSO2019.parquet")

DM_DOCENTE_2019 <- arrow::write_parquet(DM_DOCENTE_2019, "DM_DOCENTE_2019.parquet")

DM_LOCAL_OFERTA_2019 <- arrow::write_parquet(DM_LOCAL_OFERTA_2019, "DM_LOCAL_OFERTA_2019.parquet")

DM_IES_2019 <- arrow::write_parquet(DM_IES_2019, "DM_IES_2019.parquet")

TB_AUX_CINE_BRASIL_2019 <- arrow::write_parquet(TB_AUX_CINE_BRASIL_2019, "TB_AUX_CINE_BRASIL_2019.parquet")

# Lendo os dados no formato parquet:

ingressantes <- arrow::read_parquet("ingressantes.parquet")

DM_CURSO2010 <- arrow::read_parquet("DM_CURSO2010.parquet")

DM_CURSO2019 <- arrow::read_parquet("DM_CURSO2019.parquet")

DM_DOCENTE_2019 <- arrow::read_parquet("DM_DOCENTE_2019.parquet")

DM_LOCAL_OFERTA_2019 <- arrow::read_parquet("DM_LOCAL_OFERTA_2019.parquet")

DM_IES_2019 <- arrow::read_parquet("DM_IES_2019.parquet")

TB_AUX_CINE_BRASIL_2019 <- arrow::read_parquet("TB_AUX_CINE_BRASIL_2019.parquet")
