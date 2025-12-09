# Ajustando separador decimal para ","
options(OutDec = ",")

# Carregando pacotes -----------------------------------------------------------

library(ggplot2)
library(ggfortify)
library(survival)
library(survminer)
library(flexsurv)
library(kableExtra)
library(patchwork)
library(broom)
library(knitr)

# Carregando bancos de dados ---------------------------------------------------

# Base de alunos
ALUNO_ING2010_surv <- readr::read_csv("Dados//ALUNO_ING2010-surv.csv")

# Base de cursos 2019
DM_CURSO2019 <- read.table("Dados//DM_CURSO2019.csv", header = TRUE, sep = "|")

# Filtrando bancos de dados ----------------------------------------------------

# filtro co_cine_rotulo dos cursos de agronomia
codigos_ag_cine <- DM_CURSO2019 |>
  dplyr::filter(
    CO_CINE_ROTULO == "0811A04",
    CO_UF %in% c(35)
  )
