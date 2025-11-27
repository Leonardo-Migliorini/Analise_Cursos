# Ajustando separador decimal para ","
options(OutDec = ",")

# Carregando pacotes -----------------------------------------------------------

library(ggplot2)
library(ggfortify)
library(survival)
library(survminer)
library(flexsurv)

# Carregando bancos de dados ---------------------------------------------------

# Base de alunos
ALUNO_ING2010_surv <- readr::read_csv("ALUNO_ING2010-surv.csv")

# Base de cursos 2019
DM_CURSO2019 <- read.table("DM_CURSO2019.csv", header = TRUE, sep = "|")

# Filtrando bancos de dados ----------------------------------------------------

# filtro co_cine_rotulo dos cursos de agronomia
codigos_ag_cine <- DM_CURSO2019 |>
  dplyr::filter(
    CO_CINE_ROTULO == "0811A04",
    CO_UF %in% c(35)
  )

# alunos de agronomia
alunos_ag <- ALUNO_ING2010_surv |>
  dplyr::filter(
    CO_CURSO %in% codigos_ag_cine$CO_CURSO,
    TP_SITUACAO %in% c(2, 3, 4, 5, 6),
    TP_CATEGORIA_ADMINISTRATIVA %in% c(1, 2, 3)
  )

# Reorganizando algumas variáveis para a modelagem dos dados
dados_modelagem <- alunos_ag |>
  dplyr::mutate(
    tempo_em_anos = NU_ANO_CENSO - 2009,
    evento = dplyr::case_when(
      TP_SITUACAO == 6 ~ 1,
      TRUE ~ 0
    ),
    TP_NACIONALIDADE = dplyr::case_when(
      TP_NACIONALIDADE == 1 ~ 0,
      TRUE ~ 1
    ),
    CO_UF_NASCIMENTO = tidyr::replace_na(CO_UF_NASCIMENTO, 0),
    CO_UF_NASCIMENTO = dplyr::case_when(
      CO_UF_NASCIMENTO == 35 ~ "São Paulo",
      CO_UF_NASCIMENTO == 31 ~ "Minas Gerais",
      CO_UF_NASCIMENTO == 33 ~ "Rio de Janeiro",
      CO_UF_NASCIMENTO <= 29 ~ "Norte e Nordeste",
      CO_UF_NASCIMENTO == 41 ~ "Paraná",
      CO_UF_NASCIMENTO == 43 ~ "Rio Grande do Sul",
      TRUE ~ "Centro-Oeste"
    ),
    PRETA = dplyr::case_when(TP_COR_RACA == 2 ~ 1, TRUE ~ 0),
    PARDA = dplyr::case_when(TP_COR_RACA == 3 ~ 1, TRUE ~ 0),
    RACA_OUTRA = dplyr::case_when(TP_COR_RACA == 6 ~ 1, TRUE ~ 0),
    BRANCA = dplyr::case_when(TP_COR_RACA == 1 ~ 1, TRUE ~ 0),
    AMARELA = dplyr::case_when(TP_COR_RACA == 4 ~ 1, TRUE ~ 0),
    INDIGENA = dplyr::case_when(TP_COR_RACA == 5 ~ 1, TRUE ~ 0),
    CENTRO_OESTE = dplyr::case_when(CO_UF_NASCIMENTO == "Centro-Oeste" ~ 1, TRUE ~ 0),
    SP = dplyr::case_when(CO_UF_NASCIMENTO == "São Paulo" ~ 1, TRUE ~ 0),
    MG = dplyr::case_when(CO_UF_NASCIMENTO == "Minas Gerais" ~ 1, TRUE ~ 0),
    NORTE_NORDESTE = dplyr::case_when(CO_UF_NASCIMENTO == "Norte e Nordeste" ~ 1, TRUE ~ 0),
    PARANA = dplyr::case_when(CO_UF_NASCIMENTO == "Paraná" ~ 1, TRUE ~ 0),
    SUL = dplyr::case_when(
      CO_UF_NASCIMENTO %in% c("Paraná", "Rio Grande do Sul") ~ 1,
      TRUE ~ 0
    ),
    SUDESTE_SP = dplyr::case_when(
      CO_UF_NASCIMENTO %in% c("Minas Gerais", "Rio de Janeiro", "São Paulo") ~ 1,
      TRUE ~ 0
    ),
    SUDESTE = dplyr::case_when(
      CO_UF_NASCIMENTO %in% c("Minas Gerais", "Rio de Janeiro") ~ 1,
      TRUE ~ 0
    ),
    CO_MUNICIPIO_NASCIMENTO = tidyr::replace_na(CO_MUNICIPIO_NASCIMENTO, 0),
    NU_ANO_NASCIMENTO = 2010 - NU_ANO_NASCIMENTO,
    CO_PAIS_ORIGEM_1076 = dplyr::case_when(
      CO_PAIS_ORIGEM %in% c(10, 76) ~ 1,
      TRUE ~ 0
    ),
    CO_PAIS_ORIGEM_OUTRO = dplyr::case_when(
      !(CO_PAIS_ORIGEM %in% c(10, 76)) ~ 1,
      TRUE ~ 0
    ),
    CO_IES_56 = dplyr::case_when(CO_IES == 56 ~ 1, TRUE ~ 0),
    CO_IES_55 = dplyr::case_when(CO_IES == 55 ~ 1, TRUE ~ 0),
    CO_IES_7 = dplyr::case_when(CO_IES == 7 ~ 1, TRUE ~ 0),
    IDADE_18 = dplyr::case_when(NU_ANO_NASCIMENTO <= 18 ~ 1, TRUE ~ 0),
    IDADE_19 = dplyr::case_when(NU_ANO_NASCIMENTO == 19 ~ 1, TRUE ~ 0),
    IDADE_20 = dplyr::case_when(NU_ANO_NASCIMENTO == 20 ~ 1, TRUE ~ 0),
    IDADE_21 = dplyr::case_when(NU_ANO_NASCIMENTO >= 21 ~ 1, TRUE ~ 0),
    TP_SEXO = dplyr::case_when(TP_SEXO == 1 ~ 0, TRUE ~ 1)
  )

# Análise descritiva -----------------------------------------------------------

## Curva de kaplan Meier geral

fit_km_final <- survfit(Surv(
  time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento
) ~ 1)

summary(fit_km_final)

summary(fit_km_final, times = c(5, 6, 7))
quantile(fit_km_final, probs = c(0.25, 0.50, 0.75))

print(
  ggsurvplot(
    fit_km_final,
    data = dados_modelagem,
    conf.int = TRUE,
    risk.table = TRUE,
    title = "Curva de Formatura: Agronomia em IES Públicas de SP",
    xlab = "Tempo em Anos desde o Ingresso",
    ylab = "Probabilidade de Permanência (Não Formado)",
    surv.median.line = "hv",
    ggtheme = theme_light()
  )
)

## Teste de log-rank - H0: S1(t) = S2(t)

# Sexo
survdiff(Surv(
  time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento
) ~ dados_modelagem$TP_SEXO, rho = 0)

# Rejeitamos H0, logo há indicios de diferença entre as curvas de sobrevivência
# estratificada por sexo

# Plotando as curvas de kaplan-Meier segregadas por sexo
eKM <- survfit(Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ dados_modelagem$TP_SEXO)
summary(eKM)

autoplot(eKM) +
  labs(
    y = "S(t) estimada",
    x = "Tempo em dias"
  ) +
  # renomeia somente a legenda original 'strata'
  scale_fill_manual(
    values = c("0" = "pink", "1" = "lightblue"),
    labels = c("Feminino", "Masculino")
  ) +
  scale_color_manual(
    values = c("0" = "red", "1" = "blue"),
    labels = c("Feminino", "Masculino")
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid = element_blank())

# IES 55 (USP)
survdiff(Surv(
  time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento
) ~ dados_modelagem$CO_IES_55, rho = 0)

# Rejeitamos H0, logo há indicios de diferença entre as curvas de sobrevivência

# Plotando as curvas de kaplan-Meier segregadas por sexo
eKM2 <- survfit(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~
    dados_modelagem$CO_IES_55
)
summary(eKM2)

autoplot(eKM2) +
  labs(
    y = "S(t) estimada",
    x = "Tempo em dias"
  ) +
  # renomeia somente a legenda original 'strata'
  scale_fill_manual(
    values = c("0" = "pink", "1" = "lightblue"),
    labels = c("USP", "Outras IES")
  ) +
  scale_color_manual(
    values = c("0" = "red", "1" = "blue"),
    labels = c("USP", "Outras IES")
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid = element_blank())

# IES 56 (UNESP)
survdiff(Surv(
  time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento
) ~ dados_modelagem$CO_IES_56, rho = 0)

# Rejeitamos H0, logo há indicios de diferença entre as curvas de sobrevivência

# Plotando as curvas de kaplan-Meier segregadas por sexo
eKM3 <- survfit(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~
    dados_modelagem$CO_IES_56
)
summary(eKM3)

autoplot(eKM3) +
  labs(
    y = "S(t) estimada",
    x = "Tempo em dias"
  ) +
  # renomeia somente a legenda original 'strata'
  scale_fill_manual(
    values = c("0" = "pink", "1" = "lightblue"),
    labels = c("UNESP", "Outras IES")
  ) +
  scale_color_manual(
    values = c("0" = "red", "1" = "blue"),
    labels = c("UNESP", "Outras IES")
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid = element_blank())



## Modelagem de S(.) paramétrica -----------------------------------------------

## Ajuste exponencial
fit_exp <- survreg(
  Surv(
    time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento
  ) ~ 1,
  dist = "exp",
  data = dados_modelagem
)
summary(fit_exp)

alphaehat <- exp(fit_exp$icoef[1])

# S(t)hat Exponencial
(s_exp <- exp(-fit_km_final$time / alphaehat))

# Plot do ajuste
plot(fit_km_final$surv, s_exp,
  pch = 16, ylim = range(c(0, 1)), xlim = range(c(0, 1)),
  xlab = "S(t): Kaplan-Meier", ylab = "S(t): Exponencial"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)



## Ajuste para a log-normal
fit_lognorm <- survreg(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ 1,
  dist = "lognormal", data = dados_modelagem
)

summary(fit_lognorm)

muhat <- fit_lognorm$coef[1]
sigmahat <- fit_lognorm$scale

# S(t)hat Log-Normal
s_lnorm <- pnorm((-log(1:10) + muhat) / sigmahat)

# Plot do ajuste
plot(s_lnorm)
plot(fit_km_final$surv, s_lnorm,
  pch = 16, ylim = range(c(0, 1)), xlim = range(c(0, 1)),
  xlab = "S(t): Kaplan-Meier", ylab = "S(t): log-normal"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)




## Ajuste para a Weibull
fit_weibull <- survreg(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ 1,
  dist = "weibull", data = dados_modelagem
)

summary(fit_weibull)

alphawhat <- exp(fit_weibull$icoef[1])
gamahat <- 1 / fit_weibull$scale

# S(t)hat Weibull
s_weib <- exp(-(1:10 / alphawhat)^(gamahat))

# Plot do ajuste
plot(fit_km_final$surv, s_weib,
  pch = 16, ylim = range(c(0, 1)), lim = range(c(0, 1)),
  xlab = "S(t): Kaplan-Meier", ylab = "S(t): Weibull"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)



## Ajuste para a Gamma
fit_gamma <- flexsurvreg(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ 1,
  dist = "gengamma", data = dados_modelagem
)

summary(fit_gamma)

# S(t)hat Gamma
s_gamma <- 1 - pgengamma(1:10,
  mu = fit_gamma$coefficients[1],
  sigma = exp(fit_gamma$coefficients[2]), Q = fit_gamma$coefficients[3]
)

# Plot do ajuste
plot(fit_km_final$surv, s_gamma,
  pch = 16, ylim = range(c(0, 1)), xlim = range(c(0, 1)),
  xlab = "S(t): Kaplan-Meier", ylab = "S(t): gamma"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)



# Gráficos do Método 2: linearização da função de sobrevivência.

# modelo exponencial
plot(fit_km_final$time, -log(fit_km_final$surv), pch = 16, xlab = "tempos", ylab = "-log(S(t))")

# modelo Weibull
plot(log(fit_km_final$time), log(-log(fit_km_final$surv)),
  pch = 16, xlab = "log(tempos)",
  ylab = "log(-log(S(t)))"
)

# modelo Lognormal
invst <- qnorm(fit_km_final$surv)
plot(log(fit_km_final$time), invst,
  pch = 16, xlab = "log(tempos)",
  ylab = expression(Phi^1 * S(t))
)

# Testes de adequação dos modelos

# Razão de verossimilhanças
2 * (fit_gamma$loglik - fit_exp$loglik[2])
2 * (fit_gamma$loglik - fit_lognorm$loglik[2])
2 * (fit_gamma$loglik - fit_weibull$loglik[2])

# pvalores do teste
(p.valor1 <- 1 - pchisq(2 * (fit_gamma$loglik - fit_exp$loglik[2]), 2))
(p.valor3 <- 1 - pchisq(2 * (fit_gamma$loglik - fit_lognorm$loglik[2]), 1))
(p.valor2 <- 1 - pchisq(2 * (fit_gamma$loglik - fit_weibull$loglik[2]), 1))

# Pelos resultados dos testes da razão de verossimilhanças para o ajuste pela
# exponencial e weibull a hipótese nula é rejeitada, considerando significância
# de 0.05, logo os modelos não são adequados para este conjunto de dados. Já para a
# distribuição log-normal a hipótese nula não é rejeitada, logo o ajuste log-normal
# é adequado.

# Tempo médio em anos até a conclusão do curso
(e_lnorm <- exp(muhat + (sigmahat^2 / 2)))

# Tempo mediano em anos até a conclusão do curso
(m_lnorm <- exp(muhat + (sigmahat * qnorm(0.5))))

# sobrevivência 1 ano
(s_lnorm1 <- pnorm((-log(1) + muhat) / sigmahat))

# sobrevivência 2 anos:
(s_lnorm2 <- pnorm((-log(2) + muhat) / sigmahat))

## Modelo de regressão paramétrico ---------------------------------------------

# Ajuste (final) para a exponencial
fit_reg_exp <- survreg(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ CO_IES_56 +
    TP_SEXO + SP + MG + SUL + INDIGENA + CO_IES_55:TP_NACIONALIDADE,
  dist = "exp",
  data = dados_modelagem
)

alphaehat <- exp(fit_reg_exp$icoef[1])

summary(fit_reg_exp)



# Ajuste (final) para a log-normal
fit_reg_lognorm <- survreg(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ CO_IES_56 +
    TP_SEXO + SP + MG + SUL + INDIGENA + CO_IES_55:TP_NACIONALIDADE,
  dist = "lognormal", data = dados_modelagem
)

summary(fit_reg_lognorm)
muhat <- fit_reg_lognorm$coef[1]
sigmahat <- fit_reg_lognorm$scale
s_lnorm <- pnorm((-log(1:10) + muhat) / sigmahat)

# Análise de Resíduos

# Vetor de estimativas
Xbeta <- fit_reg_lognorm$coefficients[1] +
  fit_reg_lognorm$coefficients[2] * dados_modelagem$CO_IES_56 +
  fit_reg_lognorm$coefficients[3] * dados_modelagem$TP_SEXO +
  fit_reg_lognorm$coefficients[4] * dados_modelagem$SP +
  fit_reg_lognorm$coefficients[5] * dados_modelagem$MG +
  fit_reg_lognorm$coefficients[6] * dados_modelagem$SUL +
  fit_reg_lognorm$coefficients[7] * dados_modelagem$INDIGENA +
  fit_reg_lognorm$coefficients[8] * (dados_modelagem$CO_IES_55 * dados_modelagem$TP_NACIONALIDADE)

sigma <- fit_reg_lognorm$scale

# Resı́duos de Cox-Snell

ei <- -log(1 - pnorm(((log(dados_modelagem$tempo_em_anos) - Xbeta) / sigma)))

fit_km <- survfit(Surv(
  time = ei, event = dados_modelagem$evento
) ~ 1)
t <- fit_km$time
st <- fit_km$surv
sexp <- exp(-t)
par(mfrow = c(1, 2))

# Plot dos resíduos vs função taxa de falha acumulada
# Deseja-se observar uma reta com inclinação 1
plot(st, sexp,
  xlab = "S(ei): Kaplan-Meier",
  ylab = "S(ei): Exponencial padrão",
  pch = 16
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# Observeando o plot, temos uma reta bem próxima do comportamento desejado.

# Plot da curva de sobrevivência estimada (modelo exponencial) vs o estimador de
# Kaplan Meier
plot(fit_km, conf.int = F, mark.time = F, xlab = "Resı́duos de
Cox-Snell", ylab = "Sobrevivência estimada")
lines(t, sexp, lty = 4)
legend(1.0, 0.8, lty = c(1, 4), c(
  "Kaplan-Meier",
  "Exponencial padrão"
), cex = 0.8, bty = "n")
# Note que a curva da exponencial se sobressai bem a curva do Kaplan Meier.

# Interpretação

# OBS: Aqui não teremos gráficos da curva de sobrevivência, pois todas as
# covariáveis são dicotômicas

# CO_IES_56
exp(fit_reg_lognorm$coefficients[2])

# O tempo mediano de formação dos alunos matrículados na IES 56 é 5% menor em
# comparação as demais IES para o curso de agronomia (mantidas fixas as demais
# covariáveis no modelo)

# TP_SEXO
exp(fit_reg_lognorm$coefficients[3])

# O tempo mediano de formação das mulheres seria 5% menor em relação aos homens
# para os cursos de agronomia (mantidas fixas as demais covariáveis no modelo)

# SP
exp(fit_reg_lognorm$coefficients[4])
# O tempo mediano de formação dos alunos no estado de São Paulo é 9,6% maior em
# relação aos demais estados brasileiros para os cursos de agronomia (mantidas
# fixas as demais covariáveis no modelo)

# MG
exp(fit_reg_lognorm$coefficients[5])
# O tempo mediano de formação dos alunos no estado de Minas Geraus é 12,7% maior em
# relação aos demais estados brasileiros para os cursos de agronomia (mantidas
# fixas as demais covariáveis no modelo)

# SUL
exp(fit_reg_lognorm$coefficients[6])
# O tempo mediano de formação dos alunos na região sul do Brasil é 13,1% maior em
# relação as demais regiões brasileiras para os cursos de agronomia (mantidas
# fixas as demais covariáveis no modelo)


# INDIGENA
exp(fit_reg_lognorm$coefficients[7])
# O tempo mediano de formação dos alunos autodeclarados indígenas é 34,8% maior em
# relação as demais raças autodeclaráveis para os cursos de agronomia (mantidas
# fixas as demais covariáveis no modelo)


# CO_IES_55:TP_NACIONALIDADE
exp(fit_reg_lognorm$coefficients[8])
# O tempo mediano de formação de alunos estrangeiros matrículados na IES 56 é
# 52,4% maior em relação as demais IES e a alunos brasileiros para os cursos de
# agronomia (mantidas fixas as demais covariáveis no modelo)

# Ajuste (final) para a Weibull

fit_reg_weibull <- survreg(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ CO_IES_56 +
    TP_SEXO + SP + MG + SUL + INDIGENA + CO_IES_55:TP_NACIONALIDADE,
  dist = "weibull", data = dados_modelagem
)

summary(fit_reg_weibull)
alphawhat <- exp(fit_reg_weibull$icoef[1])
gamahat <- 1 / fit_reg_weibull$scale
s_weib <- exp(-(1:10 / alphawhat)^(gamahat))



# Ajuste (final) para a Gamma generalizado

fit_reg_gamma <- flexsurvreg(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ CO_IES_56 +
    TP_SEXO + SP + MG + SUL + INDIGENA + CO_IES_55:TP_NACIONALIDADE,
  dist = "gengamma", data = dados_modelagem
)

summary(fit_reg_gamma)

# s_gamma <- 1 - pgengamma(1:10, mu = fit_reg_gamma$coefficients[1],
#     sigma = exp(fit_reg_gamma$coefficients[2]), Q = fit_reg_gamma$coefficients[3])


# Testes de adequação dos modelos

# Razão de verossimilhanças
2 * (fit_reg_gamma$loglik - fit_reg_exp$loglik[2])
2 * (fit_reg_gamma$loglik - fit_reg_lognorm$loglik[2])
2 * (fit_reg_gamma$loglik - fit_reg_weibull$loglik[2])

# pvalores do teste
(p.valor1 <- 1 - pchisq(2 * (fit_reg_gamma$loglik - fit_reg_exp$loglik[2]), 2))
(p.valor2 <- 1 - pchisq(2 * (fit_reg_gamma$loglik - fit_reg_lognorm$loglik[2]), 1))
(p.valor3 <- 1 - pchisq(2 * (fit_reg_gamma$loglik - fit_reg_weibull$loglik[2]), 1))

# Pelos resultados dos testes da razão de verossimilhanças, considerando significância
# de 0.05, o modelo gamma seria o mais adequado para a modelagem dos dados.

# Análise de Resíduos

# Vetor de estimativas
# Xbeta <- fit_reg_gamma$coefficients[1] +
#         fit_reg_gamma$coefficients[4] * dados_modelagem$CO_IES_56 +
#         fit_reg_gamma$coefficients[5] * dados_modelagem$TP_SEXO +
#         fit_reg_gamma$coefficients[6] * dados_modelagem$SP +
#         fit_reg_gamma$coefficients[7] * dados_modelagem$MG +
#         fit_reg_gamma$coefficients[8] * dados_modelagem$SUL +
#         fit_reg_gamma$coefficients[9] * dados_modelagem$INDIGENA +
#         fit_reg_gamma$coefficients[10] * (dados_modelagem$CO_IES_55*dados_modelagem$TP_NACIONALIDADE)
#
# sigma = exp(fit_reg_gamma$coefficients[2])
# Q = fit_reg_gamma$coefficients[3]

# Resı́duos de Cox-Snell
# ei <-  exp(-Xbeta) # não consegui encontrar as expressões necessárias para
# definir a equação do resíduo de cox snell para a distribuição gamma generalizada

# O ChatGPT sugeriu calcular assim, espero que esteja certo.

ei <- -log(1 - pgengamma(dados_modelagem$tempo_em_anos,
  mu    = fit_reg_gamma$res["mu", "est"],
  sigma = fit_reg_gamma$res["sigma", "est"],
  Q     = fit_reg_gamma$res["Q", "est"]
))

fit_km <- survfit(Surv(
  time = ei, event = dados_modelagem$evento
) ~ 1)
t <- fit_km$time
st <- fit_km$surv
sexp <- exp(-t)
par(mfrow = c(1, 2))

# Plot dos resíduos vs função taxa de falha acumulada
# Deseja-se observar uma reta com inclinação 1
plot(st, sexp,
  xlab = "S(ei): Kaplan-Meier",
  ylab = "S(ei): Exponencial padrão",
  pch = 16
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# Observeando o plot, vemos que há um certo desvio do comportamento esperado

# Plot da curva de sobrevivência estimada (modelo exponencial) vs o estimador de
# Kaplan Meier
plot(fit_km, conf.int = F, mark.time = F, xlab = "Resı́duos de
Cox-Snell", ylab = "Sobrevivência estimada")
lines(t, sexp, lty = 4)
legend(1.0, 0.8, lty = c(1, 4), c(
  "Kaplan-Meier",
  "Exponencial padrão"
), cex = 0.8, bty = "n")

# Note que a curva Kaplan Meier até que se sobressai bem a curva da exponencial padrão.

# Interpretações do modelo

## Modelo de Cox ---------------------------------------------------------------

## ajuste (final) do modelo de cox
survcoxfit <- coxph(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~
    CO_IES_56 + TP_SEXO + CO_IES_55:TP_NACIONALIDADE,
  data = dados_modelagem, x = T, method = "breslow"
)

summary(survcoxfit)

# Análise de Resíduos

# Os resíduos devem seguir uma exponencial padrão

resm <- resid(survcoxfit, type = "martingale") # resíduo de martingal
res <- dados_modelagem$evento - resm # resíduo de cox-Snell
ekm <- survfit(Surv(res, dados_modelagem$evento) ~ 1)

# Plot dos resíduos vs exp(1)
plot(ekm,
  mark.time = F, conf.int = F, xlab = "Res´ıduos",
  ylab = "S(e) estimada"
)
res <- sort(res)
exp1 <- exp(-res)
lines(res, exp1, lty = 3)
legend(1, 0.8,
  lty = c(1, 3), c("Kaplan Meier", "Exponencial(1)"),
  lwd = 1, bty = "n", cex = 0.7
)

# Deve formar uma linha aproximadamente reta

st <- ekm$surv
t <- ekm$time
sexp1 <- exp(-t)
plot(st, sexp1,
  xlab = "S(e): Kaplan-Meier",
  ylab = "S(e): Exponencial(1)", pch = 16
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# Suposição de riscos proporcionais

# Ho: riscos proporcionais
cox.zph(survcoxfit, transform = "identity")

# temos evidência de riscos proporcionais para todas as variáveis

ggcoxdiagnostics(survcoxfit,
  type = "scaledsch", linear.predictions = FALSE,
  ggtheme = theme_bw()
)

# Avaliação dos resíduos - Pontos atípicos
ggcoxdiagnostics(survcoxfit,
  type = "deviance",
  linear.predictions = TRUE, ggtheme = theme_bw()
)
ggcoxdiagnostics(survcoxfit,
  type = "martingale",
  linear.predictions = TRUE, ggtheme = theme_bw()
)

# Avaliação dos resíduos - Pontos influentes
ggcoxdiagnostics(survcoxfit,
  type = "dfbetas",
  linear.predictions = FALSE, ggtheme = theme_bw()
)

# Interpretações do modelo

# CO_IES_56
# A taxa de formação dos alunos da UNESP é de 0,78 vezes a de alunos das demais IES.

exp(survcoxfit$coefficients[1])

# TP_SEXO
# A taxa de formação de homens é de aproximadamente 1,17 vezes a das mulheres.

exp(survcoxfit$coefficients[2])

# CO_IES_55:TP_NACIONALIDADE
# A taxa de formação de alunos estrangeiros na USP é de aproximadamente 78 vezes
# em comparação a alunos de outras IES ou brasileiros. 

exp(survcoxfit$coefficients[3])
