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
library(dplyr)

# Carregando bancos de dados ---------------------------------------------------

# Base de alunos
ALUNO_ING2010_surv <- readr::read_csv("Dados//ALUNO_ING2010-surv.csv")

# Base de cursos 2019
DM_CURSO2019 <- read.table("Dados//DM_CURSO2019.csv", header = TRUE, sep = "|")

# Filtrando bancos de dados ----------------------------------------------------

# filtro co_cine_rotulo dos cursos de Engenharia aeroespacia
codigos_ag_cine <- DM_CURSO2019 |>
  dplyr::filter(
    CO_CINE_ROTULO == "0716E01",
  )

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
    UFABC = dplyr::case_when(CO_IES == 4925 ~ 1, TRUE ~ 0),
    UFMG = dplyr::case_when(CO_IES == 575 ~ 1, TRUE ~ 0),
    O_IES = dplyr::case_when(CO_IES %in% c(2, 585, 602) ~ 1, TRUE ~ 0),
    PRETA = dplyr::case_when(TP_COR_RACA == 2 ~ 1, TRUE ~ 0),
    PARDA = dplyr::case_when(TP_COR_RACA == 3 ~ 1, TRUE ~ 0),
    RACA_OUTRA = dplyr::case_when(TP_COR_RACA == 6 ~ 1, TRUE ~ 0),
    BRANCA = dplyr::case_when(TP_COR_RACA == 1 ~ 1, TRUE ~ 0),
    AMARELA = dplyr::case_when(TP_COR_RACA == 4 ~ 1, TRUE ~ 0),
    INDIGENA = dplyr::case_when(TP_COR_RACA == 5 ~ 1, TRUE ~ 0),
    NU_ANO_NASCIMENTO = 2010 - NU_ANO_NASCIMENTO,
    TP_SEXO = dplyr::case_when(TP_SEXO == 1 ~ 0, TRUE ~ 1)
  )

# Análise descritiva -----------------------------------------------------------

tab_evento <- dados_modelagem |>
  dplyr::count(evento) |>
  mutate(
    Situacao = ifelse(evento == 1, "Formado (Evento)", "Censurado"),
    Prop = n / sum(n) * 100
  )

stats_tempo <- dados_modelagem |>
  summarise(
    Minimo = min(tempo_em_anos, na.rm = TRUE),
    Media = mean(tempo_em_anos, na.rm = TRUE),
    Mediana = median(tempo_em_anos, na.rm = TRUE),
    Maximo = max(tempo_em_anos, na.rm = TRUE),
    DP = sd(tempo_em_anos, na.rm = TRUE)
  )

# Estatísticas descritivas sem os dados censurados
stats_tempo_censless <- dados_modelagem |>
  dplyr::filter(
    evento == 1
  ) |>
  summarise(
    Minimo = min(tempo_em_anos, na.rm = TRUE),
    Media = mean(tempo_em_anos, na.rm = TRUE),
    Mediana = median(tempo_em_anos, na.rm = TRUE),
    Maximo = max(tempo_em_anos, na.rm = TRUE),
    DP = sd(tempo_em_anos, na.rm = TRUE)
  )

get_freq_formatada <- function(dados, variavel, nome_var) {
  dados |>
    dplyr::count({{ variavel }}) |>
    dplyr::mutate(
      Variavel = nome_var,
      Categoria = as.character({{ variavel }}),
      Prop = n / sum(n) * 100,
      N_Inteiro = format(n, big.mark = "."),
      Prop_Texto = format(round(Prop, 1), decimal.mark = ",")
    ) |>
    dplyr::select(Variavel, Categoria, N_Inteiro, Prop_Texto)
}

df_perfil <- dados_modelagem |>
  dplyr::filter(  # Para remover as censuras
    evento == 1
  ) |>
  dplyr::mutate(
    Nome_IES = dplyr::case_when(
      UFMG == 1 ~ "UFMG",
      UFABC == 1 ~ "UFABC",
      TRUE ~ "Outras"
    ),
    Nome_Sexo = ifelse(TP_SEXO == 1, "Masculino", "Feminino"),
    Nome_Raca = dplyr::case_when(
      BRANCA == 1 ~ "Branca",
      PRETA == 1 ~ "Preta",
      PARDA == 1 ~ "Parda",
      INDIGENA == 1 ~ "Indígena",
      TRUE ~ "Outras"
    )
  )

# 2. Gerando as partes da tabela
t1 <- get_freq_formatada(df_perfil, Nome_IES, "Instituição (IES)")
t2 <- get_freq_formatada(df_perfil, Nome_Sexo, "Sexo")
t3 <- get_freq_formatada(df_perfil, Nome_Raca, "Raça/Cor")

# 3. Unindo tudo
tabela_perfil <- dplyr::bind_rows(t1, t2, t3)

# 4. Gerando Tabela LaTeX
kbl(tabela_perfil,
    col.names = c("Variável", "Categoria", "N (Absoluto)", "% (Relativo)"),
    caption = "Caracterização demográfica e institucional dos estudantes.",
    booktabs = TRUE,
    align = c("l", "l", "r", "r")
) |>
  kableExtra::collapse_rows(columns = 1, valign = "top") |>
  kable_styling(
    latex_options = c("HOLD_POSITION", "striped"),
    full_width = F,
    position = "center"
  )

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
    title = "Curva de Formatura: Engenharia Aeroespacial",
    xlab = "Tempo em Anos desde o Ingresso",
    ylab = "Probabilidade de Permanência",
    surv.median.line = "hv",
    ggtheme = theme_light()
  )
)

## Teste de log-rank - H0: S1(t) = S2(t)

eKM_sexo <- survfit(Surv(tempo_em_anos, evento) ~ TP_SEXO, data = dados_modelagem)

# Teste Log-Rank
diff_sexo <- survdiff(Surv(tempo_em_anos, evento) ~ TP_SEXO, data = dados_modelagem)
pval_sexo <- 1 - pchisq(diff_sexo$chisq, length(diff_sexo$n) - 1)
p_label_sexo <- paste("Log-Rank p <", ifelse(pval_sexo < 0.001, "0,001", round(pval_sexo, 3)))

p1 <- autoplot(eKM_sexo) +
  labs(title = "Estratificado por Sexo", subtitle = p_label_sexo, y = "S(t) Estimada", x = "Tempo (anos)") +
  scale_fill_manual(values = c("pink", "lightblue"), labels = c("Feminino", "Masculino"), name = "Sexo") +
  scale_color_manual(values = c("red", "blue"), labels = c("Feminino", "Masculino"), name = "Sexo") +
  theme_minimal() + theme(panel.grid = element_blank(), legend.position = "bottom")

p1

## Teste de log-rank - H0: S1(t) = S2(t)

eKM_IES <- survfit(Surv(tempo_em_anos, evento) ~ UFMG, data = dados_modelagem)

# Teste Log-Rank
diff_sexo <- survdiff(Surv(tempo_em_anos, evento) ~ UFMG, data = dados_modelagem)
pval_sexo <- 1 - pchisq(diff_sexo$chisq, length(diff_sexo$n) - 1)
p_label_sexo <- paste("Log-Rank p <", ifelse(pval_sexo < 0.001, "0,001", round(pval_sexo, 3)))

p2 <- autoplot(eKM_IES) +
  labs(title = "Estratificado por IES (UFMG)", subtitle = p_label_sexo, y = "S(t) Estimada", x = "Tempo (anos)") +
  scale_fill_manual(values = c("pink", "lightblue"), labels = c("Outras", "UFMG"), name = "IES") +
  scale_color_manual(values = c("red", "blue"), labels = c("Outras", "UFMG"), name = "IES") +
  theme_minimal() + theme(panel.grid = element_blank(), legend.position = "bottom")

p2

## Teste de log-rank - H0: S1(t) = S2(t)

eKM_IES2 <- survfit(Surv(tempo_em_anos, evento) ~ UFABC, data = dados_modelagem)

# Teste Log-Rank
diff_sexo <- survdiff(Surv(tempo_em_anos, evento) ~ UFABC, data = dados_modelagem)
pval_sexo <- 1 - pchisq(diff_sexo$chisq, length(diff_sexo$n) - 1)
p_label_sexo <- paste("Log-Rank p <", ifelse(pval_sexo < 0.001, "0,001", round(pval_sexo, 3)))

p3 <- autoplot(eKM_IES2) +
  labs(title = "Estratificado por IES (UFABC)", subtitle = p_label_sexo, y = "S(t) Estimada", x = "Tempo (anos)") +
  scale_fill_manual(values = c("pink", "lightblue"), labels = c("Outras", "UFABC"), name = "IES") +
  scale_color_manual(values = c("red", "blue"), labels = c("Outras", "UFABC"), name = "IES") +
  theme_minimal() + theme(panel.grid = element_blank(), legend.position = "bottom")

p3


## Modelagem de S(.) paramétrica -----------------------------------------------

par(mfrow = c(1, 1))

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
s_lnorm <- pnorm((-log(fit_km_final$time) + muhat) / sigmahat)

# Plot do ajuste
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
s_weib <- exp(-(fit_km_final$time / alphawhat)^(gamahat))

## Ajuste para a Gamma
fit_gamma <- flexsurvreg(
  Surv(time = dados_modelagem$tempo_em_anos, event = dados_modelagem$evento) ~ 1,
  dist = "gengamma", data = dados_modelagem
)

summary(fit_gamma)

# S(t)hat Gamma
s_gamma <- 1 - pgengamma(fit_km_final$time,
  mu = fit_gamma$coefficients[1],
  sigma = exp(fit_gamma$coefficients[2]), Q = fit_gamma$coefficients[3]
)

# Plot do ajuste
plot(fit_km_final$surv, s_gamma,
  pch = 16, ylim = range(c(0, 1)), xlim = range(c(0, 1)),
  xlab = "S(t): Kaplan-Meier", ylab = "S(t): gamma"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# Plot do ajuste
plot(fit_km_final$surv, s_weib,
     pch = 16, ylim = range(c(0, 1)), lim = range(c(0, 1)),
     xlab = "S(t): Kaplan-Meier", ylab = "S(t): Weibull"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

plot(fit_km_final, conf.int = FALSE, xlab = "Tempo", ylab = "S(t)")
lines(s_weib, col = 2, lwd = 2)
lines(s_lnorm, col = 3, lwd = 2)
lines(s_gamma, col = 4, lwd = 2)
lines(s_exp, col = 5, lwd = 2)
legend("topright",
       legend = c("Kaplan-Meier", "Weibull", "Log-normal", "Gamma", "Exponencial"),
       col = c(1, 2, 3, 4, 5), lwd = 2, bty = "n"
)

get_metrics <- function(model, name) {
  if (inherits(model, "flexsurvreg")) {
    loglik <- model$loglik
    aic <- model$AIC
    n_pars <- model$npars
  } else {
    loglik <- model$loglik[2]
    n_pars <- length(model$coef) + 1
    if (model$dist == "exponential") n_pars <- 1
    aic <- 2 * n_pars - 2 * loglik
  }
  return(data.frame(Modelo = name, LogLik = loglik, AIC = aic, Parametros = n_pars))
}

resultados_ajuste <- rbind(
  get_metrics(fit_exp, "Exponencial"),
  get_metrics(fit_weibull, "Weibull"),
  get_metrics(fit_lognorm, "Log-Normal"),
  get_metrics(fit_gamma, "Gamma Generalizada")
) |>
  dplyr::arrange(AIC)

kbl(resultados_ajuste,
  caption = "Comparação dos critérios de ajuste (AIC e Log-Verossimilhança) para os modelos paramétricos.",
  digits = 2,
  booktabs = T
) |>
  kable_styling(latex_options = c("HOLD_POSITION", "striped"), position = "center")


# Gráficos do Método 2: linearização da função de sobrevivência.

par(mfrow = c(1, 1))
# Exponencial
y_exp <- -log(fit_km_final$surv)
x_exp <- fit_km_final$time
idx_exp <- is.finite(y_exp) & is.finite(x_exp)

plot(x_exp, y_exp,
  pch = 16,
  xlab = "Tempo (t)",
  ylab = expression(-log(S(t))),
  main = "Linearização Exponencial"
)
abline(lm(y_exp[idx_exp] ~ x_exp[idx_exp]), col = "red", lty = 2)

# Weibull
y_weib <- log(-log(fit_km_final$surv))
x_weib <- log(fit_km_final$time)
idx_weib <- is.finite(y_weib) & is.finite(x_weib)

plot(x_weib, y_weib,
  pch = 16,
  xlab = "log(t)",
  ylab = "log(-log(S(t)))",
  main = "Linearização Weibull"
)
abline(lm(y_weib[idx_weib] ~ x_weib[idx_weib]), col = "red", lty = 2)

# Log-Normal
y_ln <- qnorm(fit_km_final$surv)
x_ln <- log(fit_km_final$time)
idx_ln <- is.finite(y_ln) & is.finite(x_ln)

plot(x_ln, y_ln,
  pch = 16,
  xlab = "log(t)",
  ylab = expression(Phi^
    {
      -1
    } * (S(t))),
  main = "Linearização Log-Normal"
)
abline(lm(y_ln[idx_ln] ~ x_ln[idx_ln]), col = "red", lty = 2)

# Testes de adequação dos modelos

# Razão de verossimilhanças

# Gamma vs Exponencial
trv_exp <- 2 * (fit_gamma$loglik - fit_exp$loglik[2])
pval_exp <- 1 - pchisq(trv_exp, df = 2)

# Gamma vs Log-Normal
trv_ln <- 2 * (fit_gamma$loglik - fit_lognorm$loglik[2])
pval_ln <- 1 - pchisq(trv_ln, df = 1)

# Gamma vs Weibull
trv_weib <- 2 * (fit_gamma$loglik - fit_weibull$loglik[2])
pval_weib <- 1 - pchisq(trv_weib, df = 1)

tabela_trv <- data.frame(
  Modelo_Restrito = c("Exponencial", "Weibull", "Log-Normal"),
  Modelo_Geral = c("Gamma Gen.", "Gamma Gen.", "Gamma Gen."),
  Estatistica_TRV = c(trv_exp, trv_weib, trv_ln),
  Graus_Liberdade = c(2, 1, 1),
  Valor_p = c(pval_exp, pval_weib, pval_ln)
)

tabela_trv_formatada <- tabela_trv |>
  dplyr::mutate(
    Estatistica_TRV = format(round(Estatistica_TRV, 2), decimal.mark = ","),
    Valor_p = ifelse(Valor_p < 0.001, "< 0,001", format(round(Valor_p, 4), decimal.mark = ","))
  )

kbl(tabela_trv_formatada,
  caption = "Teste de Razão de Verossimilhança comparando os submodelos com a Gamma Generalizada.",
  booktabs = T,
  align = "c"
) |>
  kable_styling(latex_options = c("HOLD_POSITION", "striped"), position = "center") |>
  column_spec(1, bold = T)

# Melhor modelo é o weibull

alphawhat <- exp(fit_weibull$icoef[1])
gamahat <- 1 / fit_weibull$scale


# media
(e_weibul <- alphawhat * (gamma(1 + 1 / gamahat)))

# mediana
(m_weibull <- alphawhat * (-log(1 - 0.5))^(1 / gamahat))

# 1 e 2 anos
(s_w1 <- exp(-(5 / alphawhat)^(gamahat)))
(s_w2 <- exp(-(6 / alphawhat)^(gamahat)))

tabela_metricas <- data.frame(
  Indicador = c(
    "Tempo Medio de Conclusao",
    "Tempo Mediano de Conclusao",
    "Probabilidade de Sobrevivencia (1 ano)",
    "Probabilidade de Sobrevivencia (2 anos)"
  ),
  Valor = c(
    paste(format(round(e_weibul, 2), decimal.mark = ","), "anos"),
    paste(format(round(m_weibull, 2), decimal.mark = ","), "anos"),
    paste(format(round(s_w1 * 100, 2), decimal.mark = ","), "%"),
    paste(format(round(s_w2 * 100, 2), decimal.mark = ","), "%")
  ),
  Interpretacao = c(
    "Media estimada para formar",
    "Tempo ate 50% da turma se formar",
    "Probabilidade de continuar no curso apos 1 ano",
    "Probabilidade de continuar no curso apos 2 anos"
  )
)

kbl(tabela_metricas,
  caption = "Estimativas descritivas obtidas pelo ajuste Log-Normal",
  booktabs = T, align = "c"
) |>
  kable_styling(latex_options = c("HOLD_POSITION", "striped"), position = "center") |>
  column_spec(1, bold = T)

