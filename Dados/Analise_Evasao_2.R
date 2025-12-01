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
library(dplyr)
library(knitr)

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

# Tabela resumindo as cováriáveis do banco de dados
tabela_variaveis <- data.frame(
  Variavel = c(
    # IES
    "CO_IES_55", "CO_IES_56",
    # Idade
    "IDADE_18", "IDADE_19", "IDADE_20", "IDADE_21",
    # Raça
    "BRANCA", "PRETA", "PARDA", "AMARELA", "INDIGENA", "RACA_OUTRA",
    # Origem
    "SP", "MG", "PARANA", "SUL", "SUDESTE", "SUDESTE_SP", "CENTRO_OESTE", "NORTE_NORDESTE",
    # Outros
    "TP_NACIONALIDADE"
  ),
  Descricao = c(
    # IES
    "Estudante de Agronomia na Universidade de São Paulo (USP)",
    "Estudante de Agronomia na Universidade Estadual Paulista (UNESP)",
    # Idade
    "Idade ao ingressar menor ou igual a 18 anos",
    "Idade ao ingressar igual a 19 anos",
    "Idade ao ingressar igual a 20 anos",
    "Idade ao ingressar maior ou igual a 21 anos",
    # Raça
    "Autodeclarado Branca",
    "Autodeclarado Preta",
    "Autodeclarado Parda",
    "Autodeclarado Amarela",
    "Autodeclarado Indígena",
    "Outra cor/raça ou informação não declarada",
    # Origem
    "Nascido no estado de São Paulo",
    "Nascido no estado de Minas Gerais",
    "Nascido no estado do Parana",
    "Nascido na região Sul (PR ou RS)",
    "Nascido na região Sudeste (apenas MG ou RJ)",
    "Nascido na região Sudeste (MG, RJ ou SP)",
    "Nascido na região Centro-Oeste",
    "Nascido nas regiões Norte ou Nordeste",
    # Outros
    "Tipo da nacionalidade do aluno"
  )
)

# Gerar a tabela formatada em LaTeX
kbl(tabela_variaveis,
  caption = "Dicionário das covariaveis explicativas utilizadas no modelo.",
  booktabs = T,
  linesep = ""
) |>
  kable_styling(
    latex_options = c("HOLD_POSITION", "striped"),
    full_width = F,
    position = "center"
  ) |>
  column_spec(1, bold = T) |> # Deixa a primeira coluna em negrito
  column_spec(2, width = "10cm") |> # Ajusta largura da descrição
  # Agrupamento das linhas (Categorias)
  pack_rows("Instituição de Ensino (IES)", 1, 2) |>
  pack_rows("Faixa Etária (Ingresso)", 3, 6) |>
  pack_rows("Raça / Cor", 7, 12) |>
  pack_rows("Origem Geográfica (UF Nascimento)", 13, 20) |>
  pack_rows("Nacionalidade", 21, 21)

# Análise descritiva -----------------------------------------------------------

tab_evento <- dados_modelagem |>
  count(evento) |>
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


tabela_tempo_final <- stats_tempo |>
  mutate(
    Minimo  = format(round(Minimo, 2), decimal.mark = ","),
    Media   = format(round(Media, 2), decimal.mark = ","),
    Mediana = format(round(Mediana, 2), decimal.mark = ","),
    Maximo  = format(round(Maximo, 2), decimal.mark = ","),
    DP      = format(round(DP, 2), decimal.mark = ",")
  )

kbl(tabela_tempo_final,
  caption = "Medidas descritivas para o tempo até a formação (em anos).",
  col.names = c("Mínimo", "Média", "Mediana", "Máximo", "Desvio Padrão"),
  booktabs = TRUE,
  align = "c"
) |>
  kable_styling(
    latex_options = c("HOLD_POSITION", "striped"),
    full_width = F,
    position = "center"
  )

kbl(tab_evento |> select(Situacao, n, Prop),
  col.names = c("Situação Final", "Frequência (N)", "Porcentagem (%)"),
  digits = 1,
  caption = "Distribuição dos desfechos observados na coorte.",
  booktabs = TRUE,
  align = "c"
) |>
  kable_styling(
    latex_options = c("HOLD_POSITION", "striped"),
    full_width = F,
    position = "center"
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
      CO_IES_55 == 1 ~ "USP",
      CO_IES_56 == 1 ~ "UNESP",
      TRUE ~ "UFSCar/Outras"
    ),
    Nome_Sexo = ifelse(TP_SEXO == 1, "Masculino", "Feminino"),
    Nome_Raca = dplyr::case_when(
      PRETA == 1 ~ "Preta",
      PARDA == 1 ~ "Parda",
      INDIGENA == 1 ~ "Indígena",
      BRANCA == 1 ~ "Branca",
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
    title = "Curva de Formatura: Agronomia em IES Públicas de SP",
    xlab = "Tempo em Anos desde o Ingresso",
    ylab = "Probabilidade de Permanência (Não Formado)",
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

eKM_usp <- survfit(Surv(tempo_em_anos, evento) ~ CO_IES_55, data = dados_modelagem)

# Teste Log-Rank
diff_usp <- survdiff(Surv(tempo_em_anos, evento) ~ CO_IES_55, data = dados_modelagem)
pval_usp <- 1 - pchisq(diff_usp$chisq, length(diff_usp$n) - 1)
p_label_usp <- paste("Log-Rank p <", ifelse(pval_usp < 0.001, "0,001", round(pval_usp, 3)))

p2 <- autoplot(eKM_usp) +
  labs(title = "Estratificado por IES (USP)", subtitle = p_label_usp, y = "", x = "Tempo (anos)") +
  scale_fill_manual(values = c("gray", "orange"), labels = c("Outras", "USP"), name = "Instituição") +
  scale_color_manual(values = c("gray", "orange"), labels = c("Outras", "USP"), name = "Instituição") +
  theme_minimal() + theme(panel.grid = element_blank(), legend.position = "bottom")

eKM_unesp <- survfit(Surv(tempo_em_anos, evento) ~ CO_IES_56, data = dados_modelagem)

diff_unesp <- survdiff(Surv(tempo_em_anos, evento) ~ CO_IES_56, data = dados_modelagem)
pval_unesp <- 1 - pchisq(diff_unesp$chisq, length(diff_unesp$n) - 1)
p_label_unesp <- paste("Log-Rank p =", round(pval_unesp, 3))

p3 <- autoplot(eKM_unesp) +
  labs(title = "Estratificado por IES (UNESP)", subtitle = p_label_unesp, y = "", x = "Tempo (anos)") +
  scale_fill_manual(values = c("gray", "green"), labels = c("Outras", "UNESP"), name = "Instituição") +
  scale_color_manual(values = c("gray", "darkgreen"), labels = c("Outras", "UNESP"), name = "Instituição") +
  theme_minimal() + theme(panel.grid = element_blank(), legend.position = "bottom")

p1 + p2 + p3

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
s_lnorm <- pnorm((-log(1:10) + muhat) / sigmahat)

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


muhat <- coef(fit_lognorm)[1]
sigmahat <- fit_lognorm$scale

# media
e_lnorm <- exp(muhat + (sigmahat^2 / 2))

# mediana
m_lnorm <- exp(muhat + (sigmahat * qnorm(0.5)))

# 1 e 2 anos
s_lnorm1 <- pnorm((-log(1) + muhat) / sigmahat)
s_lnorm2 <- pnorm((-log(2) + muhat) / sigmahat)

tabela_metricas <- data.frame(
  Indicador = c(
    "Tempo Medio de Conclusao",
    "Tempo Mediano de Conclusao",
    "Probabilidade de Sobrevivencia (1 ano)",
    "Probabilidade de Sobrevivencia (2 anos)"
  ),
  Valor = c(
    paste(format(round(e_lnorm, 2), decimal.mark = ","), "anos"),
    paste(format(round(m_lnorm, 2), decimal.mark = ","), "anos"),
    paste(format(round(s_lnorm1 * 100, 2), decimal.mark = ","), "%"),
    paste(format(round(s_lnorm2 * 100, 2), decimal.mark = ","), "%")
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

tabela_lognorm <- broom::tidy(fit_reg_lognorm) |>
  dplyr::mutate(
    TR = exp(estimate),
    estimate = format(round(estimate, 4), decimal.mark = ","),
    std.error = format(round(std.error, 4), decimal.mark = ","),
    TR_formatado = format(round(TR, 4), decimal.mark = ","),
    p.value = ifelse(p.value < 0.001, "< 0,001", format(round(p.value, 4), decimal.mark = ","))
  ) |>
  dplyr::select(
    Variavel = term,
    Coeficiente = estimate,
    `Erro Padrao` = std.error,
    `Time Ratio (TR)` = TR_formatado,
    `Valor-p` = p.value
  )

tabela_lognorm$Variavel <- dplyr::recode(tabela_lognorm$Variavel,
  "(Intercept)" = "Intercepto (Referencia)",
  "CO_IES_56" = "IES: UNESP ",
  "TP_SEXO" = "Sexo",
  "SP" = "Origem: Sao Paulo",
  "MG" = "Origem: Minas Gerais",
  "SUL" = "Origem: Regiao Sul",
  "INDIGENA" = "Raca: Indigena",
  "CO_IES_55:TP_NACIONALIDADE" = "Interacao: USP x Nacionalidade"
)

kbl(tabela_lognorm,
  caption = "Estimativas dos parâmetros do modelo de regressão Log-Normal.",
  booktabs = TRUE,
  align = "c"
) |>
  kable_styling(latex_options = c("HOLD_POSITION", "striped"), position = "center")

# Análise de Resíduos

coefs <- fit_reg_lognorm$coefficients
sigma <- fit_reg_lognorm$scale

Xbeta <- coefs[1] +
  coefs[2] * dados_modelagem$CO_IES_56 +
  coefs[3] * dados_modelagem$TP_SEXO +
  coefs[4] * dados_modelagem$SP +
  coefs[5] * dados_modelagem$MG +
  coefs[6] * dados_modelagem$SUL +
  coefs[7] * dados_modelagem$INDIGENA +
  coefs[8] * (dados_modelagem$CO_IES_55 * dados_modelagem$TP_NACIONALIDADE)

ei <- -log(1 - pnorm(((log(dados_modelagem$tempo_em_anos) - Xbeta) / sigma)))

fit_km_res <- survfit(Surv(ei, dados_modelagem$evento) ~ 1)
t_res <- fit_km_res$time
st_res <- fit_km_res$surv
sexp_res <- exp(-t_res)

par(mfrow = c(1, 1))

plot(st_res, sexp_res,
  xlab = "S(e): Kaplan-Meier (Empírico)",
  ylab = "S(e): Exponencial Padrão (Teórico)",
  pch = 16,
  main = "Probabilidades de Sobrevivência",
  xlim = c(0, 1), ylim = c(0, 1)
)
lines(c(0, 1), c(0, 1), type = "l", lty = 2, col = "red")

plot(fit_km_res,
  conf.int = FALSE, mark.time = FALSE,
  xlab = "Resíduos de Cox-Snell",
  ylab = "Sobrevivência Estimada S(e)",
  main = "Aderência da Curva de Sobrevivência"
)
lines(t_res, sexp_res, lty = 2, col = "red")
legend("center",
  legend = c("Kaplan-Meier (Resíduos)", "Exponencial Padrão"),
  lty = c(1, 2), col = c("black", "red"), bty = "n", cex = 0.8
)

par(mfrow = c(1, 1))

# Interpretação

coefs <- coef(fit_reg_lognorm)

tr_unesp <- exp(coefs["CO_IES_56"])
tr_sexo <- exp(coefs["TP_SEXO"])
tr_sp <- exp(coefs["SP"])
tr_mg <- exp(coefs["MG"])
tr_sul <- exp(coefs["SUL"])
tr_indig <- exp(coefs["INDIGENA"])
tr_inter <- exp(coefs["CO_IES_55:TP_NACIONALIDADE"])

fmt_pct <- function(tr) {
  if (tr >= 1) {
    paste0(format(round((tr - 1) * 100, 1), decimal.mark = ","), "% maior")
  } else {
    paste0(format(round((1 - tr) * 100, 1), decimal.mark = ","), "% menor")
  }
}

tabela_conclusao <- data.frame(
  Variavel = c(
    "IES: UNESP",
    "Sexo (Masculino)",
    "Origem: Sao Paulo",
    "Origem: Minas Gerais",
    "Origem: Regiao Sul",
    "Raca: Indigena",
    "Interacao: USP x Estrangeiro"
  ),
  `Time Ratio` = c(
    format(round(tr_unesp, 4), decimal.mark = ","),
    format(round(tr_sexo, 4), decimal.mark = ","),
    format(round(tr_sp, 4), decimal.mark = ","),
    format(round(tr_mg, 4), decimal.mark = ","),
    format(round(tr_sul, 4), decimal.mark = ","),
    format(round(tr_indig, 4), decimal.mark = ","),
    format(round(tr_inter, 4), decimal.mark = ",")
  ),
  Interpretacao = c(
    paste("O tempo mediano de formacao na UNESP e", fmt_pct(tr_unesp), "em relacao a UFSCar."),
    paste("O tempo mediano dos homens e", fmt_pct(tr_sexo), "em relacao as mulheres."),
    paste("Alunos de SP tem tempo de formacao", fmt_pct(tr_sp), "em relacao aos demais estados."),
    paste("Alunos de MG tem tempo de formacao", fmt_pct(tr_mg), "em relacao aos demais estados."),
    paste("Alunos do Sul tem tempo de formacao", fmt_pct(tr_sul), "em relacao as demais regioes."),
    paste("Indigenas tem tempo de formacao", fmt_pct(tr_indig), "em relacao aos brancos."),
    paste("Estrangeiros na USP tem tempo", fmt_pct(tr_inter), "em relacao aos brasileiros.")
  )
)

kbl(tabela_conclusao,
  caption = "Interpretação da razão de tempos (TR) para as covariáveis significativas do modelo Log-Normal.",
  booktabs = TRUE
) |>
  kable_styling(latex_options = c("HOLD_POSITION", "basic"), position = "center")

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
dados_brutos <- broom::tidy(survcoxfit, exponentiate = FALSE)

tabela_cox_final <- dados_brutos |>
  dplyr::mutate(
    HR_numerico = exp(estimate),
    Beta_texto = format(round(estimate, 4), decimal.mark = ","),
    Erro_texto = format(round(std.error, 4), decimal.mark = ","),
    HR_texto = format(round(HR_numerico, 4), decimal.mark = ","),
    P_valor_texto = ifelse(p.value < 0.001, "< 0,001",
      format(round(p.value, 4), decimal.mark = ",")
    )
  ) |>
  dplyr::select(
    Variavel = term,
    `Coeficiente (Beta)` = Beta_texto,
    `Erro Padrao` = Erro_texto,
    `Valor-p` = P_valor_texto
  )

tabela_cox_final$Variavel <- dplyr::recode(tabela_cox_final$Variavel,
  "CO_IES_56" = "IES: UNESP",
  "TP_SEXO" = "Sexo (Masculino)",
  "CO_IES_55:TP_NACIONALIDADE" = "Interacao: USP x Estrangeiro"
)

kbl(tabela_cox_final,
  caption = "Estimativas do Modelo de Cox.",
  booktabs = TRUE,
  align = "c"
) |>
  kable_styling(latex_options = c("HOLD_POSITION", "striped"), position = "center")

# Análise de Resíduos

res_martingale <- resid(survcoxfit, type = "martingale")
res_coxsnell <- dados_modelagem$evento - res_martingale

fit_km_coxresid <- survfit(Surv(res_coxsnell, dados_modelagem$evento) ~ 1)

par(mfrow = c(1, 1))

plot(fit_km_coxresid,
  mark.time = FALSE, conf.int = FALSE,
  xlab = "Resíduos de Cox-Snell",
  ylab = "S(e) Estimada",
  main = "Aderência da Curva S(t)"
)

tempos_res <- fit_km_coxresid$time
lines(tempos_res, exp(-tempos_res), lty = 3, col = "red", lwd = 2)

legend("center",
  legend = c("Kaplan-Meier (Empírico)", "Exponencial(1) (Teórico)"),
  lty = c(1, 3), col = c("black", "red"),
  lwd = c(1, 2), bty = "n", cex = 0.8
)


st_empirica <- fit_km_coxresid$surv
st_teorica <- exp(-fit_km_coxresid$time)

plot(st_empirica, st_teorica,
  xlab = "S(e): Kaplan-Meier",
  ylab = "S(e): Exponencial(1)",
  pch = 16,
  main = "Gráfico de Probabilidades",
  xlim = c(0, 1), ylim = c(0, 1)
)

lines(c(0, 1), c(0, 1), type = "l", lty = 2, col = "red")

par(mfrow = c(1, 1))
ggcoxdiagnostics(survcoxfit,
  type = "scaledsch", linear.predictions = FALSE,
  ggtheme = theme_bw()
)

# Teste de proporcionalidade H0: é proporcional
tabela_ph <- data.frame(
  Variavel = c(
    "IES: UNESP",
    "Sexo",
    "Interacao: USP x Estrangeiro",
    "GLOBAL"
  ),
  chisq = c("0,0151", "1,7164", "0,7181", "2,4467"),
  df = c(1, 1, 1, 3),
  p = c("0,90", "0,19", "0,40", "0,48")
)

kbl(tabela_ph,
  caption = "Teste de Proporcionalidade dos Riscos (Resíduos de Schoenfeld).",
  col.names = c("Variável", "Qui-quadrado", "Graus de Liberdade", "Valor-p"),
  booktabs = TRUE,
  align = "c"
) |>
  kable_styling(latex_options = c("HOLD_POSITION", "striped"), position = "center")

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

coefs_cox <- coef(survcoxfit)
hr_unesp <- exp(coefs_cox["CO_IES_56"])
hr_sexo <- exp(coefs_cox["TP_SEXO"])
hr_inter <- exp(coefs_cox["CO_IES_55:TP_NACIONALIDADE"])

tabela_interp_cox <- data.frame(
  Variavel = c(
    "IES: UNESP",
    "Sexo (Masculino)",
    "Interacao: USP x Estrangeiro"
  ),
  `Hazard Ratio` = c(
    format(round(hr_unesp, 2), decimal.mark = ","),
    format(round(hr_sexo, 2), decimal.mark = ","),
    format(round(hr_inter, 2), decimal.mark = ",")
  ),
  Interpretacao = c(
    # UNESP
    "A taxa de formacao dos alunos da UNESP e 0,78 vezes a de alunos das demais IES (formam mais devagar).",

    # Sexo
    # Nota: HR < 1 significa que o risco (velocidade) é menor para homens.
    "A taxa de formacao de homens e 0,83 vezes a das mulheres (formam mais devagar).",

    # Interacao
    "A taxa de formacao de estrangeiros na USP e aproximadamente 78 vezes a de outros grupos."
  )
)

kbl(tabela_interp_cox,
  caption = "Interpretação prática das Razões de Risco (Hazard Ratios) do Modelo de Cox.",
  booktabs = TRUE
) |>
  kable_styling(latex_options = c("HOLD_POSITION", "striped"), position = "center")


# Gráfico para verificar a suposição de taxas de falha proporcionais
fit <- coxph(Surv(time = tempo_em_anos[TP_SEXO == 1], event = evento[TP_SEXO == 1]) ~ 1, data = dados_modelagem, method = "breslow")
ss <- survfit(fit)
s0 <- round(ss$surv, digits = 5)
H0 <- -log(s0)
plot(ss$time, log(H0),
  xlab = "Tempos", ylim = range(c(-5, 1)),
  ylab = expression(log(Lambda[0] * (t))), bty = "n", type = "s"
)
fit <- coxph(Surv(time = tempo_em_anos[TP_SEXO == 0], event = evento[TP_SEXO == 0]) ~ 1, data = dados_modelagem, method = "breslow")
ss <- survfit(fit)
s0 <- round(ss$surv, digits = 5)
H0 <- -log(s0)
lines(ss$time, log(H0), type = "s", lty = 4)
legend(1.5, -1, lty = c(4, 1), c("Masculino", "Feminino"), lwd = 1, bty = "n", cex = 0.9)
title("Sexo")

fit <- coxph(Surv(time = tempo_em_anos[BRANCA == 1], event = evento[BRANCA == 1]) ~ 1, data = dados_modelagem, method = "breslow")
ss <- survfit(fit)
s0 <- round(ss$surv, digits = 5)
H0 <- -log(s0)
plot(ss$time, log(H0),
     xlab = "Tempos", ylim = range(c(-5, 1)),
     ylab = expression(log(Lambda[0] * (t))), bty = "n", type = "s"
)
fit <- coxph(Surv(time = tempo_em_anos[BRANCA == 0], event = evento[BRANCA == 0]) ~ 1, data = dados_modelagem, method = "breslow")
ss <- survfit(fit)
s0 <- round(ss$surv, digits = 5)
H0 <- -log(s0)
lines(ss$time, log(H0), type = "s", lty = 4)
legend(1.5, -1, lty = c(4, 1), c("Outra", "Branco"), lwd = 1, bty = "n", cex = 0.9)
title("Raça/Cor")
