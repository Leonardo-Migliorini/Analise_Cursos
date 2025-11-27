library(tidyverse)
library(readr)
library(dplyr)
library(survival)
library(survminer)
library(flexsurv)
#masculino é zero e 2
#base de alunos
ALUNO_ING2010_surv <- read_csv("ALUNO_ING2010-surv.csv")

#base de cursos 2019
DM_CURSO2019 <- read.table("DM_CURSO2019.csv", header = TRUE, sep = "|")

#filtro co_cine_rotulo de agronomia
codigos_ag_cine <- DM_CURSO2019 %>%
  filter(CO_CINE_ROTULO == "0811A04") %>%
  filter(CO_UF %in% c(35))
  
#alunos de agronomia
alunos_ag <- ALUNO_ING2010_surv %>%
  filter(CO_CURSO %in% codigos_ag_cine$CO_CURSO) %>%
  filter(TP_SITUACAO %in% c(2,3, 4, 5, 6)) %>%
  filter(TP_CATEGORIA_ADMINISTRATIVA %in% c(1, 2, 3))
#tempo do aluno no sistema e segregacao de censura
dados_sobre <- alunos_ag %>%
  mutate(tempo_em_anos = NU_ANO_CENSO - 2009) %>%
  mutate(evento = ifelse(TP_SITUACAO==6, 1, 0)) %>%
  mutate(TP_NACIONALIDADE = if_else(TP_NACIONALIDADE == 1, 0, 1)) %>% 
  mutate(CO_UF_NASCIMENTO = replace_na(CO_UF_NASCIMENTO, 0)) %>%
  mutate(CO_UF_NASCIMENTO = if_else(CO_UF_NASCIMENTO == 35, "São Paulo", 
                                    if_else(CO_UF_NASCIMENTO == 31, "Minas Gerais", 
                                            if_else(CO_UF_NASCIMENTO == 33, "Rio de Janeiro", 
                                                    if_else(CO_UF_NASCIMENTO <= 29, "Norte e Nordeste", 
                                                            if_else(CO_UF_NASCIMENTO == 41, "Paraná", 
                                                                    if_else(CO_UF_NASCIMENTO == 43, "Rio Grande do Sul", "Centro-Oeste"))))))) %>%
  mutate(PRETA = if_else(TP_COR_RACA == 2, 1, 0)) %>%
  mutate(PARDA = if_else(TP_COR_RACA == 3, 1, 0)) %>%
  mutate(RACA_OUTRA = if_else(TP_COR_RACA == 6, 1, 0)) %>%
  mutate(BRANCA = if_else(TP_COR_RACA == 1, 1, 0)) %>%
  mutate(AMARELA = if_else(TP_COR_RACA == 4, 1, 0)) %>%
  mutate(INDIGENA = if_else(TP_COR_RACA == 5, 1, 0)) %>%
  mutate(CENTRO_OESTE = if_else(CO_UF_NASCIMENTO == "Centro-Oeste", 1, 0)) %>%
  mutate(SP = if_else(CO_UF_NASCIMENTO == "São Paulo", 1, 0)) %>%
  mutate(MG = if_else(CO_UF_NASCIMENTO == "Minas Gerais", 1, 0)) %>%
  mutate(NORTE_NORDESTE = if_else(CO_UF_NASCIMENTO == "Norte e Nordeste", 1, 0)) %>%
  mutate(PARANA = if_else(CO_UF_NASCIMENTO == "Paraná", 1, 0)) %>%
  mutate(SUL = if_else(CO_UF_NASCIMENTO == "Paraná", 1, if_else(CO_UF_NASCIMENTO == "Rio Grande do Sul", 1, 0))) %>%
  mutate(SUDESTE_SP = if_else(CO_UF_NASCIMENTO == "Minas Gerais", 1, if_else(CO_UF_NASCIMENTO == "Rio de Janeiro", 1, if_else(CO_UF_NASCIMENTO == "São Paulo", 1, 0)))) %>%
  mutate(SUDESTE = if_else(CO_UF_NASCIMENTO == "Minas Gerais", 1, if_else(CO_UF_NASCIMENTO == "Rio de Janeiro", 1, 0))) %>%
  mutate(CO_MUNICIPIO_NASCIMENTO = replace_na(CO_MUNICIPIO_NASCIMENTO, 0)) %>%
  mutate(NU_ANO_NASCIMENTO = 2010-NU_ANO_NASCIMENTO) %>%
  mutate(CO_PAIS_ORIGEM_1076 = if_else(CO_PAIS_ORIGEM == 10, 1, if_else(CO_PAIS_ORIGEM == 76, 1, 0))) %>%
  mutate(CO_PAIS_ORIGEM_OUTRO = if_else(CO_PAIS_ORIGEM != 10, if_else(CO_PAIS_ORIGEM != 76, 1, 0), 0)) %>%
  mutate(CO_IES_56 = if_else(CO_IES == 56, 1, 0)) %>%
  mutate(CO_IES_55 = if_else(CO_IES == 55, 1, 0)) %>%
  mutate(CO_IES_7 = if_else(CO_IES == 7, 1, 0)) %>%
  mutate(IDADE_18 = if_else(NU_ANO_NASCIMENTO <= 18, 1, 0)) %>%
  mutate(IDADE_19 = if_else(NU_ANO_NASCIMENTO == 19, 1, 0)) %>%
  mutate(IDADE_20 = if_else(NU_ANO_NASCIMENTO == 20, 1, 0)) %>%
  mutate(IDADE_21 = if_else(NU_ANO_NASCIMENTO >= 21, 1, 0)) %>%
  mutate(TP_SEXO = if_else(TP_SEXO == 1, 0, 1))

#curva de kaplan Meier
surv_obj_final <- Surv(time = dados_sobre$tempo_em_anos, event = dados_sobre$evento)
fit_km_final <- survfit(surv_obj_final ~ 1)
fit_km_final$surv

summary_km <- summary(fit_km_final)
summary_km$table
head(summary_km$surv)
summary_km

summary(fit_km_final, times = c(5, 6, 7))
quantile(fit_km_final, probs = c(0.25, 0.50, 0.75))

print(
  ggsurvplot(
    fit_km_final,
    data = dados_sobre,
    conf.int = TRUE,
    risk.table = TRUE,
    title = "Curva de Formatura: Agronomia em IES Públicas de SP",
    xlab = "Tempo em Anos desde o Ingresso",
    ylab = "Probabilidade de Permanência (Não Formado)",
    surv.median.line = "hv",
    ggtheme = theme_light()
  ))

variaveis <- list()
variaveis[[1]] = dados_sobre$CO_IES_56
variaveis[[2]] = dados_sobre$TP_SEXO
variaveis[[3]] = dados_sobre$IDADE_18
variaveis[[4]] = dados_sobre$IDADE_19
variaveis[[5]] = dados_sobre$IDADE_20
variaveis[[6]] = dados_sobre$IDADE_21
variaveis[[7]] = dados_sobre$CENTRO_OESTE
variaveis[[8]] = dados_sobre$NORTE_NORDESTE
variaveis[[9]] = dados_sobre$SP
variaveis[[10]] = dados_sobre$MG
variaveis[[11]] = dados_sobre$SUL
variaveis[[12]] = dados_sobre$PRETA
variaveis[[13]] = dados_sobre$PARDA
variaveis[[14]] = dados_sobre$BRANCA
variaveis[[15]] = dados_sobre$AMARELA
variaveis[[16]] = dados_sobre$INDIGENA
variaveis[[17]] = dados_sobre$CO_IES_55
variaveis[[18]] = dados_sobre$NU_ANO_NASCIMENTO
variaveis[[19]] = dados_sobre$CO_PAIS_ORIGEM_1076
variaveis[[20]] = dados_sobre$TP_NACIONALIDADE

varianome <- c("CO_IES_56", "TP_SEXO", "IDADE_18", "IDADE_19", "IDADE_20", "IDADE_21",
               "CENTRO_OESTE", "NORTE_NORDESTE", "SP", "MG", "SUL", "PRETA", "PARDA",
               "BRANCA", "AMARELA", "INDIGENA", "CO_IES_55", "NU_ANO_NASCIMENTO", 
               "CO_PAIS_ORIGEM_1076", "TP_NACIONALIDADE")

for (i in 1:20) {
  for (j in 1:20) {
    if (i < j) {
      dados_sobre$INTERACAO = variaveis[[i]]*variaveis[[j]]
      names(dados_sobre)[names(dados_sobre) == "INTERACAO"] <- paste0("INTERACAO",i,j)
      varianome <- append(varianome, paste0("INTERACAO",i,j))
    }}}

varianome <- c("CO_IES_56","SP", "MG", "SUL", "PARDA",
               "BRANCA", "CO_PAIS_ORIGEM_1076")
survfor <- as.formula(paste("surv_obj_final", "~", paste(varianome, collapse = " + ")))
fit_exp <- survreg(survfor, dist = "exp", data = dados_sobre)
alphaehat<-exp(fit_exp$icoef[1])
summary(fit_exp)

varianome <- c("CO_IES_56", "TP_SEXO",
               "SP", "MG", "SUL", "INDIGENA", "INTERACAO1720")

survfor <- as.formula(paste("surv_obj_final", "~", paste(varianome, collapse = " + ")))
fit_lognorm <- survreg(survfor, dist = "lognormal", data = dados_sobre)
summary(fit_lognorm)
muhat=fit_lognorm$coef[1]
sigmahat=fit_lognorm$scale
s_lnorm<-pnorm((-log(1:10)+muhat)/sigmahat)

varianome <- c("CO_IES_56", "TP_SEXO",
               "SP", "MG", "SUL", "INDIGENA", "INTERACAO1720")

survfor <- as.formula(paste("surv_obj_final", "~", paste(varianome, collapse = " + ")))
fit_weibull <- survreg(survfor, dist = "weibull", data = dados_sobre)
summary(fit_weibull)
alphawhat=exp(fit_weibull$icoef[1])
gamahat<-1/fit_weibull$scale
s_weib<-exp(-(1:10/alphawhat)^(gamahat))

varianome <- c("CO_IES_56", "TP_SEXO",
               "SP", "MG", "SUL", "INDIGENA", "INTERACAO1720")

survfor <- as.formula(paste("surv_obj_final", "~", paste(varianome, collapse = " + ")))
fit_gamma <- flexsurvreg(survfor, dist="gengamma", data = dados_sobre)
2*(fit_lognorm$loglik[2] - fit_gamma$loglik)
1-pchisq(2*(fit_lognorm$loglik[2] - fit_gamma$loglik),2)
s_gamma <- 1- pgengamma(1:10, mu=fit_gamma$coefficients[1], sigma=exp(fit_gamma$coefficients[2]), Q = fit_gamma$coefficients[3])

plot(fit_km_final$surv, s_weib, pch=16, ylim=range(c(0,1)), lim=range(c(0,1)),
     xlab="S(t): Kaplan-Meier", ylab="S(t): Weibull")
lines(c(0,1), c(0,1), type="l", lty=1)

plot(s_lnorm)
plot(fit_km_final$surv, s_lnorm, pch=16,ylim=range(c(0,1)),xlim=range(c(0,1)),
     xlab="S(t): Kaplan-Meier", ylab="S(t): log-normal")
lines(c(0,1), c(0,1), type="l", lty=1)

plot(fit_km_final$surv, s_gamma, pch=16,ylim=range(c(0,1)),xlim=range(c(0,1)),
     xlab="S(t): Kaplan-Meier", ylab="S(t): gamma")
lines(c(0,1), c(0,1), type="l", lty=1)

# 
# # varianome2 <- varianome
# k=2
# while (k==2) {
#   resul = c()
# for (a in varianome2) {
#   varianome3 = varianome2
#   varianome3 = varianome3[varianome3 != a]
#   survfor <- as.formula(paste("surv_obj_final", "~", paste(varianome3, collapse = " + ")))
#   survcoxfit = coxph(survfor, data=dados_sobre, x=T, method="breslow")
#   resul = append(resul, survcoxfit$loglik[2])
# }
#   survfor <- as.formula(paste("surv_obj_final", "~", paste(varianome2, collapse = " + ")))
#   survcoxfit = coxph(survfor, data=dados_sobre, x=T, method="breslow")
#   resul = append(resul, survcoxfit$loglik[2]-2.2)
#   fora <- which.max(resul)
#   resul
#   if (fora > length(varianome2)) {
#     k=1
#   }
#   else{
#   print(varianome2[fora])
#   varianome2 = varianome2[-fora]
#   print(length(varianome2))
#   }}

# survfor <- as.formula(paste("surv_obj_final", "~", paste(varianome2, collapse = " + ")))

survcoxfit = coxph(surv_obj_final~INTERACAO1720+CO_IES_55+INTERACAO417+
                      INTERACAO517+INTERACAO1718+INTERACAO317+TP_SEXO
                      , data=dados_sobre, x=T, method="breslow")
survcoxfit
cox.zph(survcoxfit, transform="identity")

resm<-resid(survcoxfit, type="martingale")
res<-dados_sobre$evento-resm # res´ıduo de cox-Snell
ekm <- survfit(Surv(res, dados_sobre$evento)~1)
summary(ekm)
plot(ekm, mark.time=F, conf.int=F, xlab="Res´ıduos",
     ylab="S(e) estimada")
res<-sort(res)
exp1<-exp(-res)
lines(res, exp1, lty=3)
legend(1, 0.8, lty=c(1,3), c("Kaplan Meier","Exponencial(1)"),
       lwd=1, bty="n", cex=0.7)
st<-ekm$surv
t<-ekm$time
sexp1<-exp(-t)
plot(st, sexp1, xlab="S(e): Kaplan-Meier",
     ylab= "S(e): Exponencial(1)", pch=16)

ggcoxdiagnostics(survcoxfit, type = "scaledsch", linear.predictions = FALSE,
                 ggtheme = theme_bw())
