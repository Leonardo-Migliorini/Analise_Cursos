# Ajustando separador decimal para ","
options(OutDec = ",")

# Carregando dados -------------------------------------------------------------

dados_modelagem <- arrow::read_parquet("dados_modelagem.parquet")

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

# Análise dos dados ------------------------------------------------------------

# Calculo da proporção de censuras:
summary(as.factor(dados_modelagem$TP_SITUACAO))[1]/nrow(dados_modelagem)

### Estimação Não-Paramétrica

# estimação por Kaplan-Meier
ekm <- survfit(Surv(dados_modelagem$TMP_DESFECHO, dados_modelagem$TP_SITUACAO)~1, conf.type="plain")
summary(ekm)

# Plot do Kaplan-Meier
plot(ekm, xlab="Tempo em semanas", ylab = "S(t) estimada")

### Estimação Paramétrica

# ajuste Weibull - ajusta valor extremo
(fit1 <- survreg(Surv(dados_modelagem$TMP_DESFECHO,dados_modelagem$TP_SITUACAO)~1,dist="exponen"))

(alphahat <- exp(fit1$icoef[1]))

# ajuste Weibull - ajusta valor extremo
(fit2 <- survreg(Surv(dados_modelagem$TMP_DESFECHO,dados_modelagem$TP_SITUACAO)~1,dist="weibull"))

(alphawhat <- exp(fit2$icoef[1]))

(gamahat <- 1/fit2$scale)

# ajuste Lognormal
(fit3 <- survreg(Surv(dados_modelagem$TMP_DESFECHO,dados_modelagem$TP_SITUACAO)~1,dist="lognorm"))

(muhat <- fit3$icoef[1])

(sigmahat <- fit3$scale)

# modelo gamma generalizada
fitG<-flexsurvreg(Surv(dados_modelagem$TMP_DESFECHO,dados_modelagem$TP_SITUACAO)~1,dist="gengamma")

(muhat_g <- fitG$coefficients[1])

(sigmahat_g <- fitG$coefficients[2])

(qhat_g <- fitG$coefficients[3])

# modelo Gompertz
fit_gom<-flexsurvreg(Surv(dados_modelagem$TMP_DESFECHO,dados_modelagem$TP_SITUACAO)~1,dist="gompertz")

plot(fit_gom)
plot(fitG)

# Sobrevivências estimadas usando os modelos paramétricos
(st <- ekm$surv)
time <- ekm$time

#S(t)hat Exponencial
(s_exp <- exp(-time/alphahat))

#S(t)hat Weibull
(s_weib <- exp(-(time/alphawhat)^(gamahat)))

#S(t)hat Lognormal
(s_lnorm <- pnorm((-log(time)+muhat)/sigmahat))

# Matriz com as estimativas de todos os modelos considerados
sobrevivenciahat <- round(cbind(time, st, s_exp, s_weib, s_lnorm),2)
sobrevivenciahat

# Plotando gráficos das curvas de sobrevicência de cada modelo -----------------

par(mfrow = c(1, 3))

# modelo exponencial
plot(st, s_exp, pch=16, ylim=range(c(0,1)), xlim=range(c(0,1)),
     xlab="S(t): Kaplan-Meier", ylab="S(t): Exponencial")
lines(c(0,1), c(0,1), type="l", lty=1)

# modelo Weibull
plot(st, s_weib, pch=16, ylim=range(c(0,1)), lim=range(c(0,1)),
     xlab="S(t): Kaplan-Meier", ylab="S(t): Weibull")
lines(c(0,1), c(0,1), type="l", lty=1)

# modelo Lognormal
plot(st, s_lnorm, pch=16,ylim=range(c(0,1)),xlim=range(c(0,1)),
     xlab="S(t): Kaplan-Meier", ylab="S(t): log-normal")
lines(c(0,1), c(0,1), type="l", lty=1)

# Gráficos do Método 2: linearização da função de sobrevivência.

# modelo exponencial
plot(time, -log(st), pch=16, xlab="tempos", ylab="-log(S(t))")

# modelo Weibull
plot(log(time), log(-log(st)), pch=16, xlab="log(tempos)",
     ylab="log(-log(S(t)))")

# modelo Lognormal
invst<-qnorm(st)
plot(log(time), invst, pch=16, xlab="log(tempos)",
     ylab=expression(Phi^1*S(t)))

# Teste de adequação do modelo -------------------------------------------------

# Razão de verossimilhanças
2*(fitG$loglik-fit1$loglik[2])
2*(fitG$loglik-fit2$loglik[2])
2*(fitG$loglik-fit3$loglik[2])

# pvalores do teste
(p.valor1 <- 1-pchisq(2*(fitG$loglik-fit1$loglik[2]),2))
(p.valor2 <- 1-pchisq(2*(fitG$loglik-fit2$loglik[2]),1))
(p.valor3 <- 1-pchisq(2*(fitG$loglik-fit3$loglik[2]),1))

# Gráficos para interpretação das estimativas  ---------------------------------

# Kaplan-Meier + S(t) estimada - Exponecial
plot(ekm, conf.int=F, xlab="tempos", ylab="S(t)")
lines(c(0, time), c(1, s_exp), lty=2)
legend("bottomleft",lty=c(1,2),c("Kaplan-Meier","Exponencial"),
       lwd=1,bty="n")

# Kaplan-Meier + S(t) estimada - Weibull
plot(ekm, conf.int=F, xlab="tempos", ylab="S(t)")
lines(c(0, time), c(1, s_weib), lty=2)
legend("bottomleft",lty=c(1,2),c("Kaplan-Meier","Weibull"),
       lwd=1,bty="n")

# Kaplan-Meier + S(t) estimada - Lognormal
plot(ekm, conf.int=F, xlab="tempos", ylab="S(t)")
lines(c(0, time), c(1, s_lnorm), lty=2)
legend("bottomleft",lty=c(1,2),c("Kaplan-Meier","log-normal"),
       lwd=1,bty="n")

plot(fitG)

# Estimação de quantidades de interesse ----------------------------------------

## Esperança ou tempo medio

(e_weibul <- alphawhat*(gamma(1+1/gamahat)))
(e_lnorm <- exp(muhat+(sigmahat^2/2)))

## Tempo mediano

(m_weibull<-alphawhat*(-log(1-0.5))^(1/gamahat))
(m_lnorm<-exp(muhat+(sigmahat*qnorm(0.5))))

# sobrevivência 1 ano
(s_w1<-exp(-(1/alphawhat)^(gamahat)))

(s_lnorm1<-pnorm((-log(1)+muhat)/sigmahat))

# sobrevivência 2 anos:
(s_w2<-exp(-(2/alphawhat)^(gamahat)))

(s_lnorm2<-pnorm((-log(2)+muhat)/sigmahat))

