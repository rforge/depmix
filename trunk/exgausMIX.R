library(gamlss)
library(gamlss.dist)
library(gamlss.mx)
# rt <- scan("C:/gamlss/new functions/mixtures/rt.txt")

library(depmixS4)
data(speed)
rt <- speed$rt

#rtmix <- gamlssMXfits(n=5,rt~1,family=list(exGAUS(),exGAUS()),K=2,
# ÊÊÊÊg.control=gamlss.control(trace=FALSE,n.cyc=200),control=MX.control(trace=TRUE))


#rtmix <- gamlssMX(rt~1,family=list(exGAUS(),exGAUS()),K=2)


source("gamlssMXfits.R")

rtNO <- gamlssMX(rt~1,K=2) ## two normal
rtexG <- gamlssMX(rt~1,family=c(exGAUS, exGAUS),K=2)# two exGAUS
rtT <- gamlssMX(rt~1,family=c(TF, TF),K=2) # two t


# AIC
GAIC(rtNO, rtexG, rtT)

# SBC-BIC
k=log(length(rt))
GAIC(rtNO,rtexG,rtT,k=k)

# deviance -2*log(lik) 
GAIC(rtNO, rtexG, rtT, k=0)
# the log likelihood
logLik(rtT)
'log Lik.' -196.8658 (df=7)


library(MASS)
truehist(rt, nbins=20)

frtNO <- dMX(y=seq(4, 8, 0.01), mu=list(coef(rtNO, K=1), Êcoef(rtNO, K=2)), sigma=list(exp(coef(rtNO, K=1, what="sigma")), exp( coef(rtNO, K=2, what="sigma")))
ÊÊÊÊÊÊÊÊÊÊÊÊÊÊÊ,pi=rtNO$prob, family=list("NO", "NO") )
lines(seq(4, 8, 0.01), frtNO, col="green", lty=1)


frtexG <- dMX(y=seq(4, 8, 0.01), mu=list( coef(rtexG, K=1), Êcoef(rtexG, K=2)), sigma=list(exp(coef(rtexG, K=1, what="sigma")), exp( coef(rtexG, K=2, what="sigma"))),
ÊÊÊÊÊÊÊÊÊÊÊnu=list(exp(coef(rtexG, K=1, what="nu")), exp( coef(rtexG, K=2, what="nu"))), pi=rtexG$prob, family=list("exGAUS", "exGAUS") )
lines(seq(4, 8, 0.01), frtexG, col="blue", lty=1)


frt2 <- dMX(y=seq(4, 8, 0.01), mu=list( coef(rtT, K=1), Êcoef(rtT, K=2)), sigma=list(exp(coef(rtT, K=1, what="sigma")), exp( coef(rtT, K=2, what="sigma"))),
ÊÊÊÊÊÊÊÊÊÊÊnu=list(exp(coef(rtT, K=1, what="nu")), exp( coef(rtT, K=2, what="nu"))), pi=rtT$prob, family=list("TF", "TF") )
lines(seq(4, 8, 0.01), frt2, col="red", lty=1)

rtexG <- gamlssMXfits(n=5,rt~1,family=c(exGAUS, exGAUS),K=2)# two exGAUS

