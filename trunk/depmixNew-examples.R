
# 
# Started by Ingmar Visser may 4, 2009
# 

# 
# add response model for exgaus responses
# 

setwd("~/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")

library(depmixS4)
library(gamlss)
library(gamlss.dist)

data(speed)
rt <- speed$rt

source("exgaus.R")

# 
# test basic exgaus function: this should reproduce the gamlss output
# 

mod <- exgaus(rt,pstart=c(5,.1,.1))
fm <- fit(mod)
mod1 <- gamlss(rt~1,family=exGAUS(),control=gamlss.control(n.cyc=100),
	mu.st=4.5,sigma.st=0.1,nu.st=0.7)

fm
mod1

# 
# test the weighted version of this
# 

w=runif(439)
fm <- fit(mod,w)
mod1 <- gamlss(rt~1,family=exGAUS(),control=gamlss.control(n.cyc=100),
	mu.st=4.5,sigma.st=0.1,nu.st=0.7,weights=w)

fm
mod1



# 
# test a hidden Markov model with exgaus responses
# 

rModels <- list(
		list(
				exgaus(rt,pstart=c(5,.1,.1)),
				GLMresponse(formula=corr~1,data=speed,family=multinomial(),pstart=c(0.5,0.5))
		),
		list(
				exgaus(rt,pstart=c(6,.1,.1)),
				GLMresponse(formula=corr~1,data=speed,family=multinomial(),pstart=c(.1,.9))
		)
)

trstart=c(0.9,0.1,0.1,0.9)

transition <- list()
transition[[1]] <- transInit(~Pacc,nstates=2,data=speed,pstart=c(trstart[1:2],0,0))
transition[[2]] <- transInit(~Pacc,nstates=2,data=speed,pstart=c(trstart[3:4],0,0))

instart=c(0.5,0.5)
inMod <- transInit(~1,ns=2,ps=instart,data=data.frame(rep(1,3)))

mod <- makeDepmix(response=rModels,transition=transition,prior=inMod,ntimes=attr(speed,"ntimes"),stat=FALSE)

logLik(mod)

fm1 <- fit(mod)
fm2 <- fit(mod,meth="donlp")

summary(fm1)
summary(fm2)