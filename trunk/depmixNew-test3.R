
# 
# Started by Ingmar Visser 26-2-2008
# 
# Usage: go to trunk directory and source this file in R
# 

# 
# BALANCE SCALE data example with age as covariate on class membership
# 

setwd("/Users/ivisser/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")

load("data/balance.rda")

source("responses.R")
source("depmix.R")
source("depmix.fitted.R")

source("llratio.R")
source("lystig.R")
source("fb.R")

# now fit some latent class models
trstart=c(1,0,0,1)
instart=c(0.5,0.5)

# ntimes is added as an argument

mod <- depmix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	trstart=trstart,instart=instart,ntimes=rep(1,nrow(balance)))

pars <- getpars(mod)
fixed <- c(1,0,1,1,1,1,rep(c(1,0),8))

mod1 <- fit(mod,fixed=fixed)

# 'log Lik.' -1083.036 (df=9)

logLik(mod1)
AIC(mod1)
BIC(mod1)

# 
# Add age as covariate on class membership
# 

instart=c(0.5,0.5,0,0)
mod2 <- depmix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	trstart=trstart, instart=instart, ntimes=rep(1,nrow(balance)), 
	prior=~age, initdata=balance)

fixed <- c(1,0,1,0,1,1,1,1,rep(c(1,0),8))
mod2 <- fit(mod2,fixed=fixed)

logLik(mod2)
AIC(mod2)
BIC(mod2)

llratio(mod2,mod1)


predict(mod2@response[[1]][[1]])[1,]
predict(mod2@response[[1]][[2]])[1,]
predict(mod2@response[[1]][[3]])[1,]
predict(mod2@response[[1]][[4]])[1,]

predict(mod2@response[[2]][[1]])[1,]
predict(mod2@response[[2]][[2]])[1,]
predict(mod2@response[[2]][[3]])[1,]
predict(mod2@response[[2]][[4]])[1,]


plot.multinomial <- function(object,var=1) {	
	base=1
	coef <- object@parameters$coefficients[,-base]
	print(coef)
	range=range(object@x[,2])
	print(range) 
	linv <- function(x) {
		invlogit(coef[2]*(x+coef[1]))
	}
	plot(linv,xlim=range)
	return(range)
}

logit <- function(p) {
	log(p/(1-p))
}

invlogit <- function(x) {
	exp(x)/(1+exp(x))
}




