
# 
# Started by Ingmar Visser 16-10-2007
# 
# Usage: go to trunk directory and source this file in R, if the program
# still works it should return TRUE at every test (or make immediate sense
# otherwise)

# Changes: 

# load depmixNew

setwd("/Users/ivisser/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")

source("depmixS4.r")
source("classes.r")
source("hmModel.R")
source("fithmModel.R")
source("llratio.R")
source("lystig.R")
source("fb.R")
source("EM.R")

# 
# define latent class model
# 

load("data/balance.Rda")

rModels <- list(
  list(
	rModel(formula=d1~1,data=balance,family=multinomial(),pstart=c(0.9,0.1)),
	rModel(formula=d2~1,data=balance,family=multinomial(),pstart=c(0.9,0.1)),
	rModel(formula=d3~1,data=balance,family=multinomial(),pstart=c(0.9,0.1)),
	rModel(formula=d4~1,data=balance,family=multinomial(),pstart=c(0.9,0.1))),
  list(
	rModel(formula=d1~1,data=balance,family=multinomial(),pstart=c(0.1,0.9)),
	rModel(formula=d2~1,data=balance,family=multinomial(),pstart=c(0.1,0.9)),
	rModel(formula=d3~1,data=balance,family=multinomial(),pstart=c(0.1,0.9)),
	rModel(formula=d4~1,data=balance,family=multinomial(),pstart=c(0.1,0.9)))
)

trstart=c(1,0,0,1)
instart=c(0.262,0.738)

mod <- depmix(rModels=rModels,data=balance,trstart=trstart,instart=instart,ntimes=rep(1,779))

# optimize using donlp
pars <- getpars(mod)
fixed <- c(1,0,1,1,1,1,rep(c(1,0),8))
mod1 <- fit(mod,fixed=fixed)
logLik(mod1)

# 'log Lik.' -1083.036 (df=9)

source("EM.R")

# optimize using em
logLik(mod,meth="fb")
fmod <- em(mod,ver=T)




# 
# example with a covariate on the initial state probs
# 

sumscores <- rowSums(balance)

set.seed(12)

age <- (sumscores+12)/2+rnorm(472,sd=1)
cor(age,sumscores)

balance$age <- age

instart=c(0.262,0.738,0,0.01)

# ntimes is added as an argument as the attribute ntimes for speed is different from this
mod <- depmix(rModels=rModels,init=~age,instart=instart,data=balance,
	trstart=trstart,ntimes=rep(1,472))

logLik(mod)

# logLik is now better than the original -898.3968 due to introduction of
# the covariate on the init probs











