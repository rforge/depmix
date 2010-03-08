
# 
# Started by Ingmar Visser 17-02-2010
# 
# Usage: go to trunk directory and source("depmixNew-test6.R")
# 

setwd("~/Documents/projects/depmixProject/codesvn/depmix/trunk/")

require(depmixS4)

data(speed)

# 2-state model on rt and corr from speed data set with Pacc as covariate on the transition matrix
# starting values for the transition pars (without those EM does not get off the ground)
set.seed(1)
tr=runif(6)
trst=c(tr[1:2],0,tr[3:5],0,tr[6])
mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,family=list(gaussian(),multinomial()),
	trstart=trst)

logLik(mod1)

# fit the model
fmod1 <- fit(mod1)
fmod1 # to see the logLik and optimization information

# to see the parameters
summary(fmod1)

summary(fmod1,which="prior")

summary(fmod1,which="transition")

summary(fmod1,which="response")

data(balance)
balance$age <- balance$age-5
set.seed(1)
mod <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
    family=list(multinomial(), multinomial(), multinomial(),
    multinomial()), respstart=c(rep(c(0.9,0.1),4),rep(c(0.1,0.9),4)), 
    prior=~age, initdata=balance)
fm <- fit(mod)

summary(fm,which="all")

summary(fm,which="prior")

summary(fm,which="transition")

summary(fm,which="response")




allpars <- getpars(fmod1)

allpars[2]=1 # this means the process will always start in state 2
allpars[1]=0
allpars[14]=0 # the corr parameters in state 1 are now both 0, corresponding the 0.5 prob

allpars[c(4,8)] <- -4
allpars[c(6,10)] <- 10

stmod <- setpars(fmod1,allpars)

conpat <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,0,1)
# constrain the beta's on the transition parameters to be equal
conpat[4] <- conpat[8] <- 2
conpat[6] <- conpat[10] <- 3

logLik(stmod)

# source("R/depmixfit.R")

fm1don <- fit(stmod,equal=conpat,method="donlp")


fm1sol <- fit(stmod,equal=conpat,method="rsolnp")

fm1sol
fm1don

summary(fm1sol)
summary(fm1don)

getpars(fm1sol)
getpars(fm1don)

all.equal(getpars(fm1sol), getpars(fm1don))





# 
# Constraints on multinomial identity models
#

library(depmixS4)

data(balance)
# four binary items on the balance scale task

instart=c(1,2)
set.seed(1)
respstart=runif(16)
# note that ntimes argument is used to make this a mixture model
mod4 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity")),
	respstart=respstart,instart=runif(2))

freepars(mod4)

fmod4 <- fit(mod4)

freepars(fmod4)

fmod4sol <- fit(mod4, meth="rsolnp")

fmod4don <- fit(mod4, meth="donlp")





library(depmixS4)

data(speed)

mod1 <- depmix(list(rt~1,corr~1),data=speed,nstates=2,
	family=list(gaussian(),multinomial("identity")),trstart=runif(4),
	respst=c(5,.2,.5,.5,6.5,.2,.1,.9))


mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
	family=list(gaussian(),multinomial("identity")),trstart=c(.9,.1,0,10,.9,.1,0,15),
	respst=c(5,.2,.5,.5,6.5,.2,.1,.9))

logLik(mod1)

fmod1 <- fit(mod1)

# fmod1sol <- fit(mod1,meth="rsolnp")
 
# fmod1don <- fit(mod1,meth="donlp")

# 
# summary(fmod1don)


## Not run: 
# NOTE: this requires Rdonlp2 package to be installed

# FIX SOME PARAMETERS

# get the starting values of this model to the optimized 
# values of the previously fitted model to speed optimization

pars <- c(unlist(getpars(fmod1)))

# constrain the initial state probs to be 0 and 1 
# also constrain the guessing probs to be 0.5 and 0.5 
# (ie the probabilities of corr in state 1)
# change the ones that we want to constrain
pars[1]=0
pars[2]=1 # this means the process will always start in state 2
pars[13]=0.5
pars[14]=0.5 # the corr parameters in state 1 are now both 0.5

mod2 <- setpars(mod1,pars)

logLik(mod2)

# fix the parameters by setting: 
free <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,1,1)
# fit the model

fmod2 <- fit(mod2,fixed=!free)

logLik(fmod2)

pars <- c(unlist(getpars(fmod2)))

pars[4] <- -4
pars[8] <- -4
pars[6] <- 10
pars[10] <- 10

mod3 <- setpars(mod2,pars)

logLik(mod3)

# start with fixed and free parameters
conpat <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,1,1)
# constrain the beta's on the transition parameters to be equal
conpat[4] <- conpat[8] <- 2
conpat[6] <- conpat[10] <- 3

fmod3 <- fit(mod3,equal=conpat)

fmod3

summary(fmod3)














