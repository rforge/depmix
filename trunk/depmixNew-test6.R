
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

allpars <- getpars(fmod1)

allpars[2]=Inf # this means the process will always start in state 2
allpars[14]=0 # the corr parameters in state 1 are now both 0, corresponding the 0.5 prob

allpars[c(4,8)] <- -4
allpars[c(6,10)] <- 10

stmod <- setpars(fmod1,allpars)

conpat <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,0,1)
# constrain the beta's on the transition parameters to be equal
conpat[4] <- conpat[8] <- 2
conpat[6] <- conpat[10] <- 3

fm1sol <- fit(stmod,equal=conpat,method="rsolnp")
fm1don <- fit(stmod,equal=conpat,method="donlp")

fm1sol
fm1don

summary(fm1sol)
summary(fm1don)

getpars(fm1sol)
getpars(fm1don)

all.equal(getpars(fm1sol), getpars(fm1don))
















