
# 
# Started by Ingmar Visser 17-02-2010
# 
# Usage: go to trunk directory and source("depmixNew-test6.R")
# 

setwd("~/Documents/projects/depmixProject/codesvn/depmix/trunk/")

require(depmixS4)

data(speed)


library(depmixS4)

set.seed(1)
# generate data from two different multivariate normal distributions
m1 <- c(0,1)
sd1 <- matrix(c(1,0.7,.7,1),2,2)
m2 <- c(1,0)
sd2 <- matrix(c(2,.1,.1,1),2,2)
y1 <- mvrnorm(50,m1,sd1)
y2 <- mvrnorm(50,m2,sd2)
y <- rbind(y1,y2)

# now use makeDepmix to create a depmix model for this bivariate normal timeseries
rModels <-  list()
rModels[[1]] <- list(MVNresponse(y~1))
rModels[[2]] <- list(MVNresponse(y~1))

trstart=c(0.9,0.1,0.1,0.9)

transition <- list()
transition[[1]] <- transInit(~1,nstates=2,data=data.frame(1),pstart=c(trstart[1:2]),fam=multinomial("identity"))
transition[[2]] <- transInit(~1,nstates=2,data=data.frame(1),pstart=c(trstart[3:4]),fam=multinomial("identity"))

instart=runif(2)
inMod <- transInit(~1,ns=2,ps=instart,data=data.frame(1),fam=multinomial("identity"))

mod <- makeDepmix(response=rModels,transition=transition,prior=inMod)

fm2 <- fit(mod)

fm2 <- fit(mod,meth="rsolnp")





data(speed)

set.seed(1)
tr=runif(6)
trst=c(tr[1:2],0,tr[3:5],0,tr[6])
mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
	family=list(gaussian(),multinomial("identity")),trstart=trst)
# fit the model
fmod1 <- fit(mod1)
fmod1 # to see the logLik and optimization information
# to see the parameters
summary(fmod1)

# FIX SOME PARAMETERS

pars <- c(unlist(getpars(fmod1)))

pars[1]=0
pars[2]=1 # this means the process will always start in state 2
pars[13]=0.5
pars[14]=0.5 # the corr parameters in state 1 are now both 0, corresponding the 0.5 prob
mod2 <- setpars(mod1,pars)

# fix the parameters by setting: 
free <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,1,1)
# fit the model
fmod2 <- fit(mod2,fixed=!free)

# likelihood ratio insignificant, hence fmod2 better than fmod1
llratio(fmod1,fmod2)

# NOW ADD SOME GENERAL LINEAR CONSTRAINTs

pars <- c(unlist(getpars(fmod2)))
pars[4] <- pars[8] <- -4
pars[6] <- pars[10] <- 10
mod3 <- setpars(mod2,pars)

logLik(mod3)

# start with fixed and free parameters
conpat <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,1,1)
# constrain the beta's on the transition parameters to be equal
conpat[4] <- conpat[8] <- 2
conpat[6] <- conpat[10] <- 3

fmod3 <- fit(mod3,equal=conpat)










rsolnp control list:

rho
Penalty parameter (default 1)
outer.iter
Maximum number of major (outer) iterations (default 400)
inner.iter
Maximum number of minor (inner) iterations (default 800)
delta
Relative step size in forward difference evaluation (default 1.0e-8)
tol
Tolerance on feasibility and optimality (default 1e-6)
trace
The value of the objective function and the parameters is printed at every major iteration (default 0)





