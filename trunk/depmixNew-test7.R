
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

# Bovenstaande geeft warnings en uiteindelijk error in qr.default() ...




fm2 <- fit(mod,meth="donlp")
















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





