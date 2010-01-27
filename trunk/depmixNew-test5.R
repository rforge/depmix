
# 
# Started by Ingmar Visser 26-2-2008
# 
# Usage: go to trunk directory and source("depmixNew-test4.R")
# 

# 
# BALANCE SCALE data example with age as covariate on class membership
# 

# library(depmixS4) 

# setwd("/Users/ivisser/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")

# 
# optimization speed profile: case 1: latent class data
# 

require(depmixS4)

data(balance)

# 
# DICHOTOMOUS DATA
# 

# now fit some latent class models
instart=c(0.5,0.5)
set.seed(1)
respstart=runif(16)
# note that ntimes argument is used to make this a mixture model
mod1 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	respstart=respstart,instart=instart)

logLik(mod1)

# mod1 <- fit(mod)

system.time(mod1 <- fit(mod1))


mod1 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity")),
	respstart=respstart,instart=instart)

system.time(mod1 <- fit(mod1))




# 
# TRICHOTOMOUS DATA
# 

instart=c(0.5,0.5)
set.seed(5)
respstart=runif(24)

# note that ntimes argument is used to make this a mixture model
mod1 <- mix(list(t1~1,t2~1,t3~1,t4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	respstart=respstart,instart=instart)

logLik(mod1)

# mod1 <- fit(mod)

system.time(mod1 <- fit(mod1))

mod1

mod1 <- mix(list(t1~1,t2~1,t3~1,t4~1), data=balance, nstates=2,
	family=list(multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity")),
	respstart=respstart,instart=instart)

logLik(mod1)

system.time(mod1 <- fit(mod1))

mod1










# 
# optimization speed profile: case 1: latent class data with cov on prior
# 

data(balance)

instart=c(0.5,0.5,0,0)
respstart=c(rep(c(0.1,0.9),4),rep(c(0.9,0.1),4))
trstart=c(1,0,0,1)
mod2 <- depmix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	trstart=trstart, instart=instart, respstart=respstart,
	ntimes=rep(1,nrow(balance)), prior=~age, initdata=balance)

gc()
Rprof(file="lca2")
mod2 <- fit(mod2)
Rprof(NULL)
summaryRprof("lca2")


# 
# multivariate normal
# 


library(depmixS4)

# use function xpnd and vech from MCMCpack to convert from lower.tri to square matrix and back


# multivariate normal response model
mn <- c(1,2,3)
sig <- matrix(c(1,.5,0,.5,1,0,0,0,2),3,3)
y <- mvrnorm(1000,mn,sig)
mod <- MVNresponse(y~rnorm(1000))

head(dens(mod,log=T))

head(predict(mod))

mod <- fit(mod)
colMeans(y)
var(y)

mod

npar(mod)

require(MASS)

m1 <- c(0,1)
sd1 <- matrix(c(1,0.7,.7,1),2,2)

m2 <- c(1,0)
sd2 <- matrix(c(2,.1,.1,1),2,2)

y1 <- mvrnorm(50,m1,sd1)
y2 <- mvrnorm(50,m2,sd2)

y <- rbind(y1,y2)

rModels <- list(
	list(
		MVNresponse(y~1)
	),
	list(
		MVNresponse(y~1)
	)
)

trstart=c(0.9,0.1,0.1,0.9)

transition <- list()
transition[[1]] <- transInit(~1,nstates=2,data=data.frame(1),pstart=c(trstart[1:2]))
transition[[2]] <- transInit(~1,nstates=2,data=data.frame(1),pstart=c(trstart[3:4]))

instart=runif(2)
inMod <- transInit(~1,ns=2,ps=instart,data=data.frame(1))

mod <- makeDepmix(response=rModels,transition=transition,prior=inMod)

logLik(mod)


fm <- fit(mod)

fm <- fit(mod,meth="donlp")

fm 

summary(fm)
