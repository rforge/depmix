
# 
# Started by Ingmar Visser 29-01-2010
# 
# Usage: go to trunk directory and source("depmixNew-test5.R")
# 


# 
# multinomial response model with identity link
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
# multivariate normal mixture models
# 



library(depmixS4)
# use function xpnd and vech from MCMCpack to convert from lower.tri to square matrix and back

# multivariate normal response model
mn <- c(1,2,3)
sig <- matrix(c(1,.5,0,.5,1,0,0,0,2),3,3)
y <- mvrnorm(1000,mn,sig)
mod <- MVNresponse(y~rnorm(1000))

y <- simulate(mod)

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

m1 <- MVNresponse(y~1,pst=c(0,.1,1,0.1,1))

m2 <- MVNresponse(y~1)

m1 

m1@parameters

m2 

m2@parameters

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
fmd <- fit(mod,meth="donlp")

pem <- getpars(fm)[7:16]
pdon <- getpars(fmd)[7:16]

all.equal(pem,pdon)

fm <- simulate(fm)

fm <- fit(fm)

fm 

summary(fm)
