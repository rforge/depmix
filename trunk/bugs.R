
# Bug by Maartje

library(depmixS4) 

setwd("~/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")

load("testmaartje.rda")

respstart <- c(rbind(rnorm(10,8,2),rep(0,10),runif(10,1,2),rnorm(10,8,2),rep(0,10),runif(10,1,2)))

# respstart <- runif(30)

mod1=depmix(list(i1~t,i2~t,i3~t,i4~t,i5~t,i6~t,i7~t,i8~t,i9~t,i10~t),
	data=casematrix,nstates=2,ntimes=ntimes,
	family=list(gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),
		gaussian(),gaussian(),gaussian(),gaussian()), respstart=respstart)

ll <- logLik(mod1)

fitmod1=fit(mod1)

hist(posterior(fitmod1)[,2])



# Bug by Rita Gaio

library(depmixS4)
library(Rdonlp2)
library(nnet)


mod.cov <- mix(list(sqrt(carnev)~1+CALORIES, sqrt(peixe)~1+CALORIES,
		sqrt(vegetais)~1+CALORIES, sqrt(frutos)~1+CALORIES,
		sqrt(armbat)~1+CALORIES, sqrt(sweet)~1+CALORIES,# 6 continuous variables
		sumos2~1+CALORIES, alcool2.m ~1+CALORIES),
	data=baseI,
	nstates=4,
	family=list(gaussian(), gaussian(), gaussian(),gaussian(), gaussian(), gaussian(),
		multinomial(), multinomial()),
	prior=~idade.c+CALORIES,
	initdata=baseI
) # end mod.cov
getpars(mod.cov)
print(mod.cov)

ncont <- 6 Ê# nb continuous responses
nbin <- 2 ÊÊ# nb binary responses
nmixt <- 2 ÊÊ# nb of mixtures
ncov1 <- 1 ÊÊÊ# nb covariates for densities WITHIN each cluster
ncov2 <- 2 ÊÊÊ# nb covariates affecting the components/clusters
value.cov=rep(0, nmixt*(1+ncov2)
ÊÊÊÊÊÊÊÊÊÊÊ+ ncont*(1+ncov1+1) + nbin*2*(1+ncov1)
ÊÊÊÊÊÊÊÊÊÊÊ+ ncont*(1+ncov1+1) + nbin*2*(1+ncov1),
ÊÊÊÊÊÊÊÊÊÊÊ+ ncont*(1+ncov1+1) + nbin*2*(1+ncov1),
ÊÊÊÊÊÊÊÊÊÊÊ+ ncont*(1+ncov1+1) + nbin*2*(1+ncov1) )


## Choose value.cov with random values for the means and sample sd's for the sd's ...
## deleted the vector I had since I slightly changed the model I had

mod.cov2 <- setpars(mod.cov, value=value.cov)
print(mod.cov2)

fmod.cov2 <- fit(mod.cov2, method="donlp")
## without method="donlp" we get Êiteration 0 logLik: -29803.8
## Error in lm.wfit(x = object@x, y = object@y, w = w) :
## Êmissing or negative weights not allowed

## with method="donlp": runs forever...