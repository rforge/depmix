
# 
# Started by Ingmar Visser 26-2-2008
# 
# Usage: go to trunk directory and source this file in R
# 

# 
# BALANCE SCALE data example with age as covariate on class membership
# 

setwd("/Users/ivisser/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")

library(depmixS4)

data(balance)
# four binary items on the balance scale task

# now fit some latent class models
trstart=c(1,0,0,1) # as this is a latent class model, the transition are not optimized
instart=c(0.5,0.5)
respstart=runif(16)
# note that ntimes argument is used to make this a mixture model
mod <- depmix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	respstart=respstart,trstart=trstart,instart=instart,
	ntimes=rep(1,nrow(balance)))

mod1 <- fit(mod)

# add age as covariate on class membership by using the prior argument
trstart=c(1,0,0,1) # as this is a latent class model, the transition are not optimized
instart=c(0.5,0.5,0,0) # we need the initial probs and the coefficients of age 
respstart=c(rep(c(0.1,0.9),4),rep(c(0.9,0.1),4))
trstart=c(1,0,0,1)
mod2 <- depmix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	trstart=trstart, instart=instart, respstart=respstart,
	ntimes=rep(1,nrow(balance)), prior=~age, initdata=balance)

mod3 <- fit(mod2)

llratio(mod3,mod1)



# donlp optimzation instead

# mod4 <- fit(mod2,fixed=fixed,method="donlp")





