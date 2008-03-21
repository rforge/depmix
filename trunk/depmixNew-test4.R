
# 
# Started by Ingmar Visser 26-2-2008
# 
# Usage: go to trunk directory and source this file in R
# 

# 
# BALANCE SCALE data example with age as covariate on class membership
# 

library(depmixS4) 

setwd("/Users/ivisser/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")


data(balance)

# 'log Lik.' -1083.036 (df=9)

# 
# Add age as covariate on class membership
# 

instart=c(0.5,0.5,0,0)
respstart=c(rep(c(0.1,0.9),4),rep(c(0.9,0.1),4))
trstart=c(1,0,0,1)
mod2 <- depmix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	trstart=trstart, instart=instart, respstart=respstart,
	ntimes=rep(1,nrow(balance)), prior=~age, initdata=balance)

fixed <- c(1,0,1,0,1,1,1,1,rep(c(1,0),8))
mod3 <- fit(mod2,fixed=fixed)



