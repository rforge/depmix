
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












