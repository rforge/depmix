
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
instart=c(0.5,0.5,0,0)
set.seed(1)
respstart=runif(16)
# note that ntimes argument is used to make this a mixture model
mod1 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	respstart=respstart,instart=instart,prior=~age,initdata=balance)

logLik(mod1)

# mod1 <- fit(mod)

system.time(fm1 <- fit(mod1))



mod2 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity")),
	respstart=respstart,instart=instart,prior=~age,initdata=balance)

system.time(fm2 <- fit(mod2))



# 
# 3-class models
# 

# now fit some latent class models
instart=c(0.33,0.33,0.34,0,0,0)
set.seed(1)
respstart=runif(24)
# note that ntimes argument is used to make this a mixture model
mod3 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=3,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	respstart=respstart,instart=instart,prior=~age,initdata=balance)

logLik(mod3)

# mod1 <- fit(mod)

system.time(fm3 <- fit(mod3))



mod4 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=3,
	family=list(multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity")),
	respstart=respstart,instart=instart,prior=~age,initdata=balance)

system.time(fm4 <- fit(mod4,maxit=200))



# 
# 4-class models
# 

# now fit some latent class models
instart=c(0.25,0.25,0.25,0.25,0,0,0,0)
set.seed(1)
respstart=runif(32)
# note that ntimes argument is used to make this a mixture model
mod5 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=4,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	respstart=respstart,instart=instart,prior=~age,initdata=balance)

logLik(mod5)

# mod1 <- fit(mod)

system.time(fm5 <- fit(mod5,maxit=1000)

mod6 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=4,
	family=list(multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity")),
	respstart=respstart,instart=instart,prior=~age,initdata=balance)

system.time(fm6 <- fit(mod6,maxit=500))


#  plot prior probs as function of age

x <- mlogit(base=1)
coeff <- coefficients(fm4@prior@parameters)

pr1 <- function(y) {sapply(y, function(z) {x$linkinv(c(t(coeff)%*%c(1,z)), base=1)[1]})}
pr2 <- function(y) {sapply(y, function(z) {x$linkinv(c(t(coeff)%*%c(1,z)), base=1)[2]})}
pr3 <- function(y) {sapply(y, function(z) {x$linkinv(c(t(coeff)%*%c(1,z)), base=1)[3]})}

pdf(file="balprior.pdf",width=7, height=4)

plot(pr1,min(balance$age),max(balance$age),lty=1,ylim=c(0,1),main="Prior probabilities by age, balance scale data", xlab="age", ylab="Pr")
plot(pr2,min(balance$age),max(balance$age),add=T,lty=2)
plot(pr3,min(balance$age),max(balance$age),add=T,lty=3)

legend("right",legend=c("Class 1 (correct)","Class 2 (incorrect)","Class 3 (guess)"),lty=1:3,inset=c(0.1,0))

dev.off()




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












