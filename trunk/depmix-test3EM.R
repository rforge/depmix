setwd("/Users/ivisser/Documents/projects/depmixProject/depmixNew/rforge/depmix/trunk/")


source("responses.R")
source("lystig.R")
source("depmix.R")
source("depmix.fitted.R")
source("fb.r")
source("EM.R")

load("data/speed.Rda")

trstart=c(0.896,0.104,0.084,0.916)
trstart=c(trstart[1:2],0,0.01,trstart[3:4],0,0.01)
instart=c(0,1)
resp <- c(5.52,0.202,0.472,0.528,6.39,0.24,0.098,0.902)

# intercept only (NOTE: we should fix the transInit etc function so that 
#   transition=~1 returns an x matrix of correct dimension!
mod <- depmix(list(rt~1,corr~1),data=speed,family=list(gaussian(),multinomial()),transition=~rep(1,439)-1,trstart=c(.9,.1,.1,.9),instart=instart,respst=resp,nst=2)
fmod.int <- em(mod)
logLik(fmod.int)

# now with a covariate
mod <- depmix(list(rt~1,corr~1),data=speed,family=list(gaussian(),multinomial()),transition=~Pacc,trstart=trstart,instart=instart,respst=resp,nst=2)
fmod <- em(mod,verbose=T)
logLik(fmod)

# with only multinomial response
mod <- depmix(list(corr~1),data=speed,family=list(multinomial()),transition=~Pacc,trstart=trstart,instart=instart,respst=resp[c(3,4,7,8)],nst=2)
fmod.mult <- em(mod,verbose=T)
logLik(fmod.mult)

# with only gaussian response
mod <- depmix(list(rt~1),data=speed,family=list(gaussian()),transition=~Pacc,trstart=trstart,instart=instart,respst=resp[c(1,2,5,6)],nst=2)
fmod.gauss <- em(mod,verbose=T)
logLik(fmod.gauss)
