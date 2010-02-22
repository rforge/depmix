
# 
# Started by Ingmar Visser 17-02-2010
# 
# Usage: go to trunk directory and source("depmixNew-test6.R")
# 

setwd("~/Documents/projects/depmixProject/codesvn/depmix/trunk/")

pa2conr <-
function(x) {
	fix=as.logical(x)
	x=replace(x,list=which(x==1),0)
	un=setdiff(unique(x),0)
	y=matrix(0,0,length(x))
	for(i in un) {
		z=which(x==i)
		for(j in 2:length(z)) {
			k=rep(0,length(x))
			k[z[1]]=1
			k[z[j]]=-1
			y=rbind(y,k)
		}
	}
	pa = list(free=fix,conr=y)
	return(pa)
}


# 
# Rsolnp optimization
# 

require(depmixS4)

data(speed)

# 2-state model on rt and corr from speed data set with Pacc as covariate on the transition matrix
# starting values for the transition pars (without those EM does not get off the ground)
set.seed(1)
tr=runif(6)
trst=c(tr[1:2],0,tr[3:5],0,tr[6])
mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,family=list(gaussian(),multinomial()),
	trstart=trst)

logLik(mod1)

# mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,family=list(gaussian(),multinomial()),
# 	trstart=trst,respst=c(5,.5,.5,.5,6,.5,.1,.9))
# logLik(mod1)

# fit the model
fmod1 <- fit(mod1)
fmod1 # to see the logLik and optimization information

# to see the parameters
summary(fmod1)


require(Rsolnp)

allpars <- getpars(fmod1)

allpars[2]=Inf # this means the process will always start in state 2
allpars[14]=0 # the corr parameters in state 1 are now both 0, corresponding the 0.5 prob


lowb <- rep(-Inf,length(allpars))
lowb[c(12,16)] <- 0


fixed <- getpars(fmod1,"fixed")
fixed[2] <- TRUE
fixed[14] <- TRUE

pars <- allpars[!fixed]
lowb <- lowb[!fixed]

set.seed(2)

pars <- pars+runif(9,0,.1)
allpars[!fixed] <- pars
stmod <- setpars(fmod1,allpars)

fm1Rdon <- fit(stmod,fixed=fixed)



# define loglike function
ll <- function(pars) {
    allpars[!fixed] <- pars
    mod1 <- setpars(mod1,allpars)    
    ans = -as.numeric(logLik(mod1))
    if(is.na(ans)) ans = 100000
    ans
}

ll(pars)

# use Rsolnp for unconstrained model
res <- solnp(pars, ll, control = list(trace = 1),LB=lowb)

res$pars

fpars <- allpars
fpars[!fixed] <- res$pars

fmRsol <- setpars(mod1,fpars)

    
    
    
allpars[c(4,8)] <- -4
allpars[c(6,10)] <- 10

conpat <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,0,1)
# constrain the beta's on the transition parameters to be equal
conpat[4] <- conpat[8] <- 2
conpat[6] <- conpat[10] <- 3

eqA=pa2conr(conpat)$conr
eqA.bound=c(0,0)

eqA <- eqA[,!fixed]

lowb <- rep(-Inf,length(allpars))
lowb[c(12,16)] <- 0

pars <- allpars[!fixed]
lowb <- lowb[!fixed]

eqfun <- function(x) {
    ans = as.vector(eqA %*% x)
    ans
}

eqfun(pars)

ll(pars)

res <- solnp(pars, ll, eqfun = eqfun, eqB = eqA.bound,LB=lowb)


fm1 <- fit(mod1,conpat=conpat)

















