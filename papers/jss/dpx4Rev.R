###################################################
### chunk number 1: 
###################################################
library(depmixS4)
data(speed)
plot(as.ts(speed[1:168,]),main="Speed-accuracy trade-off")


###################################################
### chunk number 2: 
###################################################
library(depmixS4)
data(speed)
set.seed(1)
mod <- depmix(rt~1, data=speed, nstates=2, trstart=runif(4))


###################################################
### chunk number 3: 
###################################################
fm <- fit(mod)


###################################################
### chunk number 4: 
###################################################
fm


###################################################
### chunk number 5: 
###################################################
summary(fm)


###################################################
### chunk number 6: 
###################################################
set.seed(1)
mod <- depmix(rt~1, data=speed, nstates=2, family=gaussian(), 
    transition=~scale(Pacc), instart=runif(2))
fm <- fit(mod)


###################################################
### chunk number 7: 
###################################################
summary(fm)


###################################################
### chunk number 8: 
###################################################
set.seed(1)
mod <- depmix(list(rt~1,corr~1), data=speed, nstates=2, 
    family=list(gaussian(),multinomial()),
    transition=~scale(Pacc),instart=runif(2))
fm <- fit(mod)


###################################################
### chunk number 9: 
###################################################
summary(fm)


###################################################
### chunk number 10: 
###################################################
data(balance)
balance$age <- balance$age-5
set.seed(1)
mod <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
    family=list(multinomial(), multinomial(), multinomial(),
    multinomial()), respstart=c(rep(c(0.9,0.1),4),rep(c(0.1,0.9),4)), 
    prior=~age, initdata=balance)
fm <- fit(mod)


###################################################
### chunk number 11: 
###################################################
summary(fm)


###################################################
### chunk number 12: 
###################################################
setpars(mod, value=1:npar(mod))


###################################################
### chunk number 13: 
###################################################
setpars(mod, getpars(mod,which="fixed"))



###################################################
### chunk number 14: 
###################################################
trst <- c(0.9,0.1,0,0,0.1,0.9,0,0)
mod <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,
	  nstates=2,family=list(gaussian(),multinomial()),
	  trstart=trst,inst=c(.999,0.001))
fm1 <- fit(mod)


###################################################
### chunk number 15: 
###################################################
# start with fixed and free parameters
conpat <- c(0,1,rep(c(0,1),4),1,1,0,1,1,1,0,1)
# constrain the beta's on the transition parameters to be equal
conpat[6] <- conpat[10] <- 2
fm2 <- fit(fm1,equal=conpat)


###################################################
### chunk number 16: 
###################################################
setClass("exgaus", contains="response")


###################################################
### chunk number 17: 
###################################################
setGeneric("exgaus", function(y, pstart = NULL, fixed = NULL, ...) 
  standardGeneric("exgaus"))

require(gamlss)
require(gamlss.dist)

setMethod("exgaus", 
  signature(y="ANY"), 
  function(y,pstart=NULL,fixed=NULL, ...) {
    y <- matrix(y,length(y))
    x <- matrix(1) 
    parameters <- list()
    npar <- 3
    if(is.null(fixed)) fixed <- as.logical(rep(0,npar))
    if(!is.null(pstart)) {
      if(length(pstart)!=npar) stop("length of 'pstart' must be ",npar)
      parameters$mu <- pstart[1]
      parameters$sigma <- log(pstart[2])
      parameters$nu <- log(pstart[3])
    }
    mod <- new("exgaus",parameters=parameters,fixed=fixed,x=x,y=y,npar=npar)
    mod
  }
)


###################################################
### chunk number 18: 
###################################################
setMethod("dens","exgaus",
    function(object,log=FALSE) {
        dexGAUS(object@y, mu = predict(object), sigma = exp(object@parameters$sigma), nu = exp(object@parameters$nu), log = log)
    }
)

setMethod("getpars","response",
    function(object,which="pars",...) {
        switch(which,
            "pars" = {
                parameters <- numeric()
                parameters <- unlist(object@parameters)
                pars <- parameters
            },
            "fixed" = {
                pars <- object@fixed
            }
        )
        return(pars)
    }
)

setMethod("setpars","exgaus",
    function(object, values, which="pars", ...) {
        npar <- npar(object)
        if(length(values)!=npar) stop("length of 'values' must be",npar)
        # determine whether parameters or fixed constraints are being set
		nms <- names(object@parameters)
		switch(which,
		  "pars"= {
		      object@parameters$mu <- values[1]
		      object@parameters$sigma <- values[2]
		      object@parameters$nu <- values[3]
		      },
		  "fixed" = {
		      object@fixed <- as.logical(values)
		  }
		  )
        names(object@parameters) <- nms
        return(object)
    }
)

setMethod("predict","exgaus", 
    function(object) {
        ret <- object@parameters$mu
        return(ret)
    }
)



###################################################
### chunk number 19: 
###################################################
setMethod("fit","exgaus",
  function(object,w) {
    if(missing(w)) w <- NULL
    y <- object@y
    fit <- gamlss(y~1,weights=w,family=exGAUS(),
      control=gamlss.control(n.cyc=100,trace=FALSE),
      mu.start=object@parameters$mu,
      sigma.start=exp(object@parameters$sigma),
      nu.start=exp(object@parameters$nu))
    pars <- c(fit$mu.coefficients,fit$sigma.coefficients,fit$nu.coefficients)
    object <- setpars(object,pars)
    object
  }
)


###################################################
### chunk number 20: 
###################################################
rModels <- list( list( 
		exgaus(speed$rt,pstart=c(5,.1,.1)),	
		GLMresponse(formula=corr~1,data=speed,family=multinomial(),pstart=c(0.5,0.5))),
	list(
		exgaus(speed$rt,pstart=c(6,.1,.1)),
		GLMresponse(formula=corr~1,data=speed,family=multinomial(),pstart=c(.1,.9))))


###################################################
### chunk number 21: 
###################################################
trstart=c(0.9,0.1,0.1,0.9)
transition <- list()
transition[[1]] <- transInit(~Pacc,nst=2,data=speed,pstart=c(0.9,0.1,0,0))
transition[[2]] <- transInit(~Pacc,nst=2,data=speed,pstart=c(0.1,0.9,0,0))
inMod <- transInit(~1,ns=2,pstart=c(0.1,0.9),data=data.frame(1))


###################################################
### chunk number 22: 
###################################################
mod <- makeDepmix(response=rModels,transition=transition,
    prior=inMod,stat=FALSE)
fm <- fit(mod)


