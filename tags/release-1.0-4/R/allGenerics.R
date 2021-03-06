
# 
# Ingmar Visser, 23-3-2008
# 

.First.lib <- function(lib, pkg) { 
	require(stats)
	require(methods)
	require(MASS)
 	require(nnet)
	require(Rsolnp)
	require(stats4)	
}

.Last.lib <- function(libpath) {}

# Guess what: all generics

setGeneric("depmix", function(response,data=NULL,nstates,transition=~1,family=gaussian(),prior=~1,initdata=NULL,
		respstart=NULL,trstart=NULL,instart=NULL,ntimes=NULL, ...) standardGeneric("depmix"))

setGeneric("GLMresponse", function(formula, data = NULL, family = gaussian(), pstart =
                 NULL, fixed = NULL, prob=TRUE, ...) standardGeneric("GLMresponse"))
                 
setGeneric("MVNresponse", function(formula, data = NULL,pstart=NULL,fixed=NULL,...) standardGeneric("MVNresponse"))

setGeneric("transInit", function(formula, nstates, data = NULL, family = multinomial(),
                 pstart = NULL, fixed = NULL, prob=TRUE, ...) standardGeneric("transInit"))

# extractors/set functions

setGeneric("npar", function(object, ...) standardGeneric("npar"))

setGeneric("ntimes", function(object, ...) standardGeneric("ntimes"))

setGeneric("nstates", function(object, ...) standardGeneric("nstates"))

setGeneric("nresp", function(object, ...) standardGeneric("nresp"))

setGeneric("freepars", function(object, ...) standardGeneric("freepars"))

setGeneric("getdf",function(object) standardGeneric("getdf"))

setGeneric("nlin", function(object, ...) standardGeneric("nlin"))

setGeneric("getConstraints", function(object, ...) standardGeneric("getConstraints"))

setGeneric("is.stationary", function(object,...) standardGeneric("is.stationary"))

setGeneric("setpars", function(object,values,which="pars",...) standardGeneric("setpars"))

setGeneric("getpars", function(object,which="pars",...) standardGeneric("getpars"))


# functions 
setGeneric("fit", function(object, ...) standardGeneric("fit"))

setGeneric("posterior", function(object, ...) standardGeneric("posterior"))

setGeneric("forwardbackward", function(object, ...) standardGeneric("forwardbackward"))

setGeneric("simulate", function(object,nsim=1,seed=NULL, ...) standardGeneric("simulate"))

setGeneric("logDens",function(object,...) standardGeneric("logDens"))

setGeneric("dens",function(object,...) standardGeneric("dens"))

setGeneric("predict", function(object, ...) standardGeneric("predict"))


# redundant??

# setGeneric("getModel", function(object, ...) standardGeneric("getModel"))

# these are imported from stats4
# setGeneric("nobs", function(object, ...) standardGeneric("nobs"))
# setGeneric("logLik", function(object, ...) standardGeneric("logLik"))
# setGeneric("AIC", function(object, ..., k=2) standardGeneric("AIC"))
# setGeneric("BIC", function(object, ...) standardGeneric("BIC"))
