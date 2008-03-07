
# 
# Started by Ingmar Visser, march 2008
# 

# Guess what: all generics

setGeneric("depmix", function(response=any, transition=any, ...) standardGeneric("depmix"))

setGeneric("npar", function(object, ...) standardGeneric("npar"))

setGeneric("nobs", function(object, ...) standardGeneric("nobs"))

setGeneric("ntimes", function(object, ...) standardGeneric("ntimes"))

setGeneric("nstates", function(object, ...) standardGeneric("nstates"))

setGeneric("nresp", function(object, ...) standardGeneric("nresp"))

setGeneric("freepars", function(object, ...) standardGeneric("freepars"))

setGeneric("logLik", function(object, ...) standardGeneric("logLik"))

setGeneric("BIC", function(object, ...) standardGeneric("BIC"))

setGeneric("getdf",function(object) standardGeneric("getdf"))

setGeneric("GLMresponse", function(formula, ...) standardGeneric("GLMresponse"))

setGeneric("transInit", function(formula, ... ) standardGeneric("transInit"))

setGeneric("setpars", function(object,values,which="pars",...) standardGeneric("setpars"))

setGeneric("getpars", function(object,which="pars",...) standardGeneric("getpars"))

setGeneric("fit",function(object,w,...) standardGeneric("fit"))

setGeneric("logDens",function(object,...) standardGeneric("logDens"))

setGeneric("dens",function(object,...) standardGeneric("dens"))

