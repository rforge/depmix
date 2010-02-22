
# this class only contains the standard slots, no additional slots
setClass("exgaus",
	contains="response"
)

setGeneric("exgaus", function(y, pstart = NULL, 
		fixed = NULL, ...) standardGeneric("exgaus"))

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

setMethod("show","exgaus",
	function(object) {
		cat("Model of type exgaus (see ?gamlss for details) \n")
		cat("Parameters: \n")
		cat("mu: ", object@parameters$mu, "\n")
		cat("sigma: ", object@parameters$sigma, "\n")
		cat("nu: ", object@parameters$nu, "\n")
	}
)

setMethod("predict","exgaus",
	function(object) {
		object@x%*%object@parameters$mu
	}
)

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

