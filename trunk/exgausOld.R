setClass("exgaus",
	representation(
		formula="formula",
		family="ANY"
	),
	contains="response"
)

setGeneric("exgaus", function(formula, data = NULL, pstart = NULL, 
		fixed = NULL, ...) standardGeneric("exgaus"))

setMethod("exgaus",
	signature(formula="formula"),
	function(formula,data=NULL,pstart=NULL,fixed=NULL, ...) {
		call <- match.call()
		mf <- match.call(expand.dots = FALSE)
		m <- match(c("formula", "data"), names(mf), 0)
		mf <- mf[c(1, m)]
		mf$drop.unused.levels <- TRUE
		mf[[1]] <- as.name("model.frame")
		mf <- eval(mf, parent.frame())
		x <- model.matrix(attr(mf, "terms"),mf)
		y <- model.response(mf)
		if(!is.matrix(y)) y <- matrix(y,ncol=1)
		parameters <- list()
		family <- exGAUS()
		npar <- ncol(x)+2
		if(is.null(fixed)) fixed <- as.logical(rep(0,npar))
		if(!is.null(pstart)) {
			if(length(pstart)!=npar) stop("length of 'pstart' must be ",npar)
			parameters$mu <- pstart[1:ncol(x)]
			parameters$sigma <- log(pstart[ncol(x)+1])
			parameters$nu <- log(pstart[ncol(x)+2])
		}
		mod <- new("exgaus",formula=formula,family=family,parameters=parameters,fixed=fixed,x=x,y=y,npar=npar)
		mod
	}
)

setMethod("show","exgaus",
	function(object) {
		cat("Model of type ", object@family$family[1], ", formula: ",sep="")
		print(object@formula)
		cat("Parameters: \n")
		cat("mu coefficients: \n")
		print(object@parameters$mu)
		cat("\nsigma: ", object@parameters$sigma, "\n")
		cat("nu: ", object@parameters$nu, "\n")
	}
)

setMethod("predict","exgaus",
	function(object) {
		object@family$mu.linkinv(object@x%*%object@parameters$mu)
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
		nc <- ncol(object@x)
		switch(which,
			"pars"= {
				object@parameters$mu <- values[1:nc]
				object@parameters$sigma <- (values[nc+1])
				object@parameters$nu <- (values[nc+2])
			},
			"fixed" = {
				object@fixed <- as.logical(values)
			}
		)
		names(object@parameters) <- nms
		return(object)
	}
)


