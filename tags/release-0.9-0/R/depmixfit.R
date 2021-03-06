
setMethod("fit",
    signature(object="mix"),
    function(object,fixed=NULL,equal=NULL,conrows=NULL,conrows.upper=0,conrows.lower=0,method=NULL,em.control=list(tol=1e-8,crit=c("relative","absolute"),random.start=FALSE),verbose=TRUE,...) {
	
		fi <- !is.null(fixed)
		cr <- !is.null(conrows)
		eq <- !is.null(equal)
		
		constr <- any(c(fi,cr,eq))
		
		# when there are constraints donlp/solnp should be used
		# otherwise EM is good
		if(is.null(method)) {
			if(constr) {
				method="rsolnp"
			} else {
				method="EM"
			}
		} else {
			if(method=="EM") {
				if(constr) {
					warning("EM not applicable for constrained models; optimization method changed to 'donlp'")
					method="donlp"
				}
			}
		}
		
		# check feasibility of starting values
		if(is.nan(logLik(object))) stop("Initial model infeasible, log likelihood is NaN; please provide better starting values. ")
		
		if(method=="EM") {
			if(is.null(em.control$tol)) em.control$tol <- 1e-8
			if(is.null(em.control$crit)) em.control$crit <- "relative"
			if(is.null(em.control$random.start)) em.control$random.start <- FALSE
			object <- em(object,tol=em.control$tol,crit=em.control$crit,random.start=em.control$random.start,verbose=verbose,...)
		}
		
		if(method=="donlp"||method=="rsolnp") {
			
			# determine which parameters are fixed
 			if(fi) {
 				if(length(fixed)!=npar(object)) stop("'fixed' does not have correct length")
 			} else {
 				if(eq) {
 					if(length(equal)!=npar(object)) stop("'equal' does not have correct length")
 					fixed <- !pa2conr(equal)$free
 				} else {
 					fixed <- getpars(object,"fixed")
 				}
 			}
			
			# set those fixed parameters in the appropriate submodels
			object <- setpars(object,fixed,which="fixed")			
			
		    # get the full set of parameters
		    allpars <- getpars(object)
						
			# get the reduced set of parameters, ie the ones that will be optimized
		    pars <- allpars[!fixed]
		    
			constraints <- getConstraints(object)
			
			lincon=constraints$lincon
			lin.u=constraints$lin.u
			lin.l=constraints$lin.l
			par.u=constraints$par.u
			par.l=constraints$par.l
						
			# incorporate equality constraints provided with the fit function, if any
			if(eq) {
				if(length(equal)!=npar(object)) stop("'equal' does not have correct length")
				equal <- pa2conr(equal)$conr
				lincon <- rbind(lincon,equal)
				lin.u <- c(lin.u,rep(0,nrow(equal)))
				lin.l <- c(lin.l,rep(0,nrow(equal)))				
			}
			
			# incorporate general linear constraints, if any
			if(cr) {
				if(ncol(conrows)!=npar(object)) stop("'conrows' does not have the right dimensions")
				lincon <- rbind(lincon,conrows)
				if(any(conrows.upper==0)) {
					lin.u <- c(lin.u,rep(0,nrow(conrows)))
				} else {
					if(length(conrows.upper)!=nrow(conrows)) stop("'conrows.upper does not have correct length")
					lin.u <- c(lin.u,conrows.upper)
				}
				if(any(conrows.lower==0)) {
					lin.l <- c(lin.l,rep(0,nrow(conrows)))
				} else {
					if(length(conrows.lower)!=nrow(conrows)) stop("'conrows.lower does not have correct length")
					lin.l <- c(lin.l,conrows.lower)
				}
			}
			
			# select only those columns of the constraint matrix that correspond to non-fixed parameters
			linconFull <- lincon
			lincon <- lincon[,!fixed,drop=FALSE]			
						
			# remove redundant rows in lincon (all zeroes)
			allzero <- which(apply(lincon,1,function(y) all(y==0)))
			if(length(allzero)>0) {
				lincon <- lincon[-allzero,,drop=FALSE]
				lin.u <- lin.u[-allzero]
				lin.l <- lin.l[-allzero]
			}
			
			# make loglike function that only depends on pars
			logl <- function(pars) {
				allpars[!fixed] <- pars
				object <- setpars(object,allpars)
				ans = -as.numeric(logLik(object))
				if(is.na(ans)) ans = 100000
				ans
			}
			
			if(method=="donlp") {
				
				reqdon <- require(Rdonlp2,quietly=TRUE)
				
				if(!reqdon) stop("Rdonlp2 not available.")
				
				# set donlp2 control parameters
				cntrl <- donlp2.control(hessian=FALSE,difftype=2,report=TRUE)	
				
				mycontrol <- function(info) {
					return(TRUE)
				}
				
				# optimize the parameters
				result <- donlp2(pars,logl,
					par.upper=par.u[!fixed],
					par.lower=par.l[!fixed],
					A=lincon,
					lin.upper=lin.u,
					lin.lower=lin.l,
					control=cntrl,
					control.fun=mycontrol,
					...
				)
				
				if(class(object)=="depmix") class(object) <- "depmix.fitted"
				if(class(object)=="mix") class(object) <- "mix.fitted"
				
				# convergence info
				object@message <- result$message
				
				# put the result back into the model
				allpars[!fixed] <- result$par
				object <- setpars(object,allpars)
			}
			
			if(method=="rsolnp") {
				
				if(!(require(Rsolnp,quietly=TRUE))) stop("Method 'rsolnp' requires package 'Rsolnp'")
				
				# separate equality and inequality constraints
				ineq <- which(lin.u!=lin.l)
				if(length(ineq)>0) lineq <- lincon[-ineq, ,drop=FALSE]
				else lineq <- lincon
								
				# returns the evaluated equality constraints
				if(nrow(lineq)>0) {
					eqfun <- function(pp) {
						ans = as.vector(lineq%*%pp)
						ans
					}
					# select the boundary values for the equality constraints
					if(length(ineq)>0) lineq.bound = lin.l[-ineq]
					else lineq.bound = lin.l
				} else {
					eqfun=NULL
					lineq.bound=NULL
				}
								
				# select the inequality constraints
				if(length(ineq)>0) {
					linineq <- lincon[ineq, ,drop=FALSE]
					ineqLB <- lin.l[ineq]
					ineqUB <- lin.u[ineq]
				} else {
					ineqfun = NULL
					ineqLB=NULL
					ineqUB=NULL
				}
				
				# call to solnp
				res <- solnp(pars, 
					logl, 
					eqfun = eqfun, 
					eqB = lineq.bound, 
					ineqfun = ineqfun, 
					ineqLB = ineqLB, 
					ineqUB = ineqUB, 
					LB = par.l[!fixed], 
					UB = par.u[!fixed], 
					control = list(delta = 1e-5, tol = 1e-6, trace = 1),
					...
				)
				
				if(class(object)=="depmix") class(object) <- "depmix.fitted"
				if(class(object)=="mix") class(object) <- "mix.fitted"
				
				object@message <- c(res$convergence," (0 is good in Rsolnp, check manual for other values)")
				
				# put the result back into the model
				allpars[!fixed] <- res$pars
				object <- setpars(object,allpars)
				
			}
			
			object@conMat <- linconFull
			object@lin.upper <- lin.u
			object@lin.lower <- lin.l
			
		}
		
		object@posterior <- viterbi(object)
		
		return(object)
	}
)


setMethod("getConstraints",
    signature(object="mix"), 
	function(object) {
		
		# set bounds, if any (should add bounds for eg sd parameters at some point ...)
		par.u <- rep(+Inf, npar(object))
		par.l <- rep(-Inf, npar(object))
		
		# make constraint matrix and its upper and lower bounds
		lincon <- matrix(0,nr=0,nc=npar(object))
		lin.u <- numeric(0)
		lin.l <- numeric(0)
		
		ns <- nstates(object)
		nrsp <- nresp(object)
		
		# get bounds from submodels
		# get internal linear constraints from submodels
		
		# first for the prior model
		bp <- 1
		ep <- npar(object@prior)
		if(!is.null(object@prior@constr)) {
			par.u[bp:ep] <- object@prior@constr$parup
			par.l[bp:ep] <- object@prior@constr$parlow
			# add linear constraints, if any
			if(!is.null(object@prior@constr$lin)) {
				lincon <- rbind(lincon,0)
				lincon[nrow(lincon),bp:ep] <- object@prior@constr$lin
				lin.u[nrow(lincon)] <- object@prior@constr$linup
				lin.l[nrow(lincon)] <- object@prior@constr$linlow
			}
		}
		
		# ... for the transition models
		if(is(object,"depmix"))	{
			for(i in 1:ns) {
				bp <- ep + 1
				ep <- ep+npar(object@transition[[i]])
				if(!is.null(object@transition[[i]]@constr)) {
					par.u[bp:ep] <- object@transition[[i]]@constr$parup
					par.l[bp:ep] <- object@transition[[i]]@constr$parlow
				}
				if(!is.null(object@transition[[i]]@constr$lin)) {
					lincon <- rbind(lincon,0)
					lincon[nrow(lincon),bp:ep] <- object@transition[[i]]@constr$lin
					lin.u[nrow(lincon)] <- object@transition[[i]]@constr$linup
					lin.l[nrow(lincon)] <- object@transition[[i]]@constr$linlow
				}
			}
		}
		
		# ... for the response models
		for(i in 1:ns) {
			for(j in 1:nrsp) {
				bp <- ep + 1
				ep <- ep + npar(object@response[[i]][[j]])
				if(!is.null(object@response[[i]][[j]]@constr)) {
					par.u[bp:ep] <- object@response[[i]][[j]]@constr$parup
					par.l[bp:ep] <- object@response[[i]][[j]]@constr$parlow
				}
				if(!is.null(object@response[[i]][[j]]@constr$lin)) {
					lincon <- rbind(lincon,0)
					lincon[nrow(lincon),bp:ep] <- object@response[[i]][[j]]@constr$lin
					lin.u[nrow(lincon)] <- object@response[[i]][[j]]@constr$linup
					lin.l[nrow(lincon)] <- object@response[[i]][[j]]@constr$linlow
				}
			}
		}
		
		res <- list(lincon=lincon,lin.u=lin.u,lin.l=lin.l,par.u=par.u,par.l=par.l)
		res
		
	}
)
