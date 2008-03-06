em <- function(object,maxit=100,tol=1e-6,verbose=FALSE,...) {
	if(!is(object,"depmix")) stop("object is not of class 'depmix'")
	
	# pseudocode
	ns <- object@nstates
	
	ntimes <- ntimes(object)
	lt <- length(ntimes)
	et <- cumsum(ntimes)
	bt <- c(1,et[-lt]+1)
		
	LL <- logLik(object)
	
	converge <- FALSE
	j <- 0
	
	A <- object@trDens
	while(j <= maxit & !converge) {
		
		for(i in 1:ns) {
			A[,,i] <- object@trDens[,,i]
		}
		
		B <- apply(object@dens,c(1,3),prod)
		# TODO: add functionality for inModel
		init <- object@init
# 		print(init)
		LL.old <- LL
		j <- j+1
		
		# expectation
		fbo <- fb(init=init,A=A,B=B,ntimes=ntimes(object))
		LL <- fbo$logLike
		
		# maximization
		
		#object@init <- fit(object@init,ip=fbo$gamma[1,])
		#object@init <- matrix(fbo$gamma[1,],nrow=1)
		
		# FIX ME for length(ntimes)>1
		# print(fbo$gamma[1,])
		# Here we need an average of gamma[bt[case],], which may need to be weighted ?? (see Rabiner, p283)
		

		
		# this is without weighting
		initprobs <- apply(fbo$gamma[bt,],2,mean)
		
		# should become object@prior <- fit(object@prior)
		object@prior@y <- fbo$gamma[bt,]
		object@prior <- fit(object@prior,w=NULL)
		#object@initModel <- setpars(object@initModel,values=object@initModel@family$linkfun(initprobs,base=object@initModel@family$base))
		
		# This should become:
		# lt <- length(object@ntimes)
		# et <- cumsum(object@ntimes)
		# bt <- c(1,et[-lt]+1)
		# object@initModel@y <- fbo$gamma[bt,]
		# object@initModel <- fit(object@initModel)
		
		#et <- cumsum(object@ntimes)
		
		trm <- matrix(0,ns,ns)
		for(i in 1:ns) {
			
			if(max(ntimes(object)>1)) { #skip transition parameters update in case of latent class model
				if(!object@stationary) {
					object@transition[[i]]@y <- fbo$xi[,,i]/fbo$gamma[,i]
					object@transition[[i]] <- fit(object@transition[[i]],w=as.matrix(fbo$gamma[,i]),ntimes=ntimes(object)) # check this
				} else {
					for(k in 1:ns) {
						trm[i,k] <- sum(fbo$xi[-c(et),k,i])/sum(fbo$gamma[-c(et),i])
					}
					# FIX THIS; it will only work with a specific trinModel
					object@transition[[i]]@parameters$coefficients <- object@transition[[i]]@family$linkfun(trm[i,],base=object@transition[[i]]@family$base)
				}
				
				#object@trModels[[i]] <- fit(object@trModels[[i]],w=NULL,ntimes=object@ntimes) # check this
				#object@trans[,,i] <- exp(logDens(object@trModels[[i]]))
				
				# update trans slot of the model
				object@trDens[,,i] <- dens(object@transition[[i]])
			}
			
			for(k in 1:nresp(object)) {
				object@response[[i]][[k]] <- fit(object@response[[i]][[k]],w=fbo$gamma[,i])
				# update logdens slot of the model
				object@dens[,k,i] <- dens(object@response[[i]][[k]])
			}
		}
		
		#object <- setpars(object,getpars(object)) # set parameters and recompute (bit of a roundabout way)
		
		LL <- logLik(object)
		if(verbose) cat("iteration",j,"logLik:",LL,"\n")
		if( (LL >= LL.old) & (LL - LL.old < tol))  converge <- TRUE
	}
	
	object
}
