em <- function(object,maxit=100,tol=1e-6,verbose=FALSE,...) {
	if(!is(object,"hmModel")) stop("object must be 'hmModel'")
	
	# pseudocode
	ns <- object@nstates
	
	LL <- logLik(object)
	
	converge <- FALSE
	j <- 0
	
	A <- object@trans
	while(j <= maxit & !converge) {
		
		for(i in 1:ns) {
			A[,,i] <- predict(object@trModels[[i]])
		}
		
		B <- exp(apply(object@logdens,c(1,3),sum))
		# TODO: add functionality for inModel
		init <- exp(logDens(object@initModel))
# 		print(init)
		LL.old <- LL
		j <- j+1
		
		# expectation
		fbo <- fb(init=init,A=A,B=B,ntimes=object@ntimes)
		LL <- fbo$logLike
		
		# maximization
		
		#object@init <- fit(object@init,ip=fbo$gamma[1,])
		#object@init <- matrix(fbo$gamma[1,],nrow=1)
		
		# FIX ME for length(ntimes)>1
		# print(fbo$gamma[1,])
		# Here we need an average of gamma[bt[case],], which may need to be weighted ?? (see Rabiner, p283)
		
		ntimes <- object@ntimes
		lt <- length(ntimes)
		et <- cumsum(ntimes)
		bt <- c(1,et[-lt]+1)
		
		# this is without weighting
		initprobs <- apply(fbo$gamma[bt,],2,mean)
		
		object@initModel <- setpars(object@initModel,values=object@initModel@family$linkfun(initprobs,base=object@initModel@family$base))
		
		# This should become:
		# lt <- length(object@ntimes)
		# et <- cumsum(object@ntimes)
		# bt <- c(1,et[-lt]+1)
		# object@initModel@y <- fbo$gamma[bt,]
		# object@initModel <- fit(object@initModel)
		
		et <- cumsum(object@ntimes)
		
		trm <- matrix(0,ns,ns)
		
		vit <- viterbi(object)
		emp_trans <- y <- matrix(NA,nrow=sum(ntimes),ncol=2)
		for(case in 1:lt) {
      emp_trans[bt[case]:(et[case]-1),] <- cbind(vit[bt[case]:(et[case]-1)],vit[(bt[case]+1):(et[case])])
    }
		
		for(i in 1:ns) {
			
			if(max(object@ntimes)>1) { #skip transition parameters update in case of latent class model
				if(!object@stationary) {
          # y is now indicator matrix with 1 in column j if transition to state j (from any state) and 0 otherwise
          object@trModels[[i]]@y <-  matrix(as.numeric(unlist(lapply(as.list(1:ns),function(x) emp_trans[,2] == x))),ncol=ns)
          # w below is indicator vector with 1 if transition was from state i
          object@trModels[[i]] <- fit(object@trModels[[i]],w=as.numeric(emp_trans[,1]==i),ntimes=object@ntimes) # check this
          #object@trModels[[i]]@y <- fbo$xi[,,i]/fbo$gamma[,i]
					#object@trModels[[i]] <- fit(object@trModels[[i]],w=as.matrix(fbo$gamma[,i]),ntimes=object@ntimes) # check this
				} else {
					for(k in 1:ns) {
						trm[i,k] <- sum(fbo$xi[-c(et),k,i])/sum(fbo$gamma[-c(et),i])
					}
					object@trModels[[i]]@parameters$coefficients <- object@trModels[[i]]@family$linkfun(trm[i,],base=object@initModel@family$base)
				}
				
				#object@trModels[[i]] <- fit(object@trModels[[i]],w=NULL,ntimes=object@ntimes) # check this
				#object@trans[,,i] <- exp(logDens(object@trModels[[i]]))
				
				# update trans slot of the model
				object@trans[,,i] <- predict(object@trModels[[i]])
			}
			
			for(k in 1:object@nresp) {
				object@rModels[[i]][[k]] <- fit(object@rModels[[i]][[k]],w=fbo$gamma[,i])
				# update logdens slot of the model
				object@logdens[,k,i] <- logDens(object@rModels[[i]][[k]])
			}
		}
		
		#object <- setpars(object,getpars(object)) # set parameters and recompute (bit of a roundabout way)
		
		LL <- logLik(object)
		if(verbose) cat("iteration",j,"logLik:",LL,"\n")
		if( (LL >= LL.old) & (LL - LL.old < tol))  converge <- TRUE
	}
	
	object
}
