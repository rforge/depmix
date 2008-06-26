# simulate data from a depmix model

# TODO: move this to all generics etc...
#setGeneric("is.stationary", function(object,...) standardGeneric("is.stationary"))
setClass("depmix.sim",
  contains="depmix",
  representation(
    states="matrix"
  )
)

setMethod("is.stationary",signature(object="depmix"),
  function(object) {
		return(object@stationary)
	}
)

setMethod("simulate",signature(object="depmix"),
  function(object,nsim=1,seed=NULL,...) {
    ntim <- ntimes(object) 
   	nt <- sum(ntim)
  	lt <- length(ntim)
  	et <- cumsum(ntim)
  	bt <- c(1,et[-lt]+1)

  	nr <- nresp(object)
  	ns <- nstates(object)

    # simulate state sequences first, then observations

    # random generation is slow when done separately for each t, so first draw 
    #   variates for all t, and then determine state sequences iteratively
  	states <- array(,dim=c(nt,nsim))
  	states[bt,] <- simulate(object@prior,n=nsim,is.prior=T)
  	sims <- array(,dim=c(nt,ns,nsim))
  	for(i in 1:ns) {
      sims[,i,] <- simulate(object@transition[[i]],nsim=nsim)
 	  }
 	  # track states
  	for(case in 1:lt) {
      for(i in (bt[case]+1):et[case]) {
        states[i,] <- sims[cbind(i,states[i-1,],1:nsim)]
      }
    }

    states <- as.vector(states)
    responses <- list(length=nr)
    #responses <- array(,dim=c(nt,nr,nsim))
    for(i in 1:nr) {
      tmp <- matrix(,nrow=nt*nsim,ncol=NCOL(object@response[[1]][[i]]@y))
      for(j in 1:ns) {
        tmp[states==j,] <- simulate(object@response[[j]][[i]],nsim=nsim)[states==j,]
      }
      responses[[i]] <- tmp
    }
    
    # generate new depmix.sim object
    class(object) <- "depmix.sim"
    object@states <- as.matrix(states)
    
    object@prior@x <- apply(object@prior@x,2,rep,nsim)
    for(j in 1:ns) {
      if(!is.stationary(object)) object@transition[[j]]@x <- as.matrix(apply(object@transition[[j]]@x,2,rep,nsim))
      for(i in 1:nr) {
        object@response[[j]][[i]]@y <- as.matrix(responses[[i]])
        object@response[[j]][[i]]@x <- as.matrix(apply(object@response[[j]][[i]]@x,2,rep,nsim))
      }
    }
    object@ntimes <- rep(object@ntimes,nsim)
    
   	# make appropriate array for transition densities
  	nt <- sum(object@ntimes)
  	if(is.stationary(object)) trDens <- array(0,c(1,ns,ns)) else trDens <- array(0,c(nt,ns,ns))

  	# make appropriate array for response densities
  	dns <- array(,c(nt,nr,ns))

  	# compute observation and transition densities
  	for(i in 1:ns) {
  		for(j in 1:nr) {
  			dns[,j,i] <- dens(object@response[[i]][[j]]) # remove this response as an argument from the call to setpars
  		}
  		trDens[,,i] <- dens(object@transition[[i]])
  	}

  	# compute initial state probabilties
  	object@init <- dens(object@prior)
    object@trDens <- trDens
    object@dens <- dns
    
    return(object)
  }
)

setMethod("simulate",signature(object="transInit"),
  function(object,nsim=1,seed=NULL,times,is.prior=FALSE) {
    if(!is.null(seed)) set.seed(seed)
    if(is.prior) {
      pr <- dens(object)
      sims <- array(apply(pr,1,rmultinom,n=nsim,size=1),dim=c(ncol(pr),nsim,nrow(pr)))
      states <- t(apply(sims,c(2,3), function(x) which(x==1)))
      return(states)
    } else {
      if(missing(times)) {
        # this is likely to be a stationary model...
        pr <- predict(object)
      } else {
        pr <- predict(object)[times,]
        if(length(times)==1) pr <- matrix(pr,ncol=length(pr))
      }
      nt <- nrow(pr)
      sims <- array(apply(pr,1,rmultinom,n=nsim,size=1),dim=c(ncol(pr),nsim,nt))
      states <- t(apply(sims,c(2,3), function(x) which(x==1)))
      # states <- apply(apply(pr,2,rmultinom rmultinom(nt*nsim,size=1,prob=pr),2,function(x) which(x==1))
      return(states)
    }
  }
)



