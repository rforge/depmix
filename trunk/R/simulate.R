# simulate data from a depmix model

# TODO: move this to all generics etc...
#setGeneric("is.stationary", function(object,...) standardGeneric("is.stationary"))
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
#        for(j in 1:nsim) {
#          if(is.stationary(object)) {
#            states[i,j] <- simulate(object@transition[[states[i-1,j]]],nsim=1)
#          } else {
#            states[i,j] <- simulate(object@transition[[states[i-1,j]]],nsim=1,time=i)
#          }
#        }
#      }
#    }

    responses <- array(,dim=c(nt,nr,nsim))
    for(i in 1:nr) {
      tmp <- array(,dim=c(nt,ns,nsim))
      for(j in 1:ns) {
        tmp[,j,] <- simulate(object@response[[j]][[i]],nsim=nsim)
      }
      for(j in 1:nsim) {
        responses[,i,j] <- tmp[cbind(1:nt,states[,j],j)]
      }
    }
    return(list(states=states,responses=responses))
  }
)

setMethod("simulate",signature(object="transInit"),
  function(object,nsim=1,seed=NULL,is.prior=FALSE,time) {
    if(is.prior) {
      pr <- dens(object)
      sims <- array(apply(pr,1,rmultinom,n=nsim,size=1),dim=c(ncol(pr),nsim,nrow(pr)))
      states <- t(apply(sims,c(2,3), function(x) which(x==1)))
      return(states)
    } else {
      if(missing(time)) {
        # this is likely to be a stationary model...
        pr <- predict(object)
      } else {
        pr <- predict(object)[time,]
        if(length(time)==1) pr <- matrix(pr,ncol=length(pr))
      }
      nt <- nrow(pr)
      sims <- array(apply(pr,1,rmultinom,n=nsim,size=1),dim=c(ncol(pr),nsim,nt))
      states <- t(apply(sims,c(2,3), function(x) which(x==1)))
      # states <- apply(apply(pr,2,rmultinom rmultinom(nt*nsim,size=1,prob=pr),2,function(x) which(x==1))
      return(states)
    }
  }
)



