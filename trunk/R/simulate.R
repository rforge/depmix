# simulate data from a depmix model

# TODO: move this to all generics etc...
setGeneric("is.stationary", function(object,...) standardGeneric("is.stationary"))
 setMethod("is.stationary",signature(object="depmix"),
  function(object) {
		return(object@stationary)
	}
)

setMethod("simulate",signature(object="depmix"),
  function(object,nsim=1,seed=NULL,...) {
   	nt <- sum(object@ntimes)
  	lt <- length(object@ntimes)
  	et <- cumsum(object@ntimes)
  	bt <- c(1,et[-lt]+1)

  	nr <- nresp(object)
  	ns <- nstates(object)

    # simulate state sequences first, then observations

  	states <- array("numeric",dim=c(nt,nsim))
  	for(case in 1:lt) {
      states[bt,] <- simulate(object@prior,n=nsim,is.prior=T)
      for(i in (bt[case]+1):et[case]) {
        for(j in 1:n) {
          if(is.stationary(object)) {
            states[i,j] <- simulate(object@transition[[states[i-1,j]]],n=nsim)
          } else {
            states[i,j] <- simulate(object@transition[[states[i-1,j]]],n=nsim,time=i)
          }
        }
      }
    }

    responses <- array("numeric",dim=c(nt,nresp,nsim))
    for(i in 1:resp(object)) {
      tmp <- array(,dim=c(nt,ns,nsim))
      for(j in 1:ns) {
        tmp[,j,] <- simulate(object@response[[j]][[i]],nsim=nt*nsim)
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
      states <- vector()
      for(i in 1:nrow(prob)) {
        states <- c(states,which(rmultinom(n=nsim,size=1,prob=pr[i,]))==1)
      }
      states <- as.vector(matrix(states,nrow=nrow(prob),ncol=nsim,byrow=T))
      return(states)
    } else {
      if(missing(time)) {
        # this is likely to be a stationary model...
        pr <- predict(object)
        states <- apply(rmultinom(nsim,size=1,prob=pr),2,function(x) which(x==1))
        return(states)
      } else {
        pr <- predict(object)[time,]
        states <- apply(rmultinom(nsim,size=1,prob=pr),2,function(x) which(x==1))
        return(states)
      }
    }
  }
)

setMethod("simulate",signature(object="NORMresponse"),
  function(object,nsim=1,seed=NULL,time) {
    if(missing(time)) {
      # draw in one go
      mu <- predict(object)
      sd <- getpars(object)["sd"]
      response <- rnorm(nt*nsim,mean=mu,sd=sd)
    } else {
      mu <- predict(object)[time]
      sd <- getpars(object)["sd"]
      response <- rnorm(length(time)*nsim,mean=mu,sd=sd)
    }
  }
)

setMethod("simulate",signature(object="MULTINOMresponse"),
  function(object,nsim=1,seed=NULL,time) {
    if(missing(time)) {
      # draw all times in one go
      pr <- predict(object)
      response <- t(apply(pr,1,function(x) apply(rmultinom(n=nsim,size=1,pr=x),2,function(y) which(y==1))))
    } else {
      pr <- predict(object)[time,]
      response <- t(apply(pr,1,function(x) apply(rmultinom(n=nsim,size=1,pr=x),2,function(y) which(y==1))))
    }
  }
)



