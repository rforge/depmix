

library(depmixS4)

data(balance)
# four binary items on the balance scale task

instart=c(0.5,0.5)
set.seed(1)
respstart=runif(16)
# note that ntimes argument is used to make this a mixture model
mod4 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial("identity"),multinomial("identity"),
		multinomial("identity"),multinomial("identity")),
	respstart=respstart,instart=instart)

fmod4 <- fit(mod4, em.control=list(random.start=TRUE))

fmod4

mod4b <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial("identity"),multinomial("identity"),
		multinomial("identity"),multinomial("identity")))


fmod4b <- fit(mod4b)

fmod4b <- fit(mod4b, em.control=list(tol=1e-8,
		crit=c("relative","absolute"),random.start=TRUE))



# add age as covariate on class membership by using the prior argument
instart=c(0.5,0.5,0,0) # we need the initial probs and the coefficients of age 
set.seed(2)
respstart=runif(16)
mod5 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial("identity"),multinomial("identity"),
		multinomial("identity"),multinomial("identity")),
	instart=instart, respstart=respstart, prior=~age, initdata=balance)

fmod5 <- fit(mod5)

mod5b <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial("identity"),multinomial("identity"),
		multinomial("identity"),multinomial("identity")),
	prior=~age, initdata=balance)

fmod5b <- fit(mod5b)

fmod5b <- fit(mod5b,em.control=list(tol=1e-8,
		crit=c("relative","absolute"),random.start=TRUE))


data(speed)

# 2-state model on rt and corr from speed data set 
# with Pacc as covariate on the transition matrix
# starting values for the transition pars (without 
# those EM does not get off the ground)
set.seed(1)
tr=runif(6)
trst=c(tr[1:2],0,tr[3:5],0,tr[6])
mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
	family=list(gaussian(),multinomial("identity")),trstart=trst)
# fit the model
fmod1 <- fit(mod1)
fmod1 # to see the logLik and optimization information
# to see the parameters

mod1b <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
	family=list(gaussian(),multinomial("identity")))
# fit the model
fmod1 <- fit(mod1b)

fmod1 <- fit(mod1b,em.control=list(tol=1e-8,
		crit=c("relative","absolute"),random.start=TRUE))


# generate data from two different multivariate normal distributions
m1 <- c(0,1)
sd1 <- matrix(c(1,0.7,.7,1),2,2)
m2 <- c(1,0)
sd2 <- matrix(c(2,.1,.1,1),2,2)
y1 <- mvrnorm(50,m1,sd1)
y2 <- mvrnorm(50,m2,sd2)
y <- rbind(y1,y2)

# now use makeDepmix to create a depmix model for this bivariate normal timeseries
rModels <-  list()
rModels[[1]] <- list(MVNresponse(y~1))
rModels[[2]] <- list(MVNresponse(y~1))

trstart=c(0.9,0.1,0.1,0.9)

transition <- list()
transition[[1]] <- transInit(~1,nstates=2,data=data.frame(1),pstart=c(trstart[1:2]))
transition[[2]] <- transInit(~1,nstates=2,data=data.frame(1),pstart=c(trstart[3:4]))

instart=runif(2)
inMod <- transInit(~1,ns=2,ps=instart,data=data.frame(1))

mod <- makeDepmix(response=rModels,transition=transition,prior=inMod)

fm2 <- fit(mod)


# now use makeDepmix to create a depmix model for this bivariate normal timeseries
rModels <-  list()
rModels[[1]] <- list(MVNresponse(y~1))
rModels[[2]] <- list(MVNresponse(y~1))

transition <- list()
transition[[1]] <- transInit(~1,nstates=2,data=data.frame(1))
transition[[2]] <- transInit(~1,nstates=2,data=data.frame(1))

inMod <- transInit(~1,ns=2,data=data.frame(1))

mod2b <- makeDepmix(response=rModels,transition=transition,prior=inMod)

fm2b <- fit(mod2b,em.control=list(tol=1e-8,
		crit=c("relative","absolute"),random.start=TRUE))






# 
# Maarten Speekenbrink 23-3-2008
# 

em <- function(object,maxit=100,tol=1e-6,verbose=FALSE,...) {
	if(!is(object,"mix")) stop("object is not of class '(dep)mix'")
	call <- match.call()
	if(is(object,"depmix")) {
		call[[1]] <- as.name("em.depmix")
	} else {
		call[[1]] <- as.name("em.mix")
	}
	object <- eval(call, parent.frame())
	object
}

# em for lca and mixture models
em.mix <- function(object,maxit=100,tol=1e-6,verbose=FALSE,start=FALSE,...) {
	if(!is(object,"mix")) stop("object is not of class 'mix'")
	
	ns <- object@nstates
	
	ntimes <- ntimes(object)
	lt <- length(ntimes)
	et <- cumsum(ntimes)
	bt <- c(1,et[-lt]+1)
	
	converge <- FALSE
	j <- 0
	
	# compute response probabilities
	B <- apply(object@dens,c(1,3),prod)
	gamma <- object@init*B
	LL <- sum(log(rowSums(gamma)))
	# normalize
	gamma <- gamma/rowSums(gamma)
	
	if(start) {
		nr <- sum(ntimes(object))
		snt <- nstates(object)
		gamma <- matrix(runif(nr*snt),nr=nr,nc=snt)
		gamma <- gamma/rowSums(gamma)
	}
	
	LL.old <- LL + 1
	
	while(j <= maxit & !converge) {
		
		# maximization
		
		# should become object@prior <- fit(object@prior)
		object@prior@y <- gamma[bt,,drop=FALSE]
		object@prior <- fit(object@prior, w=NULL,ntimes=NULL)
		object@init <- dens(object@prior)
		
		for(i in 1:ns) {
			for(k in 1:nresp(object)) {
				object@response[[i]][[k]] <- fit(object@response[[i]][[k]],w=gamma[,i])
				# update dens slot of the model
				object@dens[,k,i] <- dens(object@response[[i]][[k]])
			}
		}
		
		# expectation
		B <- apply(object@dens,c(1,3),prod)
		gamma <- object@init*B
		LL <- sum(log(rowSums(gamma)))
		# normalize
		gamma <- gamma/rowSums(gamma)
		
		# print stuff
		if(verbose&((j%%5)==0)) {
			cat("iteration",j,"logLik:",LL,"\n")
		}
		
		if( (LL >= LL.old) & (LL - LL.old < tol))  {
			cat("iteration",j,"logLik:",LL,"\n")
			converge <- TRUE
		}

		LL.old <- LL
		j <- j+1

	}

	class(object) <- "mix.fitted"

	if(converge) object@message <- "Log likelihood converged to within tol."
	else object@message <- "'maxit' iterations reached in EM without convergence."

	# no constraints in EM
	object@conMat <- matrix()
	object@lin.lower <- numeric()
	object@lin.upper <- numeric()
	
	object
	
}

# em for hidden markov models
em.depmix <- function(object,maxit=100,tol=1e-6,verbose=FALSE,start=FALSE,...) {
	
	if(!is(object,"depmix")) stop("object is not of class '(dep)mix'")
	
	ns <- object@nstates
	
	ntimes <- ntimes(object)
	lt <- length(ntimes)
	et <- cumsum(ntimes)
	bt <- c(1,et[-lt]+1)
	
	converge <- FALSE
	j <- 0
	
	# A <- object@trDens
	# B <- object@dens
	# init <- object@init
	
	# initial expectation
	fbo <- fb(init=object@init,A=object@trDens,B=object@dens,ntimes=ntimes(object),stationary=object@stationary)
	LL <- fbo$logLike
	LL.old <- LL + 1
	
	if(start) {
		nr <- sum(ntimes(object))
		snt <- nstates(object)
		fbo$gamma <- matrix(runif(nr*snt),nr=nr,nc=snt)
		fbo$gamma <- fbo$gamma/rowSums(fbo$gamma)
	}
	
	while(j <= maxit & !converge) {
		
		# maximization
				
		# should become object@prior <- fit(object@prior)
		object@prior@y <- fbo$gamma[bt,,drop=FALSE]
		object@prior <- fit(object@prior, w=NULL,ntimes=NULL)
		object@init <- dens(object@prior)
				
		trm <- matrix(0,ns,ns)
		for(i in 1:ns) {
			if(max(ntimes(object)>1)) { # skip transition parameters update in case of latent class model
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
				# update trDens slot of the model
				object@trDens[,,i] <- dens(object@transition[[i]])
			}
		}
		
		for(i in 1:ns) {
			
			for(k in 1:nresp(object)) {
				object@response[[i]][[k]] <- fit(object@response[[i]][[k]],w=fbo$gamma[,i])
				# update dens slot of the model
				object@dens[,k,i] <- dens(object@response[[i]][[k]])
			}
		}
		
		# expectation
		fbo <- fb(init=object@init,A=object@trDens,B=object@dens,ntimes=ntimes(object),stationary=object@stationary)
		LL <- fbo$logLike
				
		if(verbose&((j%%5)==0)) cat("iteration",j,"logLik:",LL,"\n")
		if( (LL >= LL.old) & (LL - LL.old < tol))  {
			cat("iteration",j,"logLik:",LL,"\n")
			converge <- TRUE
		}
		
		LL.old <- LL
		j <- j+1
		
	}
	
	#if(class(object)=="depmix") class(object) <- "depmix.fitted"
	#if(class(object)=="mix") class(object) <- "mix.fitted"
	
	class(object) <- "depmix.fitted"
	
	if(converge) object@message <- "Log likelihood converged to within tol."
	else object@message <- "'maxit' iterations reached in EM without convergence."
	
	# no constraints in EM
	object@conMat <- matrix()
	object@lin.lower <- numeric()
	object@lin.upper <- numeric()
	
	object
}


data(speed)

# 2-state model on rt and corr from speed data set with Pacc as covariate on the transition matrix
# starting values for the transition pars (without those EM does not get off the ground)
set.seed(1)
tr=runif(6)
trst=c(tr[1:2],0,tr[3:5],0,tr[6])

mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
family=list(gaussian(),multinomial()))

logLik(mod1)

fm <- em(mod1,start=TRUE,verb=TRUE)

data(balance)
# four binary items on the balance scale task


# note that ntimes argument is used to make this a mixture model
mod4 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
        family=list(multinomial(),multinomial(),multinomial(),multinomial()))

fmod4 <- em(mod4,start=T,verb=TRUE)


# add age as covariate on class membership by using the prior argument
instart=c(0.5,0.5,0,0) # we need the initial probs and the coefficients of age 
set.seed(2)
respstart=runif(16)
mod5 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
        family=list(multinomial(),multinomial(),multinomial(),multinomial()),
        instart=instart, respstart=respstart, prior=~age, initdata=balance)

fmod5 <- fit(mod5)

mod5 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
        family=list(multinomial(),multinomial(),multinomial(),multinomial()),
        prior=~age, initdata=balance)

fm6 <- em(mod5,start=TRUE,verb=TRUE)

