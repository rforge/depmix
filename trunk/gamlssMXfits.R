gamlssMXfits <- function (n = 5,
formula = formula(data), 
pi.formula = ~1, 
family = "NO",
weights, 
K = 2, 
prob = NULL,
data = sys.parent(),
control = MX.control(), 
g.control = gamlss.control(trace=FALSE),
zero.component = FALSE,
...) {
	gamlssMXfitscall <- match.call()
	modellist<-list()
	dev <- rep(0,length=n)
	seed <- floor(runif(min=0,max=10000, n=n))
	for (i in (1:n)) {
		m1<-try(gamlssMX(formula=formula,pi.formula=pi.formula,
				family = family ,K=K, prob=prob, data=data,
				control=MX.control(seed=seed[i]), 
				g.control = gamlss.control(trace=FALSE),
				zero.component=zero.component)) #
		if(any(class(m1)%in%"try-error")) { 
			cat("model=", i, "failed", "\n") 
			dev[i] <- NA
			modellist[[i]] <- NA
			next
		}
		modellist[[i]] <- m1 
		dev[i] <- deviance(modellist[[i]])
		cat("model=", i,"\n")
	}
	II <- which.min(dev)
	model <- modellist[[II]]
	gamlssMXfitscall$n <- NULL
	gamlssMXfitscall[[1]] <- as.name("gamlssMX")
	model$call <- gamlssMXfitscall
	model$extra <- list(dev=dev, seed=seed, which=II)
	model
}
