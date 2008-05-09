
# 
# Ingmar Visser, 23-3-2008
# 

# 
# Class definition, accessor functions, print and summary methods
# 

# 
# DEPMIX CLASS
# 

setClass("depmix",
	representation(response="list", # response models
		transition="list", # transition models (multinomial logistic)
		prior="ANY", # the prior model (multinomial logistic)
		dens="array", # response densities (B)
		trDens="array", # transition densities (A)
		init="array", # usually called pi 
		stationary="logical",
		ntimes="numeric",
		nstates="numeric",
		nresp="numeric",
		npars="numeric" # number of parameters
	)
)

# accessor functions
setMethod("npar","depmix",
	function(object) return(object@npars)
)

setMethod("ntimes","depmix",
	function(object) return(object@ntimes)
)

setMethod("nstates","depmix",
	function(object) return(object@nstates)
)

setMethod("nresp","depmix",
	function(object) return(object@nresp)
)


# 
# PRINT method
# 

setMethod("show","depmix",
	function(object) {
		cat("Initial state probabilties model \n")
		print(object@prior)
		cat("\n")
		for(i in 1:object@nstates) {
			cat("Transition model for state (component)", i,"\n")
			print(object@transition[[i]])
			cat("\n")
		}
		cat("\n")
		for(i in 1:object@nstates) {
			cat("Response model(s) for state", i,"\n\n")
			for(j in 1:object@nresp) {
				cat("Response model for response",j,"\n")
				print(object@response[[i]][[j]])
				cat("\n")
			}
			cat("\n")
		}
	}
)


# 
# SUMMARY method: to do
# 



