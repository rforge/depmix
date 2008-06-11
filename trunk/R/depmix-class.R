
# 
# Ingmar Visser, 11-6-2008
# 

# 
# DEPMIX CLASS BELOW THE MIX CLASS
# 

# 
# Class definition, accessor functions, print and summary methods
# 

# 
# MIX CLASS
# 

setClass("mix",
	representation(response="list", # response models
		prior="ANY", # the prior model (multinomial logistic)
		dens="array", # response densities (B)
		init="array", # usually called pi 
		nstates="numeric",
		nresp="numeric",
		ntimes="numeric",
		npars="numeric" # number of parameters
	)
)

# accessor functions
setMethod("npar","mix",
	function(object) return(object@npars)
)

setMethod("ntimes","mix",
	function(object) return(object@ntimes)
)

setMethod("nstates","mix",
	function(object) return(object@nstates)
)

setMethod("nresp","mix",
	function(object) return(object@nresp)
)


# 
# PRINT method
# 

setMethod("show","mix",
	function(object) {
		cat("Initial state probabilties model \n")
		print(object@prior)
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
	representation(transition="list", # transition models (multinomial logistic)
		trDens="array", # transition densities (A)
		stationary="logical"
	),
	contains="mix"
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



