
# 
# Ingmar Visser, 23-3-2008
# 

# 
# Main function to construct depmix models
# 

#
# UNIVARIATE AND MULTIVARIATE MARKOV MIXTURE OF GLM'S
# 

setMethod("depmix",
	signature(response="ANY"),
	function(response, data=NULL, nstates, transition=~1, family=gaussian(), prior=~1, initdata=NULL,
		respstart=NULL, trstart=NULL, instart=NULL, ntimes=NULL, ...) {
		
		if(is.null(data)) {
			if(is.null(ntimes)) stop("'ntimes' must be provided if not in the data")
		} else {
			if(is.null(attr(data,"ntimes"))) {
				if(is.null(ntimes)) ntimes <- nrow(data)
			} else {
				ntimes <- attr(data,"ntimes")
			}
			if(sum(ntimes)!=nrow(data)) stop("'ntimes' and data do not match")
		}
		
		# make response models
		response <- makeResponseModels(response=response,data=data,nstates=nstates,family=family,values=respstart)
		
		# make transition models
		stationary=FALSE
		if(transition==~1) stationary=TRUE
		transition <- makeTransModels(nstates=nstates,formula=transition,data=data,stationary=stationary,values=trstart)
		
		# make prior model
		prior <- makePriorModel(nstates=nstates,ncases=length(ntimes),formula=prior,data=initdata,values=instart)
		
		# call main depmix with all these models, ntimes and stationary
		model <- makeDepmix(response=response,transition=transition,prior=prior,ntimes=ntimes,stationary=stationary)
		
		# deal with starting values here!!!!!!
		
		return(model)
	}
)






