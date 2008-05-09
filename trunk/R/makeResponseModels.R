makeResponseModels <-
function(response,data=NULL,nstates,family,values=NULL,...) {
	
	resp <- response
	response <- list()
	
	st=FALSE
	if(!is.null(values)) st=TRUE
	
	# univariate response data
	if(class(resp)=="formula") {
		nresp <- 1
		for(i in 1:nstates) {
			response[[i]] <- list()
			response[[i]][[1]] <- GLMresponse(resp,data=data,family=family)
			if(st) {
				bp <- npar(response[[i]][[1]])
				response[[i]][[1]] <- GLMresponse(resp,data=data,family=family,pstart=values[1:bp])
				bp <- bp+1
				values <- values[bp:length(values)]
			}
		}
	}
	
	# multi variate response data
	if(is.list(resp)) {
		nresp <- length(resp)
		for(i in 1:nstates) {
			response[[i]] <- list()
			for(j in 1:nresp) {
				response[[i]][[j]] <- GLMresponse(resp[[j]],data=data,family=family[[j]])
				if(st) {
					bp <- npar(response[[i]][[j]])
					response[[i]][[j]] <- GLMresponse(resp[[j]],data=data,family=family[[j]],pstart=values[1:bp])
					bp <- bp+1
					values <- values[bp:length(values)]
				}
			}
		}
	}
	
	return(response)
}

