\name{simulate}

\docType{method}

\alias{simulate}

\alias{simulate,response-method}
\alias{simulate,depmix-method}
\alias{simulate,GLMresponse-method}
\alias{simulate,transInit-method}

\title{Methods to simulate from depmix models}

\description{

	Random draws from \code{depmix} objects.

}

\usage{

    \S4method{simulate}{depmix}(object, nsim=1, seed=NULL, times=NULL, ...)
    
	\S4method{simulate}{response}(object)
	
	\S4method{simulate}{GLMresponse}(object)
	
	\S4method{simulate}{transInit}(object)
}

\arguments{
	
	\item{object}{Object to generate random draws.  An object of class
	\code{depmix}, \code{response}, \code{transInit}}

	\item{nsim}{The number of draws (one draw simulates a data set of the
	size that is defined by ntimes); defaults to 1.}
	
	\item{seed}{Set the seed.}
	
	\item{times}{An indicator vector indicating for which times in the
	complete series to generate the data.  For internal use.}
	
	\item{...}{Not used currently.}
}

\details{

	For a \code{depmix} model, simulate generates \code{nsim} random state
	sequences, each of the same length as the observation sequence in the
	\code{depmix} model (i.e., \code{sum(ntimes(object))}.  The state
	sequences are then used to generate \code{nsim} observation sequences
	of thee same length.

	Setting the \code{times} option selects the time points in the total
	state/observation sequence (i.e., counting is continued over ntimes).
	Usage of the \code{times} option is not recommended.

}

\value{

	For a \code{depmix} object, a \code{list} with two elements;
	\code{states}, an N*nsim matrix with a state sequence in each column,
	and \code{responses}, an N*nobs*nsim array with observation sequences,
	where N = \code{sum(ntimes(object))} 

}

\author{Maarten Speekenbrink}

\keyword{methods}
