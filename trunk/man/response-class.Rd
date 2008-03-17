\name{response-class}

\docType{class}

\alias{response-class}
\alias{getdf}
\alias{getdf,response-method}

\title{Class "response"}

\description{A \code{\link{response}} model for depmix models.}

\usage{
	\S4method{getdf}{response}(object)
}

\arguments{
	\item{object}{An object of class "response".}
}

\section{Slots}{
	\describe{
    	\item{\code{parameters}:}{A (named) list of parameters.}
		\item{\code{fixed}:}{A logical vector indicating which parameters
			are fixed.}
		\item{\code{y}:}{A matrix with the actual response; possibly
			multivariate.}
		\item{\code{x}:}{A design matrix; possibly only an intercept term.}
		\item{\code{npar}:}{The number of parameters.}
	}
	
}

\details{
	This class offers a framework from which to build specific response
	models such as glm based responses or multinomial responses.
}

\section{Accessor Functions}{
	The following functions should be used for accessing the corresponding
	slots:
	\describe{
		\item{\code{npar}:}{The number of parameters of the model.}
		\item{\code{getdf}:}{The number of non-fixed parameters.}
	}
}

\author{Ingmar Visser}

\keyword{classes}
