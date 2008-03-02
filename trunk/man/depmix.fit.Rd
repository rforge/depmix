\name{fit}
\docType{method}
\alias{fit}
\alias{depmix.fit}
\alias{fit,depmix-method}
\title{Method fit}
\description{Method for fitting depmix models.}
\usage{

	\S4method{fit}{depmix}(object, w=NULL, fixed=NULL, equal=NULL, conrows=NULL,
		conrows.upper=0, conrows.lower=0, method=c("em","donlp"),...)
	
}
\arguments{
	\item{object}{An object of class "depmix".}
	\item{w}{Weights??? What's this argument doing here? It's not being
		used.}
	\item{fixed}{Vector of mode logical indicating which parameters should 
		be fixed.}
	\item{equal}{Vector indicating equality constraints; see details.}
	\item{conrows}{Rows of a general linear constraint matrix; see details.}
	\item{conrows.upper, conrows.lower}{Upper and lower bounds for the
		linear constraints; see details.}
	\item{method}{The optimization method; mostly determined by
		constraints.}
	\item{...}{Not used currently.}
}
\details{
	The method fits depmix models by the EM algorithm if there are no
	linear constraints on the parameters and if the transition model has
	no covariates. Otherwise the general optimizer donlp is used which 
	handles general linear (in-)equality constraints. 
		
	Three types of constraints can be specified on the parameters: fixed,
	equality, and general constraints. Constraint vectors should be of
	length npar(object). See help on getpars and setpars about the
	ordering of parameters. 
	
	The \code{equal} argument is used to specify equality constraints:
	parameters that get the same integer number in this vector are
	estimated to be equal. Any integers can be used in the vector except 0
	and 1, which indicate fixed and free parameters respectively. 

	Using the donlp optimizer a Newton-Raphson scheme is employed to
	estimate parameters subject to linear constraints by imposing: 
	
			bl <= A*x <= bu,
	
	where x is the parameter vector, bl is a vector of lower bounds, bu
	is a vector of upper bounds, and A is the constraint matrix. 

	The \code{conrows} argument is used to specify rows of A directly, and 
	the conrows.lower and conrows.upper arguments to specify the bounds on 
	the constraints. \code{conrows} is a matrix of npar(object) columns and
	one row for each constraint (ie a vector in the case of a single
	constraint). 
}
\value{
	\code{fit} returns an object of class depmix.fitted which contains 
	the original depmix object, and further has slots: 
	
	\describe{
		\item{message:}{Convergence information.}
		\item{conMat:}{The constraint matrix A, see details.}
		\item{posterior:}{The posterior state probabilities.}
	}
	
	The print function shows the \code{message} and the parameter
	estimates.
	
}
\author{Ingmar Visser}
\keyword{classes}
