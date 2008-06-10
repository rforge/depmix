\name{depmix-methods}

\docType{methods}

\alias{logLik}
\alias{logLik,depmix-method}

\alias{nobs}
\alias{nobs,depmix-method}

\alias{npar}
\alias{npar,depmix-method}

\alias{freepars}
\alias{freepars,depmix-method}

\alias{setpars}
\alias{setpars,depmix-method}

\alias{getpars}
\alias{getpars,depmix-method}

\title{ Depmix methods. }

\description{
Computes the log likelihood of a depmix object.
}

\usage{
	\S4method{logLik}{depmix}(object,method="lystig")

	\S4method{nobs}{depmix}(object, ...)
	
	\S4method{npar}{depmix}(object)
	\S4method{freepars}{depmix}(object)
	\S4method{setpars}{depmix}(object,values, which="pars",...)
	\S4method{getpars}{depmix}(object,which="pars",...)	

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{object}{A \code{depmix} pobject}
  
  \item{method}{The log likelihood can be computed by either the forward
  backward algorithm from Rabiner, 1989, or by the method of Lystig &
  Hughes, 2002.  The latter is the default as it is faster because in the
  forward backward routine the state and transition smoothed probabilities
  are also computed which are not neccessary for the log likelihood. Those
  smoothed variables, and the forward and backward variables are accessible
  through the \code{forwardbackward} function.}
  
  \item{values}{To be used in \code{setpars} to set new parameter values.}
  
  \item{which}{The default "pars" returns a vector of all parameters of a
	  \code{depmix} object; the alternative value "fixed" return a
	  logical vector of the same length indicating which parameters are
	  fixed. The setpars functions sets parameters (or the logical fixed 
	  vector) to new values; \code{setpars} also recomputes the dens,
	  trans and init slots of \code{depmix} objects. Note that the 
	  \code{getpars} and \code{setpars} functions for \code{depmix} 
	  objects simply call the functions of the same name for the response
	  and transition models.}
	  
  \item{...}{Not used currently.}

}
\value{
  
	\item{logLik} {returns a \code{logLik} object with attributes \code{df} and \code{nobs}.}
	
	\item{nobs} {returns the number of observations (used in computing the BIC).}

	\item{npar} {returns the number of paramters of a model.}

	\item{freepars} {returns the number of non-fixed parameters.}

	\item{setpars} {returns a depmix object with new parameter values.}

	\item{getpars} {returns a vector with the current parameter values.}

}
\author{Ingmar Visser}
\examples{

}

\keyword{methods}

