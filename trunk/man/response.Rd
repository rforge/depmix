\name{response}
\docType{method}
\alias{response}
\alias{predict,response-method}
\alias{dens,response-method}
\alias{npar,response-method}
\alias{transInit}
\alias{GLMresponse}
\alias{transInit,response-method}
\alias{GLMresponse,response-method}
\title{Methods for creating depmix response models}
\description{Create \code{\link[depmix]{response}} models.}
\usage{
	
	GLMresponse(formula, family, data, pstart=NULL, fixed=NULL)
	
	transInit(formula, family=multinomial(), data, nstates, 
		pstart=NULL, prob=TRUE, fixed=NULL)
	
	\S4methods{npar}{response}(object,...)
}
\arguments{
	\item{object}{An object of class \code{response} or one of its
		descendents.}
	\item{formula}{A model formula.}
	\item{family}{A family object; currently only multinomial() and
		gaussian() are allowed options.}
	\item{data}{An optional data.frame to interpret the variables from the 
		formula argument in.}
	\item{nstates}{The number of states of the model.}
	\item{pstart}{Starting values for the coefficients and other
		parameters, ie the standard deviation for the gaussian() family.}
	\item{fixed}{Logical vector indicating which paramters are to be fixed.}
	\item{prob}{Logical indicating whether the starting values for
		multinomial() family models are probabilities or logistic
		parameters (see details).}
}
\details{
	
	Both methods use the familiar formula interface from linear and general
	linear models to specify how responses (or transition or prior
	parameters) depend on covariates.
	
	The GLMresponse model is an interface to the glm functions of which
	the functionality is leant, ie predict, fit and density functions. 
	
	The transInit response model provides functionality for multinomial
	responses that are currently fit using nnet (this may change in the
	future but this should not affect the interface of this function). Note
	that the transInit model actually lacks a reponse, ie the y-slot is
	empty, at the time of construction, as the transitions are not
	observed. 
	
}
\value{
	\code{GLMresponse} and \code{transInit} return objects of class
	\code{GLMresponse} and \code{transInit} respectively; both classes
	extend the \code{\link[depmix]{response-class}}. 
	
	Objects with class \code{response} have at least methods: \code{predict}
	to return \code{predict}'ed responses, \code{dens} to return the
	\code{dens}'ity of responses. 
}
\author{Ingmar Visser}
\keyword{classes}
