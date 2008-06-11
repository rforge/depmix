\name{fit}

\docType{method}

\alias{fit}
\alias{depmix.fit}
\alias{fit,depmix-method}

\alias{show,depmix.fitted-method}
\alias{summary,depmix.fitted-method}

\title{Fit depmix models}

\description{

	\code{fit} optimizes parameters of \code{\link{depmix}} models, optionally
	subject to general linear (in)equality constraints.}

\usage{
	
	\S4method{fit}{depmix}(object, fixed=NULL, equal=NULL, conrows=NULL,
		conrows.upper=0, conrows.lower=0, method=NULL,...)
	
	\S4method{summary}{depmix.fitted}(object)
	
}

\arguments{

	\item{object}{An object of class \code{depmix}.}
	
	\item{fixed}{Vector of mode logical indicating which parameters should 
		be fixed.}

	\item{equal}{Vector indicating equality constraints; see details.}

	\item{conrows}{Rows of a general linear constraint matrix; see details.}

	\item{conrows.upper, conrows.lower}{Upper and lower bounds for the
		linear constraints; see details.}

	\item{method}{The optimization method; mostly determined by
		constraints.}

	\item{...}{Further arguments passed on to the optimization methods.}

}

\details{ 

	Models are fitted by the EM algorithm if there are no constraints on
	the parameters.  Otherwise the general optimizer \code{donlp2} from the
	package \code{Rdonlp2} is used which handles general linear
	(in-)equality constraints.
	
	Three types of constraints can be specified on the parameters: fixed,
	equality, and general linear (in-)equality constraints.  Constraint
	vectors should be of length npar(object).  See help on
	\code{\link{getpars}} and \code{\link{setpars}} about the ordering of
	parameters.
	
	The \code{equal} argument is used to specify equality constraints:
	parameters that get the same integer number in this vector are
	estimated to be equal. Any integers can be used in this way except 0
	and 1, which indicate fixed and free parameters, respectively. 

	Using the \code{donlp2} optimizer a Newton-Raphson scheme is employed
	to estimate parameters subject to linear constraints by imposing:
	
			bl <= A*x <= bu,
	
	where x is the parameter vector, bl is a vector of lower bounds, bu is
	a vector of upper bounds, and A is the constraint matrix.

	The \code{conrows} argument is used to specify rows of A directly, and
	the conrows.lower and conrows.upper arguments to specify the bounds on
	the constraints.  \code{conrows} is a matrix of npar(object) columns
	and one row for each constraint (a vector in the case of a single
	constraint).
	
	\code{\link{llratio}} performs a log-likelihood ratio test on two
	\code{fit}'ted models; the first object should have the largest degrees
	of freedom (find out by using \code{\link{freepars}}).
	
}

\value{
	
	\code{fit} returns an object of class
	\code{\link{depmix.fitted}} which contains the
	original \code{depmix} object, and further has slots:
	
	\item{message}{: Convergence information.}
	
	\item{conMat}{: The constraint matrix A, see details.}
	
	\item{posterior}{: The posterior state sequence (computed with the
	viterbi algorithm), and the posterior probabilities (delta
	probabilities in Rabiner, 1989, notation).}
	
	The print method shows the \code{message} along with the likelihood and
	AIC and BIC; the summary method prints the parameter estimates.

	Posterior densities and the viterbi state sequence can be accessed
	through \code{\link{posterior}}.
	
}

\references{
	
	Lawrence R. Rabiner (1989).  A tutorial on hidden Markov models and
	selected applications in speech recognition.  \emph{Proceedings of
	IEEE}, 77-2, p.  267-295.
	
}

\examples{

data(speed)
# 2-state model on the RTs of the speed data with random 
# starting values for the transition pars (without those EM does not get off the ground)
set.seed(1)
mod <- depmix(rt~1,data=speed,nstates=2,trstart=runif(4))
# fit the model
mod1 <- fit(mod)
mod1 # to see the logLik and optimization information
# to see the parameters
summary(mod1)


data(balance)
# four binary items on the balance scale task

# now fit some latent class models
trstart=c(1,0,0,1) 
# as this is a latent class model, the transitions are not optimized 
# and hence these values are arbitrary
instart=c(0.5,0.5)
set.seed(1)
respstart=runif(16)
# note that ntimes argument is used to make this a mixture model
mod <- depmix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	respstart=respstart,trstart=trstart,instart=instart,
	ntimes=rep(1,nrow(balance)))

mod1 <- fit(mod)

# add age as covariate on class membership by using the prior argument
trstart=c(1,0,0,1) 
instart=c(0.5,0.5,0,0) # we need the initial probs and the coefficients of age 
set.seed(2)
respstart=runif(16)
trstart=c(1,0,0,1)
mod2 <- depmix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
	family=list(multinomial(),multinomial(),multinomial(),multinomial()),
	trstart=trstart, instart=instart, respstart=respstart,
	ntimes=rep(1,nrow(balance)), prior=~age, initdata=balance)

mod3 <- fit(mod2)

# check the likelihood ratio; adding age significantly improves the goodness-of-fit
llratio(mod3,mod1)

}

\author{Ingmar Visser}

\keyword{methods}
