\name{depmixS4-package}

\alias{depmixS4-package}
\alias{depmixS4}

\docType{package}
\title{
depmixS4 provides classes for fitting hidden Markov models
}
\description{
depmixS4 fits hidden Markov models on (multiple) sets of observations by the use
of either EM or direct optimization of the log-likelihood; the latter is used to
incorporate general linear constraints into models. The response densities for 
each state may be chosen from the GLM family, or a multinomial. User defined 
response densities are easy to add.
}
\details{
\tabular{ll}{
Package: \tab depmixS4\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2008-06-09\cr
License: \tab GPL\cr
}

Model fitting is done in two steps; first, models are specified through the
depmix function, which uses standard arguments to eg specify GLM densities
for the responses; second, the model is fitted; the fit function is used to 
provide extra arguments concerning the optimization, such as providing 
constraints on parameters. 

}
\author{
Ingmar Visser & Maarten Speekenbrink
Maintainer: i.visser@uva.nl
}
\references{

On hidden Markov models: Lawrence R. Rabiner (1989).  A tutorial on
hidden Markov models and selected applications in speech recognition.
\emph{Proceedings of IEEE}, 77-2, p.  267-295.

On latent class models: A. L. McCutcheon (1987).  \emph{Latent class
analysis}.  Sage Publications.

}
\keyword{ package }
\seealso{
\code{\link[<depmixS4>:<depmixS4>-package]{depmix}}
}
\examples{
# These should be added at some point ...
}