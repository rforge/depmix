\name{responses}

\alias{responses}

\alias{BINOMresponse}
\alias{GAMMAresponse}
\alias{MULTINOMresponse}
\alias{MVNresponse}
\alias{NORMresponse}
\alias{POISSONresponse}

\title{Response models currently implemented in depmix.}

\description{Depmix contains a number of default response models. We provide a
brief description of these here.}

\section{BINOMresponse}{
	
	\code{BINOMresponse} is a binomial response model. It derives from the basic
	\code{\link{GLMresponse}} class.

  \describe{
    \item{y:}{The dependent variable can be either a
  binary vector, a factor, or a 2-column matrix, with successes and misses.}
    \item{x:}{The design matrix.}
    \item{Parameters:}{A named list with a single element ``coefficients'',
    which contains the GLM coefficients.}
  }
	
}

\section{GAMMAresponse}{

	\code{GAMMAresponse} is a model for a Gamma distributed response.
  It extends the basic \code{\link{GLMresponse}} class directly.

  \describe{
    \item{y:}{The dependent variable.}
    \item{x:}{The design matrix.}
    \item{Parameters:}{A named list with a single element ``coefficients'',
    which contains the GLM coefficients.}
  }

}

\section{MULTINOMresponse}{

	\code{MULTINOMresponse} is a model for a multinomial distributed response.
  It extends the basic \code{\link{GLMresponse}} class, although the
  functionality is somewhat different from other models that do so.

  \describe{
    \item{y:}{The dependent variable. This is a binary matrix with N rows and
    Y columns, where Y is the total number of categories.}
    \item{x:}{The design matrix.}
    \item{Parameters:}{A named list with a single element ``coefficients'',
    which is an \code{ncol(x)} by \code{ncol(y)} matrix which contains the GLM
    coefficients.}
  }

}

\section{MVNresponse}{

	\code{MVNresponse} is a model for a multivariate normal distributed response.

  \describe{
    \item{y:}{The dependent variable. This is a matrix.}
    \item{x:}{The design matrix.}
    \item{Parameters:}{A named list with a elements ``coefficients'',
    which contains the GLM coefficients, and ``Sigma'', which contains the
    covariance matrix.}
  }

}

\section{NORMresponse}{

	\code{NORMresponse} is a model for a normal (Gaussian) distributed response.
  It extends the basic \code{\link{GLMresponse}} class directly.

  \describe{
    \item{y:}{The dependent variable.}
    \item{x:}{The design matrix.}
    \item{Parameters:}{A named list with elements ``coefficients'',
    which contains the GLM coefficients, and ``sd'', which contains the
    standard deviation.}
  }

}

\section{POISSONresponse}{

	\code{POISSONresponse} is a model for a Poisson distributed response.
  It extends the basic \code{\link{GLMresponse}} class directly.

  \describe{
    \item{y:}{The dependent variable.}
    \item{x:}{The design matrix.}
    \item{Parameters:}{A named list with a single element ``coefficients'',
    which contains the GLM coefficients.}
  }

}

\section{examples}{
	
	mod <- GLMresponse(sample(1:3,1000,rep=TRUE)~1,family=multinomial())
	fit(mod)
	
	x <- rnorm(1000)
	p <- inv.logit(x)
	ss <- rbinom(1000,1,p)
	mod <- GLMresponse(cbind(ss,1-ss)~x,family=binomial())
	fit(mod)
	
	mod <- GLMresponse(sample(1:3,1000,rep=TRUE)~1,family=multinomial())
	fit(mod)

	mod <- GLMresponse(sample(1:3,1000,rep=TRUE)~1,family=multinomial())
	fit(mod)

	mod <- GLMresponse(sample(1:3,1000,rep=TRUE)~1,family=multinomial())
	fit(mod)

	mod <- GLMresponse(sample(1:3,1000,rep=TRUE)~1,family=multinomial())
	fit(mod)

	mod <- GLMresponse(sample(1:3,1000,rep=TRUE)~1,family=multinomial())
	fit(mod)


}

\author{Maarten Speekenbrink}

\keyword{models}
