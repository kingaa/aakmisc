\name{random}
\alias{random.org}
\alias{urandom}
\alias{rngControl}
\alias{rngSeeds}
\title{Functions for generating and working with truly random integers.}
\description{
  Functions for generating and working with truly random seeds.
}
\usage{
random.org(n = 10, rnd = "new")
urandom(n = 10)
rngControl(expr, seed = NULL)
rngSeeds(n, seed = NULL)
}
\arguments{
  \item{n}{
    Number of integers required.
  }
  \item{rnd}{
    random.org parameter
  }
  \item{expr}{
    Expression to be evaluated with RNG control.
  }
  \item{seed}{
    RNG seed.
  }
}
\examples{
  \dontrun{
  random.org(n=5)
  }
  seed <- urandom(n=1)
  seeds <- rngSeeds(5,seed=seed)
  set.seed(seed)
  runif(5)
  rngControl(runif(5),seed=seed[1])
  rngControl(runif(5),seed=seed[1])
  runif(5)
  set.seed(seed)
  runif(5)
  runif(5)
}
\references{
  \url{http://www.random.org}
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
