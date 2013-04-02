\name{random}
\alias{random.org}
\alias{urandom}
\title{Functions for generating truly random integers.}
\description{
  Functions for generating truly random integers.
}
\usage{
random.org(n = 10, rnd = "new")
urandom(n = 10)
}
\arguments{
  \item{n}{
    Number of integers required.
  }
  \item{rnd}{
    random.org parameter
  }
}
\examples{
  \dontrun{
  random.org(n=5)
  }
  seed <- urandom(n=1)
  set.seed(seed)
  runif(5)
}
\references{
  \url{http://www.random.org}
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
