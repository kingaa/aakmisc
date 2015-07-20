\name{bake}
\alias{bake}
\alias{broil}
\title{Run a computation only if needed, saving the results for later use.}
\description{
  If the computation has already been performed, load the results, else run the computation and store the results.
}
\usage{
bake(file, expr)
broil(file, expr)
}
\arguments{
  \item{file}{
    Name of the RDS file (for \code{bake}) or RDA file (for \code{broil}) in which the result will be stored or retrieved, as appropriate.
  }
  \item{expr}{
    Expression to be evaluated.
  }
}
\examples{
  \dontrun{
  bake(file="example1.rds",{
    x <- runif(1000)
    mean(x)
  })

  broil(file="example2.rds",{
    x <- runif(10)
    y <- rnorm(n=10,mean=3*x+5,sd=2)
  })

  plot(x,y)  
  }
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
