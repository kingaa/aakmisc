\name{bake}
\alias{bake}
\title{Run a computation only if needed.}
\description{
  If the computation has already been performed, load the results, else run the computation and store the results.
}
\usage{
bake(file, expr)
}
\arguments{
  \item{file}{
    Name of the RDS file in which the result will be stored or retrieved, as appropriate.
  }
  \item{expr}{
    Expression to be evaluated.
  }
}
\examples{
  \dontrun{
  bake(file="example.rds",{
    x <- runif(1000)
    mean(x)
  })
  }
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
