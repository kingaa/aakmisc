\name{trnc}
\alias{trnc}
\title{Truncation of plots.}
\description{
  Truncate to the specified window.
}
\usage{
trnc(x, range=c(0,1), only.finite=TRUE)
}
\arguments{
  \item{x}{
    Numeric vector of values to manipulate.
  }
  \item{range}{
    Numeric vector of length two giving desired output range.
  }
  \item{only.finite}{
    if TRUE (the default), will only modify finite values.
    }
}
\examples{
trnc(c(-1,0.5,1,2,NA))
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
\seealso{
\code{\link[scales]{censor}}
}
