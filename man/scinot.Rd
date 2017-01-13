\name{scinot}
\alias{scinot}
\title{Scientific notation.}
\description{
  Format using scientific notation.
}
\usage{
scinot(x, digits = 2, format = c("expression","latex","math"),
       simplify = FALSE)
}
\arguments{
  \item{x}{
    Number to format.
  }
  \item{digits}{
    Number of significant digits in mantissa.
  }
  \item{format}{
    Format specification.
    \code{type="expression"} results in an R expression.
    \code{type="latex"} results in a latex expression.
    \code{type="math"} is like "latex" but wraps the text in "$".
  }
  \item{simplify}{
    logical.
    If \code{simplify=TRUE}, then \eqn{1 \times 10^n}{1x10^n} is simplified to \eqn{10^n}{10^n}.
  }
}
\examples{
x <- c(0.0309595,8577676.441)
scinot(x[2],4)
scinot(x[1],2,"latex")
sapply(x,scinot,digits=3,format='math')
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
