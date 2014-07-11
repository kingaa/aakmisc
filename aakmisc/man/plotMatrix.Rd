\name{plotMatrix}
\alias{plotMatrix}
\alias{plotMatrix.list}
\alias{plotMatrix.data.frame}
\alias{print.plotMatrix}
\title{A scatterplot matrix with densities on the diagonal.}
\description{
  A special scatterplot matrix.
}
\usage{
\method{plotMatrix}{data.frame}(data, marg.exp = 0.02, labels = names(data),
     alpha = 1, pch = 16, size = unit(2,"mm"), \dots)
\method{plotMatrix}{list}(data, marg.exp = 0.02, labels = names(data),
     alpha = 1, pch = 16, size = unit(2,"mm"), \dots)
\method{print}{plotMatrix}(x, newpage = is.null(vp), vp = NULL, \dots)
}
\arguments{
  \item{data}{
    Data to plot.
  }
  \item{marg.exp}{
    Fraction by which to expand the plot at the margins.
  }
  \item{labels}{
    Names of variables plotted.
  }
  \item{alpha, pch, size}{
    Refer to the plotted points in the scatterplots.
  }
  \item{\dots}{
    optional arguments, passed to \code{\link{hist}}.
  }
  \item{x}{
    \code{plotMatrix} object to display.
  }
  \item{newpage}{
    logical; if \code{TRUE}, \code{grid.newpage()} will be called before the graphics are drawn.
  }
  \item{vp}{
    viewport to use.
    See \code{\link[grid]{viewport}}.
  }
}
\examples{
  \dontrun{
x <- data.frame(a=rexp(n=1000,rate=1/3),b=rnorm(1000))
mutate(x,c=a+b^2,d=a-b^3) -> x

print(plotMatrix(x,alpha=0.2))

g <- plotMatrix(
                x[-2],
                labels=c(
                  expression(alpha),
                  expression(beta),
                  expression(phi)
                  ),
                alpha=0.3
                )
print(g)

print(plotMatrix(as.list(x),alpha=0.2,breaks='scott'))
  }
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
