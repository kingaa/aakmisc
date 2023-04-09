##' A scatterplot matrix with densities on the diagonal.
##'
##' A special scatterplot matrix.
##'
##' @name plotMatrix
##' @rdname plotMatrix
##' @aliases plotMatrix plotMatrix.list plotMatrix.data.frame print.aakplot
##'
##' @param data Data to plot.
##' @param marg.exp Fraction by which to expand the plot at the margins.
##' @param labels Names of variables plotted.
##' @param alpha,pch,size Refer to the plotted points in the scatterplots.
##' @param \dots optional arguments, passed to \code{\link{hist}}.
##' @param x \code{plotMatrix} object to display.
##' @param newpage logical; if \code{TRUE}, \code{grid.newpage()} will be called
##' before the graphics are drawn.
##' @param vp viewport to use.  See \code{\link[grid]{viewport}}.
##' @examples
##'
##'   \dontrun{
##' x <- data.frame(a=rexp(n=1000,rate=1/3),b=rnorm(1000))
##' mutate(x,c=a+b^2,d=a-b^3) -> x
##'
##' print(plotMatrix(x,alpha=0.2))
##'
##' g <- plotMatrix(
##'                 x[-2],
##'                 labels=c(
##'                   expression(alpha),
##'                   expression(beta),
##'                   expression(phi)
##'                   ),
##'                 alpha=0.3
##'                 )
##' print(g)
##'
##' print(plotMatrix(as.list(x),alpha=0.2,breaks='scott'))
##'   }
##'
##' @include aaa.R
##' @importFrom grid unit grid.newpage
##' viewport pushViewport popViewport
##' gList gTree gpar grid.draw grid.layout grid.newpage
##' packGrob placeGrob pointsGrob rectGrob textGrob frameGrob xaxisGrob yaxisGrob
##' @importFrom grDevices grey
##' @importFrom graphics hist
##' @importFrom utils head
##'
NULL

plotMatrix.internal <- function (data, marg.exp=0.02, labels = names(data),
  alpha = 1, pch = 16, size = unit(0.03,"npc"),
  ...) {

  nvar <- length(data)

  histos <- lapply(data,hist,plot=FALSE,...,warn.unused=FALSE)

  ranges <- lapply(
    histos,
    function (x) {
      r <- range(x$breaks)
      d <- marg.exp*diff(r)
      r+c(-d,d)
    }
  )

  splot <- function (a, b, xaxis, yaxis) {
    gTree(
      children=gList(
        rectGrob(
          gp=gpar(col='black',fill=NA)
        ),
        pointsGrob(
          x=data[[a]],
          y=data[[b]],
          pch=pch,
          size=size,
          gp=gpar(col="black",alpha=alpha)
        ),
        if (xaxis[1]) xaxisGrob(main=xaxis[2]) else NULL,
        if (xaxis[1]) {
          if (xaxis[2])
            textGrob(labels[a],y=unit(-3,"lines"))
          else
            textGrob(labels[a],y=unit(1,"npc")+unit(3,"lines"))
        } else NULL,
        if (yaxis[1]) yaxisGrob(main=yaxis[2]) else NULL,
        if (yaxis[1]) {
          if (yaxis[2])
            textGrob(labels[b],x=unit(-3,"lines"))
          else
            textGrob(labels[b],x=unit(1,"npc")+unit(3,"lines"))
        } else NULL
      ),
      vp=viewport(
        xscale=ranges[[a]],
        yscale=ranges[[b]]
      )
    )
  }

  hplot <- function (a, xaxis) {
    y <- histos[[a]]$density
    x <- head(histos[[a]]$breaks,-1)
    w <- diff(histos[[a]]$breaks)
    gTree(
      children=gList(
        rectGrob(
          gp=gpar(col='black',fill=NA)
        ),
        rectGrob(
          x=x,
          y=0,
          width=w,
          height=y,
          just=c(0,0),
          default.units='native',
          gp=gpar(fill=grey(0.8))
        ),
        if (xaxis[1]) xaxisGrob(main=xaxis[2]) else NULL,
        if (xaxis[1]) {
          if (xaxis[2])
            textGrob(labels[a],y=unit(-3,"lines"))
          else
            textGrob(labels[a],y=unit(1,"npc")+unit(3,"lines"))
        } else NULL
      ),
      vp=viewport(
        xscale=ranges[[a]],
        yscale=c(0,(1+marg.exp)*max(histos[[a]]$density))
      )
    )
  }

  fg <- frameGrob(layout=grid.layout(nrow=length(data),ncol=length(data)))
  for (i in seq_len(nvar)) {
    for (j in seq_len(nvar)) {
      if (i == j) {
        fg <- placeGrob(
          fg,
          hplot(i,
            xaxis=c(
              ((j==1)&&(i%%2==0))||((j==nvar)&&(i%%2==1)),
              j==nvar
            )
          ),
          row=i,col=i
        )
      } else {
        fg <- placeGrob(
          fg,
          splot(i,j,
            xaxis=c(
              ((j==1)&&(i%%2==0))||((j==nvar)&&(i%%2==1)),
              j==nvar
            ),
            yaxis=c(
              ((i==1)&&(j%%2==0))||((i==nvar)&&(j%%2==1))||
                ((i==1)&&(j==nvar)),
              (i==1)
            )
          ),
          row=j,col=i
        )
      }
    }
  }
  gob <- packGrob(
    frameGrob(),
    fg,
    width=unit(1,"npc")-unit(8,"lines"),
    height=unit(1,"npc")-unit(8,"lines")
  )
  class(gob) <- c("aakplot",class(gob))
  gob
}

##' @rdname plotMatrix
##' @export
plotMatrix <- function (data, ...) UseMethod("plotMatrix")

plotMatrix.default <- function (data, ...) {
  stop(sQuote("plotMatrix")," not defined for ",sQuote("data"),
    " of class ",sQuote(class(data)))
}

##' @rdname plotMatrix
##' @method plotMatrix list
##' @export
plotMatrix.list <- plotMatrix.internal

##' @rdname plotMatrix
##' @method plotMatrix data.frame
##' @export
plotMatrix.data.frame <- plotMatrix.internal

##' @rdname plotMatrix
##' @method print aakplot
##' @export
print.aakplot <- function (x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()
  if (!is.null(vp)) pushViewport(vp)
  grid.draw(x)
  if (!is.null(vp)) popViewport()
}
