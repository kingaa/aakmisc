plotMatrix.internal <- function (data, marg.exp=0.02, labels = names(data),
                                 alpha = 1, pch = 16, size = unit(2,"mm"),
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
  class(gob) <- c("plotMatrix",class(gob))
  invisible(gob)
}

plotMatrix <- function (data, ...) UseMethod("plotMatrix")

plotMatrix.default <- function (data, ...) {
  stop(sQuote("plotMatrix")," not defined for ",sQuote("data"),
              " of class ",sQuote(class(data)))
}

plotMatrix.list <- plotMatrix.internal

plotMatrix.data.frame <- plotMatrix.internal

print.plotMatrix <- function (x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()
  if (!is.null(vp)) pushViewport(vp)
  grid.draw(x)
  if (!is.null(vp)) popViewport()
}
