##' Scientific notation.
##'
##' Format using scientific notation.
##'
##' @name scinot
##' @rdname scinot
##'
##' @param x number(s) to format.
##' @param digits number of significant digits in mantissa.
##' @param format format specification.
##' \code{format="expression"} results in an R expression.
##' \code{format="latex"} results in a latex expression.
##' \code{format="math"} is like "latex" but wraps the text in "$".
##' @param simplify logical.
##' If \code{simplify=TRUE}, then \eqn{1 \times 10^n}{1x10^n} is simplified to \eqn{10^n}{10^n}.
##'
##' @author Aaron A. King
##' @seealso \code{\link[scales]{scientific}}
##' @examples
##'
##' x <- c(0.0309595,8577676.441,10000)
##' scinot(x[2],4)
##' scinot(x[1],2,"latex")
##' sapply(x,scinot,digits=3,format='math')
##' scinot(x,digits=0,simplify=FALSE)
##' scinot(x,digits=0,simplify=TRUE)
##'
##' @export
scinot <- function (x, digits = 2, format = c("expression","latex","math"),
  simplify = FALSE) {
  digits <- as.integer(digits)
  format <- match.arg(format)
  simplify <- as.logical(simplify)
  x <- signif(as.numeric(x),digits=digits)
  sfun <- function (x) {
    ch <- floor(log10(abs(x)))
    mn <- x/10^ch
    if (is.na(x)) {
      switch(
        format,
        expression=expression(NA),
        latex="NA",
        math="NA"
      )
    } else if (x==0) {
      switch(
        format,
        expression=expression(0),
        latex="0",
        math="0"
      )
    } else if (simplify && mn == 1) {
      switch(
        format,
        expression=as.expression(bquote(10^.(ch))),
        latex=paste0("10$^{",ch,"}$"),
        math=paste0("$10^{",ch,"}$")
      )
    } else {
      switch(
        format,
        expression=as.expression(bquote(.(mn)%*%10^.(ch))),
        latex=paste0("{",mn,"}{$\\times$}10$^{",ch,"}$"),
        math=paste0("${",mn,"}{\\times}10^{",ch,"}$")
      )
    }
  }
  sapply(x,sfun)
}
