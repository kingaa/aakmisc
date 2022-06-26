##' Truncation of plots.
##'
##' Truncate to the specified window.
##'
##' \code{trnc} is a function for truncating data to a specified window.
##' It is suitable for use in \code{scale_{x,y}_{continuous,discrete}}, for example.
##'
##' @name trnc
##' @rdname trnc
##'
##' @param x Numeric vector of values to manipulate.
##' @param range Numeric vector of length two giving desired output range.
##' @param only.finite if TRUE (the default), will only modify finite values.
##' @seealso \code{\link[scales]{censor}}
##' @examples
##'
##' trnc(c(-1,0.5,1,2,NA))
##'
##' @export
trnc <- function (x, range=c(0,1), only.finite=TRUE) {
  force(range)
  finite <- if (only.finite) is.finite(x) else TRUE
  x[finite & x < range[1]] <- range[1]
  x[finite & x > range[2]] <- range[2]
  x
}
