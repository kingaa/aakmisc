##' function for truncating data to window
##' suitable for use in ggplot::scale_{x,y}_{continuous,discrete} functions

trnc <- function (x, range=c(0,1), only.finite=TRUE) {
    force(range)
    finite <- if (only.finite) is.finite(x) else TRUE
    x[finite & x < range[1]] <- range[1]
    x[finite & x > range[2]] <- range[2]
    x
}
