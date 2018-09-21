##' matrix2latex
##'
##' Format a matrix for latex.
##'
##' @name matrix2latex
##' @rdname matrix2latex
##'
##' @param x matrix
##' @param type latex matrix environment
##'
##' @export
matrix2latex <- function (x, type = "pmatrix") {
  type <- as.character(type)
  mrow <- function (x) {
    paste0(paste0(x,collapse=" & "),sep=" \\\\")
  }
  head <- paste(c("\\","begin{",type,"}"),collapse="")
  body <- paste0(apply(x,1,mrow),collapse=" ")
  tail <- paste0(c('\\',"end{",type,"}"),collapse="")
  paste0(c(head,body,tail),collapse=" ")
}
