scinot <- function (x, digits = 2, format = c("expression","latex","math"),
  simplify = FALSE) {
  digits <- as.integer(digits)
  format <- match.arg(format)
  simplify <- as.logical(simplify)
  x <- signif(as.numeric(x),digits=digits)
  sfun <- function (x) {
    ch <- floor(log10(x))
    mn <- x/10^ch
    switch(
      format,
      expression={
        if (simplify && mn == 1)
          bquote(10^.(ch))
        else
          bquote(.(mn)%*%10^.(ch))
      },
      latex={
        if (simplify && mn == 1)
          paste0("10$^{",ch,"}$")
        else
          paste0("{",mn,"}{$\\times$}10$^{",ch,"}$")
      },
      math={
        if (simplify && mn == 1)
          paste0("$10^{",ch,"}$")
        else
          paste0("${",mn,"}{\\times}10^{",ch,"}$")
      }
    )
  }
  sapply(x,sfun)
}
