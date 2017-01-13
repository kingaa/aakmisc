scinot <- function (x, digits = 2, format = c("expression","latex","math"),
                    simplify = FALSE) {
  if (length(x) > 1)
    warning("only the first element of ",sQuote("x")," will be formatted")
  format <- match.arg(format)
  x <- signif(as.numeric(x[1]),digits=digits)
  ch <- floor(log10(abs(x)))
  mn <- x/10^ch
  switch(
      format,
      expression={
          if (mn == 1)
              bquote(10^.(ch))
          else
              bquote(.(mn)%*%10^.(ch))
      },
      latex={
          if (mn == 1)
              paste0("10$^{",ch,"}$")
          else
              paste0("{",mn,"}{$\\times$}10$^{",ch,"}$")
      },
      math={
          if (mn == 1)
              paste0("$10^{",ch,"}$")
          else
              paste0("${",mn,"}{\\times}10^{",ch,"}$")
      }
  )
}
