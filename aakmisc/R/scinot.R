scinot <- function (x, digits = 2, format = c("expression","latex","math")) {
  if (length(x) > 1)
    warning("only the first element of ",sQuote("x")," will be formatted")
  format <- match.arg(format)
  x <- signif(x[1],digits=digits)
  ch <- floor(log10(abs(x)))
  mn <- x/10^ch
  switch(
         format,
         expression={
           bquote(.(mn)%*%10^.(ch))
         },
         latex={
           paste0("{",mn,"}{$\\times$}10$^{",ch,"}$")
         },
         math={
           paste0("${",mn,"}{\\times}10^{",ch,"}$")
         }
         )
}
