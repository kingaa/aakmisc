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
            
        } else if (simplify && mn == 1) {
            switch(
                format,
                expression=bquote(10^.(ch)),
                latex=paste0("10$^{",ch,"}$"),
                math=paste0("$10^{",ch,"}$")
            )

        } else {
            switch(
                format,
                expression=bquote(.(mn)%*%10^.(ch)),
                latex=paste0("{",mn,"}{$\\times$}10$^{",ch,"}$"),
                math=paste0("${",mn,"}{\\times}10^{",ch,"}$")
            )
        }
    }
    sapply(x,sfun)
}
