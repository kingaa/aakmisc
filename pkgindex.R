invisible(require(tools,quietly=TRUE))

winrepos <- './dists'
srcrepos <- './dists'

invisible(eval(parse(text=commandArgs(trailingOnly=TRUE))))

Sys.umask("0022")
## if ((!is.null(winrepos)) && (length(winrepos)>0))
write_PACKAGES(dir=winrepos,type='win.binary')
## if ((!is.null(srcrepos)) && (length(srcrepos)>0))
write_PACKAGES(dir=srcrepos,type='source')
