startTunnel <- function (port = NULL,
                         remotehost = getOption("aakmisc.remotehost",
                           "kinglab.eeb.lsa.umich.edu"),
                         sleep = 5) {
  if (is.null(port))
    port <- ceiling(runif(n=1,min=49151,max=65535))
  if (is.null(remotehost))
    stop("must specify ",sQuote("remotehost"))
  ## stop any existing ssh tunnel
  pid <- getOption("aakmisc.tunnelpid",NULL)
  if (!is.null(pid)) stopTunnel(pid=pid)
  ## start ssh tunnel and record pid
  pidfile <- tempfile()
  cmd <- paste0("ssh -NL ",port,":localhost:5432 ",
                remotehost," & echo $! > ",pidfile)
  stat <- system(cmd)
  pid <- system2("cat",pidfile,stdout=TRUE)
  unlink(pidfile)
  options(
          aakmisc.tunnelpid=as.integer(pid),
          aakmisc.port=port
          )
  Sys.sleep(sleep)
  invisible(list(port=port,tunnelpid=pid,remotehost=remotehost))
}

stopTunnel <- function (...,
                        pid = getOption("aakmisc.tunnelpid",NULL)) {
  if (is.null(pid))
    stop("must specify ",sQuote("pid"))
  stat <- system2("kill",pid)
  if (stat!=0) {
    cat(sQuote("stopTunnel")," failed: return status ",stat,"\n")
    invisible(FALSE)
  } else {
    options(
            aakmisc.tunnelpid=NULL,
            aakmisc.port=NULL
            )
    invisible(TRUE)
  }
}
