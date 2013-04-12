startTunnel <- function (port,
                         remotehost = getOption("aakmisc.remotehost"),
                         sleep = 5) {
  if (missing(port))
    port <- ceiling(runif(n=1,min=49151,max=65535))
  if (is.null(remotehost))
    stop("must specify ",sQuote("remotehost"))
  pidfile <- tempfile()
  cmd <- paste0("ssh -NL ",port,":localhost:5432 ",
                remotehost," & echo $! > ",pidfile)
  system(cmd)
  Sys.sleep(sleep)
  list(port=port,pidfile=pidfile)
}

stopTunnel <- function (info) {
  if (is.character(info))
    info <- list(pidfile=info)
  pid <- system(paste0("cat ",info$pidfile),intern=T)
  stat <- system2("kill",pid)
  if (stat==0) {
    file.remove(info$pidfile)
    return(TRUE)
  } else {
    cat(sQuote("stopTunnel")," failed: return status ",stat,"\n")
    return(FALSE)
  }
}

