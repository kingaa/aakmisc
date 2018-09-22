##' ssh tunneling
##'
##' Setting up ssh tunnels for database access.
##'
##' @name tunnel
##' @rdname tunnel
##' @inheritParams db
##' @include db.R
##'
##' @param remotehost Hostname of PostgreSQL server.  An ssh tunnel to this host
##' will be created.
##' @param pid ID of ssh tunnel process.  Set automatically by
##' \code{startTunnel}.
##' @param sleep Time in seconds to sleep after initiating the ssh tunnel.
##'
NULL

##' @name startTunnel
##' @rdname tunnel
##' @export
startTunnel <- function (port = NULL,
  remotehost = getOption("aakmisc.remotehost", NULL),
  user = getOption("aakmisc.user", NULL),
  sleep = 5) {
  if (is.null(port))
    port <- ceiling(runif(n=1,min=49151,max=65535))
  if (is.null(remotehost))
    stop(sQuote("remotehost")," unspecified")
  if (is.null(user))
    user <- Sys.getenv("USER")
  ## stop any existing ssh tunnel
  pid <- getOption("aakmisc.tunnelpid",NULL)
  if (!is.null(pid)) stopTunnel(pid=pid)
  ## start ssh tunnel and record pid
  pidfile <- tempfile()
  cmd <- paste0("ssh -NL ",port,":localhost:5432",
    " -l ",user," ",
    remotehost," & echo $! > ",pidfile)
  system(cmd)
  pid <- scan(pidfile,what=integer(0),quiet=TRUE)
  unlink(pidfile)
  options(
    aakmisc.tunnelpid=as.integer(pid),
    aakmisc.port=port
  )
  Sys.sleep(sleep)
  invisible(list(port=port,tunnelpid=pid,remotehost=remotehost))
}

##' @name stopTunnel
##' @rdname tunnel
##' @export
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
