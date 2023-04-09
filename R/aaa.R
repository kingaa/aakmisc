##' Startup and shutdown procedures
##'
##' @name cleanup
##' @rdname tunnel
##' @include tunnel.R
##' @importFrom ggplot2 theme_set theme_bw
##'
NULL

.cleanup.ssh.tunnel <- function (...) {
  if (!is.null(getOption("aakmisc.tunnelpid",NULL))) {
    message("terminating ssh tunnel") #nocov
    stopTunnel() #nocov
  }
}

.onAttach <- function (...) {
  options(stringsAsFactors=FALSE)
  ggplot2::theme_set(ggplot2::theme_bw())
  reg.finalizer(.GlobalEnv,.cleanup.ssh.tunnel,onexit=TRUE)
}
