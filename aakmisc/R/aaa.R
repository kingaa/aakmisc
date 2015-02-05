.cleanup.ssh.tunnel <- function (...) {
  if (!is.null(getOption("aakmisc.tunnelpid",NULL))) {
    message("terminating ssh tunnel")
    stopTunnel()
  }
}

.onAttach <- function (...) {
  packageStartupMessage("setting ",sQuote("stringsAsFactors=FALSE"))
  options(stringsAsFactors=FALSE)
  ggplot2::theme_set(ggplot2::theme_bw())
  reg.finalizer(.GlobalEnv,.cleanup.ssh.tunnel,onexit=TRUE)
}
