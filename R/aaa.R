.onAttach <- function (...) {
  packageStartupMessage("setting ",sQuote("stringsAsFactors=FALSE"))
  options(stringsAsFactors=FALSE)
}
