##' Functions for generating and working with truly random integers.
##'
##' Functions for generating and working with truly random seeds.
##'
##' @name random
##' @rdname random
##' @aliases random.org urandom rngControl rngSeeds
##' @include aaa.R
##' @importFrom curl curl
##'
##' @param n Number of integers required.
##' @param rnd random.org parameter
##' @param expr Expression to be evaluated with RNG control.
##' @param seed RNG seed.
##'
##' @return integers suitable for use as RNG seeds
##'
##' @author Aaron A. King
##' @references \url{https://www.random.org}
##' @examples
##'
##' random.org(n=5)
##' seed <- urandom(n=1)
##' seeds <- rngSeeds(5,seed=seed)
##' set.seed(seed)
##' runif(5)
##' rngControl(runif(5),seed=seed[1])
##' rngControl(runif(5),seed=seed[1])
##' runif(5)
##' set.seed(seed)
##' runif(5)
##' runif(5)
##'
NULL

##' @rdname random
##' @details
##' \code{random.org} gets seeds from \url{random.org}.
##'
##' @export
random.org <- function (n = 10, rnd = "new") {
  template <- "https://www.random.org/integers/?num=%d&min=%d&max=%d&col=%d&base=%d&format=plain&rnd=%s"
  base <- 2
  min <- 0
  max <- 1
  col <- 31
  query <- sprintf(template,n*col,min,max,col,base,rnd)
  con <- curl::curl(query)
  dat <- scan(con,what=character(0))
  close(con)
  dim(dat) <- c(n,col)
  strtoi(apply(dat,1,paste,collapse=""),base=base)
}

## Local random-device interface
## Only available on *nix platforms
if (.Platform$OS.type=="unix") {
  urandom_internal <- function (n = 10) {
    as.integer(abs(readBin("/dev/urandom",what=integer(0),n=n)))
  }
} else {
  urandom_internal <- function (n = 10) {
    rngSeeds(n=n)
  }
}

##' @rdname random
##' @details
##' \code{urandom} gets seeds locally from \file{/dev/urandom} on *nix systems.
##'
##' @export
urandom <- urandom_internal

##' @rdname random
##' @details
##' \code{rngControl} is a function to control RNG for the evaluation of an expression.
##' @importFrom stats runif
##' @export
rngControl <- function (expr, seed = NULL) {
  expr <- substitute(expr)
  if (!is.null(seed)) {
    if (!exists(".Random.seed",envir=.GlobalEnv)) runif(1) #nocov
    save.seed <- get('.Random.seed',envir=.GlobalEnv)
    set.seed(seed)
  }
  res <- eval(expr,envir=parent.frame())
  if (!is.null(seed)) {
    assign('.Random.seed',save.seed,envir=.GlobalEnv)
  }
  res
}

##' @rdname random
##' @details
##' \code{rngSeeds} generates RNG seeds using \code{\link{sample.int}}.
##' It is included for situations when neither \code{\link{random.org}} nor \code{\link{urandom}} is available.
##' @export
rngSeeds <- function (n, seed = NULL) {
    rngControl(
      sample.int(.Machine$integer.max,n),
      seed=seed
    )
}
