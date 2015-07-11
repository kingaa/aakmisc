## download random seeds from random.org

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

## get seeds locally (on *nix)
urandom <- function (n = 10) {
  as.integer(abs(readBin("/dev/urandom",what=integer(0),n=n)))
}

## function to control RNG for a single evaluation
rngControl <- function (expr, seed = NULL) {
  expr <- substitute(expr)
  if (!is.null(seed)) {
    if (!exists(".Random.seed",envir=.GlobalEnv)) runif(1)
    save.seed <- get('.Random.seed',envir=.GlobalEnv)
    set.seed(seed)
  }
  res <- eval(expr,envir=parent.frame())
  if (!is.null(seed)) {
    assign('.Random.seed',save.seed,envir=.GlobalEnv)
  }
  res
}

rngSeeds <- function (n, seed = NULL) {
  rngControl(
             as.integer(floor(runif(n=n,min=1,max=2^31))),
             seed=seed
             )
}
