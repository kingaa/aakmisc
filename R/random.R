## download random seeds from random.org

random.org <- function (n = 10, rnd = "new") {
  template <- "http://www.random.org/integers/?num=%d&min=%d&max=%d&col=%d&base=%d&format=plain&rnd=%s"
  base <- 2
  min <- 0
  max <- 1
  col <- 31
  query <- sprintf(template,n*col,min,max,col,base,rnd)
  con <- url(query)
  dat <- scan(con,what=character(0))
  close(con)
  dim(dat) <- c(n,col)
  strtoi(apply(dat,1,paste,collapse=""),base=base)
}

## get seeds locally (on *nix)
urandom <- function (n = 10) {
  as.integer(abs(readBin("/dev/urandom",what=integer(0),n=n)))
}
