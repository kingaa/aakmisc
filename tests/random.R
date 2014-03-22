library(aakmisc)

if (.Platform$OS.type == "unix") {
  seed <- urandom(n=1)
} else {
  seed <- 7968868L
}

set.seed(seed)
rngSeeds(5,seed=seed)

x1 <- runif(5)
y1 <- rngControl(runif(5),seed=34934L)
y2 <- rngControl(runif(5),seed=34934L)
z1 <- runif(5)

set.seed(seed)
x2 <- runif(5)
z2 <- runif(5)

stopifnot(identical(x1,x2))
stopifnot(identical(y1,y2))
stopifnot(identical(z1,z2))
