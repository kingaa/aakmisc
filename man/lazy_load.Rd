\name{Lazy loading of knitr caches}
\alias{lazyload_cache_dir}
\alias{lazyload_cache_labels}
\title{
  Functions for lazy-loading knitr caches.
}
\description{
  These functions are helpful for loading cached chunks into an interactive \R session.

  Use \code{lazyload_cache_dir} to load a whole directory of cached objects.

  Use \code{lazyload_cache_labels} to load and explicit set of cached chunks.
}
\usage{
lazyload_cache_dir(path = "./cache", envir = parent.frame(), ask = FALSE,
                   verbose = getOption("verbose",FALSE), full.names = TRUE, \dots)
lazyload_cache_labels(labels, path = "./cache/", envir = parent.frame(),
                   verbose = getOption("verbose",FALSE), filter,
                   full.names = TRUE, \dots)
}
\arguments{
  \item{path}{the path to the cache directory}
  \item{labels}{character vector; chunk labels to load}
  \item{envir}{the environment to load the objects into}
  \item{ask}{if \code{TRUE}, interactively ask whether to load each database discovered in \code{path}}
  \item{verbose}{if \code{TRUE}, display the names of chunk labels being loaded}
  \item{full.names}{
    use the full name, i.e., include the path, for the chunk label?
    This argument is passed to \code{\link[base]{list.files}}.
  }
  \item{filter}{
    optional function; passed to \code{\link[base]{lazyLoad}}.
    When called on a character vector of object names, this function should return a logical vector: objects for which this is \code{TRUE} will be loaded.
  }
  \item{\dots}{additional arguments passed to \code{\link[base]{list.files}}}
}
\value{
  Both functions return \code{NULL}, invisibly.
}
\author{
  Peter DeWitt (https://github.com/dewittpe).
}
\keyword{programming}
