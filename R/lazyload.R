##' Functions for lazy-loading knitr caches.
##'
##' These functions are helpful for loading cached chunks into an interactive
##' session.
##'
##' Use \code{lazyload_cache_dir} to load a whole directory of cached objects.
##'
##' Use \code{lazyload_cache_labels} to load and explicit set of cached chunks.
##'
##' @name lazyload
##' @rdname lazyload
##' @aliases lazyload_cache_dir lazyload_cache_labels
##' @param path the path to the cache directory
##' @param labels character vector; chunk labels to load
##' @param envir the environment to load the objects into
##' @param ask if \code{TRUE}, interactively ask whether to load each database
##' discovered in \code{path}
##' @param verbose if \code{TRUE}, display the names of chunk labels being
##' loaded
##' @param full.names use the full name, i.e., include the path, for the chunk
##' label?  This argument is passed to \code{\link[base]{list.files}}.
##' @param filter optional function; passed to \code{\link[base]{lazyLoad}}.
##' When called on a character vector of object names, this function should
##' return a logical vector: objects for which this is \code{TRUE} will be
##' loaded.
##' @param \dots additional arguments passed to \code{\link[base]{list.files}}
##' @return Both functions return \code{NULL}, invisibly.
##' @author Peter DeWitt (https://github.com/dewittpe).
##' @keywords programming
NULL

##' @rdname lazyload
##' @export
lazyload_cache_dir <- function(path = "./cache", envir = parent.frame(),
  ask = FALSE, verbose = getOption("verbose",FALSE),
  full.names = TRUE, ...) {

  files <- do.call(list.files,
    list(path=path,pattern="\\.rdx$",full.names=full.names,...))
  files <- gsub("\\.rdx","",files)
  load_these <- rep(TRUE,length(files))

  if (ask) {
    for (i in seq_along(files)) {
      answer <- readline(prompt=paste("load database:",gsub("_[0-9a-f]{32}","",files[i]),"(y/n)"))
      load_these[i] <- answer %in% c("Yes","yes","Y","y")
    }
  }

  files <- files[load_these]

  if (!verbose) {
    sapply(files, lazyLoad, envir = envir)
  } else {
    sapply(
      files,
      function(x, envir) {
        message(paste("Lazyloading:",gsub("_[0-9a-f]{32}","",x)))
        lazyLoad(x,envir=envir)
      },
      envir=envir
    )
  }

  invisible(NULL)
}

##' @rdname lazyload
##' @export
lazyload_cache_labels <- function(labels, path = "./cache/", envir = parent.frame(),
  verbose = getOption("verbose",FALSE), filter,
  full.names = TRUE, ...) {

  files <- do.call(list.files,
    list(path=path,
      pattern=paste0("^(",paste(labels,collapse="|"),")_[0-9a-f]{32}\\.rdx$"),
      full.names=full.names,...))
  files <- gsub("\\.rdx$","",files)

  lfound <- sapply(lapply(labels,grepl,x=files),any)

  if (!all(lfound)) {
    files <- do.call(list.files,list(path=path,pattern="_[0-9a-f]{32}\\.rdx$",full.names=FALSE,...))
    files <- gsub("_[0-9a-f]{32}\\.rdx$","",files)
    message(paste0("label(s)\n",paste(paste0("  ",labels[!lfound]),collapse = "\n"),"\nnot found in path '",path,"\n\n",
      "Available labels:\n",paste(paste0("  ",files),collapse = "\n")))
    warning("Nothing loaded",call. = FALSE)
  } else {
    if (!verbose) {
      sapply(files,lazyLoad,envir=envir,filter=filter)
    } else {
      sapply(files,
        function(x,envir,filter) {
          message(paste("Lazyloading",x))
          lazyLoad(x,envir=envir,filter=filter)
        },
        envir=envir,
        filter=filter)
    }
  }
  invisible(NULL)
}
