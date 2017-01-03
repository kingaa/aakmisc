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
