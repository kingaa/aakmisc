##' Interface with databases
##'
##' Interface with project databases.
##'
##' @name db
##' @rdname db
##'
##' @param host Hostname on which to connect to the PostgreSQL server.
##' @param dbname Name of PostgreSQL database.
##' @param port Port on which to connect to PostgreSQL database.  If NULL, a
##' random port number will be used.
##' @param user Username to use in conneting to PostgreSQL database.  If NULL,
##' Sys.getenv("USER") will be used.
##' @param mle A data-frame of MLEs to be recorded.
##' @param files Files containing R scripts to be recorded.
##' @param script Name of script.
##' @param file File to which the script will be written.  See
##' \code{\link{cat}}.
##' @param statement SQL statement passed to \code{\link[DBI]{dbGetQuery}}.
##' @param name,value Name and contents of table to create.
##' @param overwrite,append,row.names See \code{\link[DBI]{dbWriteTable}}.
##' @param ... Additional arguments will be passed to \code{\link[DBI]{dbConnect}}.
##' @author Aaron A. King
##' @examples
##' \dontrun{
##' startTunnel()
##' listScripts()
##' stopTunnel()
##' }
##' @import methods
##' @importFrom DBI dbDisconnect dbGetQuery dbUnloadDriver dbDriver
##' @importFrom RPostgreSQL dbConnect dbWriteTable
##' @importFrom dplyr bind_rows
NULL

##' @rdname db
##' @export
writeDBTable <- function (name, value,
  overwrite = FALSE, append = FALSE, row.names = FALSE,
  host = getOption("aakmisc.dbhost","localhost"),
  dbname = getOption("aakmisc.dbname",NULL),
  port=getOption("aakmisc.port",5432),
  user=getOption("aakmisc.user",NULL),
  ...) {
  if (missing(name) || missing(value))
    stop("must specify ",sQuote("name")," and ",sQuote("value"))
  if (is.null(host))
    stop("must specify ",sQuote("host"))
  if (is.null(dbname))
    stop("must specify ",sQuote("dbname"))
  if (is.null(user))
    user <- Sys.getenv("USER")

  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,host=host,dbname=dbname,port=port,user=user,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  dbWriteTable(
    db,
    name=name,
    value=value,
    overwrite=overwrite,
    append=append,
    row.names=row.names
  )
}

##' @rdname db
##' @export
getQuery <- function (statement,
  host = getOption("aakmisc.dbhost","localhost"),
  dbname = getOption("aakmisc.dbname",NULL),
  port=getOption("aakmisc.port",5432),
  user=getOption("aakmisc.user",NULL),
  ...) {
  if (is.null(host))
    stop("must specify ",sQuote("host"))
  if (is.null(dbname))
    stop("must specify ",sQuote("dbname"))
  if (is.null(user))
    user <- Sys.getenv("USER")

  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,host=host,dbname=dbname,port=port,user=user,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  dbGetQuery(db,statement=statement)
}

##' @rdname db
##' @export
getMLEs <- function (host = getOption("aakmisc.dbhost","localhost"),
  dbname = getOption("aakmisc.dbname",NULL),
  port=getOption("aakmisc.port",5432),
  user=getOption("aakmisc.user",NULL),
  ...) {
  if (is.null(host))
    stop("must specify ",sQuote("host"))
  if (is.null(dbname))
    stop("must specify ",sQuote("dbname"))
  if (is.null(user))
    user <- Sys.getenv("USER")

  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,host=host,dbname=dbname,port=port,user=user,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  dbGetQuery(db,"select * from mle")
}

##' @rdname db
##' @export
recMLEs <- function (mle,
  host = getOption("aakmisc.dbhost","localhost"),
  dbname = getOption("aakmisc.dbname",NULL),
  port=getOption("aakmisc.port",5432),
  user=getOption("aakmisc.user",NULL),
  ...) {
  if (is.null(host))
    stop("must specify ",sQuote("host"))
  if (is.null(dbname))
    stop("must specify ",sQuote("dbname"))
  if (is.null(user))
    user <- Sys.getenv("USER")

  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,host=host,dbname=dbname,port=port,user=user,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  x <- dbGetQuery(db,"select * from mle")

  if ((!setequal(names(mle),names(x))))
    stop("field names in ",sQuote("mle"),
      " don't match those in database")
  mle <- mle[names(x)]

  dbWriteTable(db,"mle",mle,append=TRUE,row.names=FALSE)
}

##' @rdname db
##' @export
recScript <- function (files,
  host = getOption("aakmisc.dbhost","localhost"),
  dbname = getOption("aakmisc.dbname",NULL),
  port=getOption("aakmisc.port",5432),
  user=getOption("aakmisc.user",NULL),
  ...) {
  if (is.null(host))
    stop("must specify ",sQuote("host"))
  if (is.null(dbname))
    stop("must specify ",sQuote("dbname"))
  if (is.null(user))
    user <- Sys.getenv("USER")

  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,host=host,dbname=dbname,port=port,user=user,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  files |>
    lapply(
      \(f) {
        data.frame(
          script=gsub("\\.R$","",f),
          code=readChar(f,nchars=file.info(f)$size)
        )
      }
    ) |>
    bind_rows() -> new

  dbGetQuery(db,"select script from scripts") -> old
  overlap <- new$script%in%old$script
  if (any(overlap))
    stop("script(s) ",
      paste(sQuote(new$script[overlap]),collapse=","),
      " previously defined")

  dbWriteTable(db,"scripts",new,row.names=FALSE,append=TRUE)

  dbGetQuery(db,"select script from scripts") -> def

  cat("recorded scripts:",def$script,sep="\n")
}

##' @rdname db
##' @export
dropScript <- function (script,
  host = getOption("aakmisc.dbhost","localhost"),
  dbname = getOption("aakmisc.dbname",NULL),
  port=getOption("aakmisc.port",5432),
  user=getOption("aakmisc.user",NULL),
  ...) {
  if (is.null(host))
    stop("must specify ",sQuote("host"))
  if (is.null(dbname))
    stop("must specify ",sQuote("dbname"))
  if (is.null(user))
    user <- Sys.getenv("USER")

  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,host=host,dbname=dbname,port=port,user=user,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  sql <- sprintf("delete from scripts where script='%s'",script)

  dbGetQuery(db,sql)

  dbGetQuery(db,"select script from scripts") -> def
  cat("recorded scripts:",def$script,sep="\n")
}

##' @rdname db
##' @export
listScripts <- function (host = getOption("aakmisc.dbhost","localhost"),
  dbname = getOption("aakmisc.dbname",NULL),
  port=getOption("aakmisc.port",5432),
  user=getOption("aakmisc.user",NULL),
  ...) {
  if (is.null(host))
    stop("must specify ",sQuote("host"))
  if (is.null(dbname))
    stop("must specify ",sQuote("dbname"))
  if (is.null(user))
    user <- Sys.getenv("USER")

  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,host=host,dbname=dbname,port=port,user=user,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  dbGetQuery(db,"select script from scripts") -> def
  cat("recorded scripts:",def$script,sep="\n")
}

##' @rdname db
##' @export
catScript <- function (script, file = "",
  host = getOption("aakmisc.dbhost","localhost"),
  dbname = getOption("aakmisc.dbname",NULL),
  port=getOption("aakmisc.port",5432),
  user=getOption("aakmisc.user",NULL),
  ...) {
  if (is.null(host))
    stop("must specify ",sQuote("host"))
  if (is.null(dbname))
    stop("must specify ",sQuote("dbname"))
  if (is.null(user))
    user <- Sys.getenv("USER")

  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,host=host,dbname=dbname,port=port,user=user,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  sql <- sprintf("select code from scripts where script='%s'",script)

  dbGetQuery(db,sql) -> res
  cat(res$code,file=file)
}
