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

  plyr::ldply(
    files,
    function (f) {
      data.frame(
        script=gsub("\\.R$","",f),
        code=readChar(f,nchars=file.info(f)$size)
      )
    }
  ) -> new

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
