getMLEs <- function (...) {
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  dbGetQuery(db,"select * from mle")
}

recMLEs <- function (mle, ...) {
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  x <- dbGetQuery(db,"select * from mle")

  if ((!setequal(names(mle),names(x))))
    stop("field names in ",sQuote("mle"),
         " don't match those in database")
  mle <- mle[names(x)]

  dbWriteTable(db,"mle",mle,append=TRUE,row.names=FALSE)
}

recScript <- function (files, ...) {
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  ldply(
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

dropScript <- function (script, ...) {
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  sql <- sprintf("delete from scripts where script='%s'",script)

  dbGetQuery(db,sql)

  dbGetQuery(db,"select script from scripts") -> def
  cat("recorded scripts:",def$script,sep="\n")
}

listScripts <- function (...) {
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  dbGetQuery(db,"select script from scripts") -> def
  cat("recorded scripts:",def$script,sep="\n")
}

catScript <- function (script, file = "", ...) {
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv,...)
  on.exit(dbDisconnect(db))
  on.exit(dbUnloadDriver(drv),add=TRUE)

  sql <- sprintf("select code from scripts where script='%s'",script)

  dbGetQuery(db,sql) -> res
  cat(res$code,file=file)
}
