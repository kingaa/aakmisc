\name{dbtools}
\alias{getMLEs}
\alias{recMLEs}
\alias{recScript}
\alias{dropScript}
\alias{catScript}
\alias{listScripts}
\alias{getQuery}
\alias{writeDBTable}
\alias{startTunnel}
\alias{stopTunnel}
\title{Interface with databases}
\description{
  Interface with project databases.
}
\usage{
getMLEs(host = getOption("aakmisc.dbhost","localhost"),
        dbname = getOption("aakmisc.dbname",NULL),
        port = getOption("aakmisc.port",5432),
        \dots)
recMLEs(mle, host = getOption("aakmisc.dbhost","localhost"),
        dbname = getOption("aakmisc.dbname",NULL),
        port = getOption("aakmisc.port",5432),
        \dots)
listScripts(host = getOption("aakmisc.dbhost","localhost"),
            dbname = getOption("aakmisc.dbname",NULL),
            port = getOption("aakmisc.port",5432),
            \dots)
recScript(files, host = getOption("aakmisc.dbhost","localhost"),
          dbname = getOption("aakmisc.dbname",NULL),
          port = getOption("aakmisc.port",5432),
          \dots)
dropScript(script, host = getOption("aakmisc.dbhost","localhost"),
           dbname = getOption("aakmisc.dbname",NULL),
           port = getOption("aakmisc.port",5432),
           \dots)
catScript(script, file = "", host = getOption("aakmisc.dbhost","localhost"),
          dbname = getOption("aakmisc.dbname",NULL),
          port = getOption("aakmisc.port",5432),
          \dots)
getQuery(statement,
         host = getOption("aakmisc.dbhost","localhost"),
         dbname = getOption("aakmisc.dbname",NULL),
         port = getOption("aakmisc.port",5432),
         \dots)
writeDBTable(name, value, overwrite = FALSE, append = FALSE,
             row.names = FALSE,
             host = getOption("aakmisc.dbhost","localhost"),
             dbname = getOption("aakmisc.dbname",NULL),
             port = getOption("aakmisc.port",5432),
             \dots)
startTunnel(port = NULL,
            remotehost = getOption("aakmisc.remotehost",
                                   "kinglab.eeb.lsa.umich.edu"),
            sleep = 5)
stopTunnel(..., pid = getOption("aakmisc.tunnelpid",NULL))
}
\arguments{
  \item{host}{
    Hostname on which to connect to the PostgreSQL server.
  }
  \item{dbname}{
    Name of PostgreSQL database.
  }
  \item{port}{
    Port on which to connect to PostgreSQL database.
    If NULL, a random port number will be used.
  }
  \item{mle}{
    A data-frame of MLEs to be recorded.
  }
  \item{files}{
    Files containing R scripts to be recorded.
  }
  \item{script}{
    Name of script.
  }
  \item{file}{
    File to which the script will be written.
    See \code{\link{cat}}.
  }
  \item{statement}{
    SQL statement passed to \code{\link[DBI]{dbGetQuery}}.
  }
  \item{name, value}{
    Name and contents of table to create.
  }
  \item{overwrite, append, row.names}{
    See \code{\link[DBI]{dbWriteTable}}.
  }
  \item{remotehost}{
    Hostname of PostgreSQL server.
    An ssh tunnel to this host will be created.
  }
  \item{pid}{
    ID of ssh tunnel process.
    Set automatically by \code{startTunnel}.
  }
  \item{sleep}{
    Time in seconds to sleep after initiating the ssh tunnel.
  }
  \item{\dots}{
    Additional arguments will be passed to \code{\link[DBI]{dbConnect}}.
  }
}
\examples{
  \dontrun{
startTunnel()
listScripts()
stopTunnel()
  }
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
