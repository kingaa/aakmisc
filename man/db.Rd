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
getMLEs(host = getOption("aakmisc.dbhost"),
        dbname = getOption("aakmisc.dbname"),
        \dots)
recMLEs(mle, host = getOption("aakmisc.dbhost"),
        dbname = getOption("aakmisc.dbname"),
        \dots)
listScripts(host = getOption("aakmisc.dbhost"),
            dbname = getOption("aakmisc.dbname"),
            \dots)
recScript(files, host = getOption("aakmisc.dbhost"),
          dbname = getOption("aakmisc.dbname"),
          \dots)
dropScript(script, host = getOption("aakmisc.dbhost"),
           dbname = getOption("aakmisc.dbname"),
           \dots)
catScript(script, file = "", host = getOption("aakmisc.dbhost"),
          dbname = getOption("aakmisc.dbname"),
          \dots)
getQuery(statement,
         host = getOption("aakmisc.dbhost"),
         dbname = getOption("aakmisc.dbname"),
         \dots)
writeDBTable(name, value, overwrite = FALSE, append = FALSE,
             row.names = FALSE,
             host = getOption("aakmisc.dbhost"),
             dbname = getOption("aakmisc.dbname"), \dots)
startTunnel(port, remotehost = getOption("aakmisc.remotehost"), sleep = 5)
stopTunnel(info)
}
\arguments{
  \item{host}{
    Hostname on which to connect to the PostgreSQL server.
  }
  \item{dbname}{
    Name of PostgreSQL database.
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
  \item{port}{
    Local port to use for ssh tunnel.
    If unspecified, a random port number will be used.
  }
  \item{remotehost}{
    Hostname of PostgreSQL server.
    An ssh tunnel to this host will be created.
  }
  \item{sleep}{
    Time in seconds to sleep after initiating the ssh tunnel.
  }
  \item{info}{
    Information about the ssh tunnel returned by \code{startTunnel}.
  }
  \item{\dots}{
    Additional arguments will be passed to \code{\link[DBI]{dbConnect}}.
  }
}
\examples{
  \dontrun{
startTunnel() -> tun
listScripts(port=tun$port)
stopTunnel(tun)
  }
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
