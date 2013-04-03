\name{dbtools}
\alias{getMLEs}
\alias{recMLEs}
\alias{recScript}
\alias{dropScript}
\alias{catScript}
\alias{listScripts}
\alias{startTunnel}
\alias{stopTunnel}
\title{Interface with databases}
\description{
  Interface with project databases.
}
\usage{
getMLEs(host = "localhost", \dots)
recMLEs(mle, host = "localhost", \dots)
listScripts(host = "localhost", \dots)
recScript(files, host = "localhost", \dots)
dropScript(script, host = "localhost", \dots)
catScript(script, file = "", host = "localhost", \dots)
startTunnel(port, remotehost = "kinglab.eeb.lsa.umich.edu", sleep = 5)
stopTunnel(pidfile)
}
\arguments{
  \item{host}{
    Hostname on which to connect to the PostgreSQL server.
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
  \item{port}{
    Local port to use for ssh tunnel.
    If unspecified, a random port number will be used.
  }
  \item{remotehost}{
    Hostname of PostgreSQL server.
  }
  \item{sleep}{
    Time in seconds to sleep after initiating the ssh tunnel.
  }
  \item{pidfile}{
    Name of temporary file to hold PID of ssh process.
    By default, \code{\link{tempfile}} is used.
  }
  \item{\dots}{
    Additional arguments will be passed to \code{\link[DBI]{dbConnect}}.
  }
}
\examples{
  \dontrun{
startTunnel() -> tun
listScripts(port=tun$port)
stopTunnel(tun$pidfile)
  }
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
