\name{dbtools}
\alias{getMLEs}
\alias{recMLEs}
\alias{recScript}
\alias{dropScript}
\alias{catScript}
\alias{listScripts}
\title{Interface with databases}
\description{
  Interface with project databases.
}
\usage{
getMLEs(\dots)
recMLEs(mle, \dots)
listScripts(\dots)
recScript(files, \dots)
dropScript(script, \dots)
catScript(script, file = "", \dots)
}
\arguments{
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
  \item{\dots}{
    Additional arguments will be passed to \code{\link[DBI]{dbConnect}}.
  }
}
\author{Aaron A. King \email{kingaa at umich dot edu}}
