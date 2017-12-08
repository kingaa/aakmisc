\name{NEWS}
\title{News for Package 'aakmisc'}
\section{Changes in aakmisc version 0.26}{
  \itemize{
    \item The package no longer automatically loads \pkg{ggplot2}, \pkg{scales}, \pkg{grid}, \pkg{plyr}, \pkg{reshape2}, \pkg{magrittr}, and \pkg{readr}.
    \item \code{scinot} is now suitable for use in \code{ggplot::scale_X_Y}.
    \item The new function \code{trnc} allows truncation to the plot window when used in \code{ggplot::scale_X_Y}.
  }
}
\section{Changes in aakmisc version 0.25}{
  \itemize{
    \item The license under which the software is provided has been upgraded to GPL version 3.
    \item Package \pkg{readr} is a new dependency and \pkg{scales} is explicitly imported.
  }
}
\section{Changes in aakmisc version 0.24}{
  \itemize{
    \item The license under which the software is provided has been upgraded to GPL version 3.
  }
}
\section{Changes in aakmisc version 0.23}{
  \itemize{
    \item \code{startTunnel} now takes a \code{user} option.
  }
}
\section{Changes in aakmisc version 0.21}{
  \itemize{
    \item Codes for lazy-loading \pkg{knitr} caches have been added.
    These useful functions are shamelessly cadged from Peter DeWitt's \pkg{qwrap2} package.
  }
}
\section{Changes in aakmisc version 0.16}{
  \itemize{
    \item \code{bake} and \code{stew} have been moved to \pkg{pomp}.
  }
}
\section{Changes in aakmisc version 0.15}{
  \itemize{
    \item New \code{stew} function (similar to \code{bake}).
  }
}
\section{Changes in aakmisc version 0.12}{
  \itemize{
    \item On attachment, the default \pkg{ggplot2} theme is \code{theme_bw()}.
  }
}
\section{Changes in aakmisc version 0.11}{
  \itemize{
    \item New \code{bake} function.
  }
}
\section{Changes in aakmisc version 0.10}{
  \itemize{
    \item A new \code{scinot} function for formatting numbers in scientific notation.
  }
}
\section{Changes in aakmisc version 0.9}{
  \itemize{
    \item There is a new \code{plotMatrix} method for generating scatterplot matrices and density plots from data-frames.
  }
}
\section{Changes in aakmisc version 0.8}{
  \itemize{
    \item will attempt to clean up ssh tunnel processes on exit from R
  }
}
