\name{flog.try}
\alias{flog.try}
\title{Trys to execute an expression with errors logged.}
\description{

  This is a version of \code{\link[base]{try}} which logs errors using
  the \code{\link[futile.logger]{flog.logger}} mechanism.  

}
\usage{
flog.try(expr, context = deparse(substitute(expr)), loggername = flog.namespace(), tracelevel = c("WARN", "ERROR", "FATAL"))
}
\arguments{
  \item{expr}{An R expression to be executed. }
  \item{context}{A character string defining what was operation is being
    performed for use in the log message.}
  \item{loggername}{A package name defining the logger to be used.  See
    \code{\link[futile.logger]{flog.namespace}}.} 
  \item{tracelevel}{A character vector.  In response to signals of the
    listed types, a stack trace will be sent to the log file.}
}
\details{

  This function behaves like the \code{\link[base]{try}} function,
  attempt to execute \code{expr}.  If successful, the result is
  returned, if not an object of class \code{try-error} is returned, so
  that the calling function can figure out how to proceed.

  It has two important difference from \code{try}.  The first is the
  \code{context} argument which provides information about what was
  happening when the error was generated.  In a large problem, this can
  provide vital debugging information, like the issue was with a
  particular node in a graph.

  The second is that the error message and the stack trace are posted to
  the logging stream using the \code{\link[futile.logger]{flog.logger}}
  function.  This makes the code easier to use in server processes.
  
}
\value{

  Either the result of running \code{expr} or an object of class
  \code{try-error}.  

}
\author{Russell Almond}
\note{

  I should move this to the RGAutils package as it is generally useful.
  
}
\seealso{
  \code{\link[base]{try}}, \code{\link[futile.logger]{flog.logger}}

  The function \code{\link{maxAllTableParams}} shows an example of this
  in use.
}
\examples{

\dontrun{
maxAllTableParams <- function (net, Mstepit=5,
                                    tol=sqrt(.Machine$double.eps),
                                    debug=FALSE) {
  Errs <- list()
  netnm <- PnetName(net)
  lapply(PnetPnodes(net),
         function (nd) {
           ndnm <- PnodeName(nd)
           flog.debug("Updating params for node %s in net %s.",ndnm,netnm)
           out <- flog.try(maxCPTParam(nd,Mstepit,tol),
                           context=sprintf("Updating params for node %s in net %s.",
                                           ndnm, netnm))
           if (is(out,'try-error')) {
             Errs <- c(Errs,out)
             if (debug) recover()
           }
         })
  if (length(Errs) >0L)
    stop("Errors encountered while updating parameters for ",netnm)
  invisible(net)
}
}
}
\keyword{ language }
\keyword{ util }
