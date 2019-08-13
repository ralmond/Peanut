\name{BuildTable}
\alias{BuildTable}
\alias{BuildAllTables}
\title{Builds the conditional probability table for a Pnode}
\description{

  The function \code{BuildTable} builds the conditional probability
  table for a \code{\link{Pnode}} object, and sets the prior weight for
  the node using the current values of parameters.  It sets these in the
  Bayesian network object as appropriate for the implementation.  The
  expression \code{BuildAllTables(net)} builds tables for all of the
  nodes in \code{\link{PnetPnodes}(net)}. 

}
\usage{
BuildTable(node)
BuildAllTables(net, debug=FALSE)
}
\arguments{
  \item{node}{A \code{\link{Pnode}} object whose table is to be built.}
  \item{net}{A \code{\link{Pnet}} object for whom the tables are needed
    to be built.}
  \item{debug}{A logical scalar.  If true then
    \code{\link[base]{recover}} is called after an error, so that the
    node in question can be inspected.}

}
\details{

  The fields of the \code{\link{Pnode}} object correspond to the
  arguments of the \code{\link[CPTtools]{calcDPCTable}} function.  The
  output conditional probability table is then set in the node object in
  an implementation dependent way.  Similarly, the current value of
  \code{\link{GetPriorWeight}} is used to set the weight that the prior
  table will be given in the EM algorithm.

}
\section{Logging and Debug Mode}{

  As of version 0.6-2, the meaning of the \code{debug} argument is
  changed.  In the new version, the
  \code{\link[futile.logger]{flog.logger}} mechanism is used for
  progress reports, and error reporting.  In particular, setting
  \code{\link[futile.logger]{flog.threshold}(DEBUG)} (or \code{TRACE})
  will cause progress reports to be sent to the logging output.

  The \code{debug} argument has been repurposed.  It now call
  \code{\link{recover}} when the error occurs, so that the problem can
  be debugged.
  
}
\value{

  The \code{node} or \code{net} argument is returned invisibly.
  As a side effect the conditional probability table and prior weight of
  \code{node} (or a collection of nodes) is modified.

}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\note{

  The function \code{BuildTable} is an abstract generic function, and it
  needs a specific implementation.  See the
  \code{\link[PNetica]{PNetica-package}} for an example.

}

\seealso{
  \code{\link{Pnode}}, \code{\link{PnodeQ}},
  \code{\link{PnodePriorWeight}}, \code{\link{PnodeRules}}, 
  \code{\link{PnodeLink}}, \code{\link{PnodeLnAlphas}}, 
  \code{\link{PnodeAlphas}}, \code{\link{PnodeBetas}}, 
  \code{\link{PnodeLinkScale}},\code{\link{GetPriorWeight}},
  \code{\link[CPTtools]{calcDPCTable}}
}
\examples{

\dontrun{

## This is the implementation of BuildTable in Netica.  The [<- and
## NodeExperience functions are part of the RNetica implementation.

BuildTable.NeticaNode <- function (node) {
  node[] <- calcDPCFrame(ParentStates(node),NodeStates(node),
                          PnodeLnAlphas(node), PnodeBetas(node),
                          PnodeRules(node),PnodeLink(node),
                          PnodeLinkScale(node),PnodeQ(node),
                          PnodeParentTvals(node))
  NodeExperience(node) <- GetPriorWeight(node)
  invisible(node)
}

## This is a simplified implementation of BuildAllTables
## (The full implementation adds logging and error handling.)
BuildAllTables <- function (net) {
  lapply(PnetPnodes(net),BuildTable)
  invisible(net)
}


}}
\keyword{distribution}

