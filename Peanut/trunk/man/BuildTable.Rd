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
  \item{debug}{A logical scalar.  If true the names of the nodes are
    printed as each one is built to help determine where errors are
    occuring.}
}
\details{

  The fields of the \code{\link{Pnode}} object correspond to the
  arguments of the \code{\link[CPTtools]{calcDPCTable}} function.  The
  output conditional probability table is then set in the node object in
  an implementation dependent way.  Similarly, the current value of
  \code{\link{GetPriorWeight}} is used to set the weight that the prior
  table will be given in the EM algorithm.

  
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

## This is the implementation of BuildTable in Netica.  The [[<- and
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

## This is the implementation of BuildAllTables
BuildAllTables <- function (net) {
  lapply(PnetPnodes(net),BuildTable)
  invisible(net)
}


}}
\keyword{distribution}

