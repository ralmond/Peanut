\name{PnodeStats}
\alias{PnodeEAP}
\alias{PnodeSD}
\alias{PnodeMargin}
\alias{PnodeMedian}
\alias{PnodeMode}
\title{Pnode Marginal Statistics}
\description{

  These functions compute statistics of the marginal distribution of the
  corresponding node.  These are designed to be used with
  \code{\linkS4class{Statistic}} objects.

}
\usage{
PnodeMargin(node, net =NULL)
PnodeEAP(node, net = NULL)
PnodeSD(node, net = NULL)
PnodeMedian(node, net = NULL)
PnodeMode(node, net = NULL)
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object representing the network.  May be omitted if not needed.
}
  \item{node}{A \code{\link{Pnode}} object describing the node whose
    statistics are desired.  Or a chacter string giving the name of a 
    node in a network.
  }
}
\details{

  These are the functions that implement the statistics.  These are
  typically called by \code{\link{calcStat}} which finds the nodes
  corresponding to the named nodes in the statistics.  The \code{net}   
  argument is only used if the supplied statistic is a name.

}
\value{

  \code{PnodeMargin} returns a vector corresponding to the states of
  \code{node} giving the marginal probabilities of the states.

  \code{PnodeEAP} returns a numeric scalar giving the expected a
  posteriori value (mean) of the \code{\link{PnodeStateValues}} of the
  node.  \code{PnodeSD} gives the standard deviation.

  \code{PnodeMedian} assumes the states are ordered, and returns the
  state at the 50th percentile.  This is a factor (character) value.

  \code{PnodeMode} returns the most likely state as a factor (character)
  value. 

  
}
\details{
  
  In general, this method dispatches on the type of the node, so uses
  implementation specific code.  
  
  The exception is if the \code{node} argument is a character value.
  In this case, the \code{net} must be supplied and the function 
  \code{\link{PnetFindNode}} is called to find the actual node object.

}
\note{
  
  The previous implementation had the order of the \code{net} and 
  \code{node} arguments reversed.  \em{This breaks compatability with 
  code written against 0.9 and earlier versions.}  However, as I was 
  working with the package, the need to be able to call 
  `PnodeMargin(node)` to look at the current value of a node seemed too 
  important.
  
  Note that the function \code{\link{PnodeNet}} should point back to
  the network, so the \code{net} argument is not needed, except when 
  the node name is given instead of the node object.
}
\references{
  Almond, R.G., Mislevy, R.J. Steinberg, L.S., Yan, D. and Willamson, D.
  M. (2015). \emph{{Bayesian} Networks in Educational Assessment}.
  Springer.  Chapter 13.
}
\author{Russell Almond}
\seealso{
  Statistics Class:
  \code{\linkS4class{Statistic}}

  Constructor function:
  \code{\link{Statistic}}

  \code{\link{calcStat}}

  These statistics will likely produce errors unless
  \code{\link{PnetCompile}} has been run first.
}
\examples{
\dontrun{

library(PNetica) ## Need a specific implementation
sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(system.file("testnets", "IRT10.2PL.base.dne",
                                        package="PNetica"),
                           session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- PnetFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  
}


BuildAllTables(irt10.base)
PnetCompile(irt10.base) ## Netica requirement

theta <- PnetFindNode(irt10.base)

PnodeMargin(theta)
PnodeMargin("theta",irt10.base)
PnodeEAP(theta)
PnodeSD(theta)
PnodeMedian(theta)
PnodeMode(theta)


DeleteNetwork(irt10.base)
stopSession(sess)
}
}
\keyword{ graphs }
