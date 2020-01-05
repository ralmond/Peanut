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
PnodeMargin(net, node)
PnodeEAP(net, node)
PnodeSD(net, node)
PnodeMedian(net, node)
PnodeMode(net, node)
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object representing the network.
}
  \item{node}{A \code{\link{Pnode}} object describing the node whose
    statistics are desired.
  }
}
\details{

  These are the functions that implement the statistics.  These are
  typically called by \code{\link{calcStat}} which finds the nodes
  corresponding to the named nodes in the statistics.  Both the net and
  node are passes as arguments as this may be needed in some
  implementations. 

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

irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne",
                           sep=.Platform$file.sep),session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- PnetFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  
}
## Make some statistics
marginTheta <- Statistic("PnodeMargin","theta","Pr(theta)")
meanTheta <- Statistic("PnodeEAP","theta","EAP(theta)")
sdTheta <- Statistic("PnodeSD","theta","SD(theta)")
medianTheta <- Statistic("PnodeMedian","theta","Median(theta)")
modeTheta <- Statistic("PnodeMedian","theta","Mode(theta)")


BuildAllTables(irt10.base)
PnetCompile(irt10.base) ## Netica requirement

calcStat(marginTheta,irt10.base)
calcStat(meanTheta,irt10.base)
calcStat(sdTheta,irt10.base)
calcStat(medianTheta,irt10.base)
calcStat(modeTheta,irt10.base)

DeleteNetwork(irt10.base)
stopSession(sess)
}
}
\keyword{ graphs }
