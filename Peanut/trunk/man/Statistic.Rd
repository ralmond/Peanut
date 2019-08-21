\name{Statistic}
\alias{Statistic}
\alias{calcStat}
\title{Key functions for the Statistics class}
\description{

  A \code{\linkS4class{Statistic}} is a functional that when applied to
  a Bayesian network returns a value.  Usually, the statistic is a
  function of the distribution of a single node, but it could also be a
  function of several nodes.  Statistic objets have a
  \code{\link{calcStat}} method, which when applied to a network,
  produces the value.  Lists of statistics are often maintained by Bayes
  net engines to report values at designated times (e.g., after new
  evidence arrives).  The \code{Statistic} function is the constructor
  or \code{Statistic} objects.

}
\usage{
Statistic(fun, node, name = sprintf("\%s(\%s)", fun, node), ...)
calcStat(stat, net)
}
\arguments{
  \item{fun}{Object of class \code{"character"} giving a
      function to be applied to the nodes. The function should have
      \code{signature(net="Pnet", node)}, where \code{node} could be
      either a \code{\link{Pnode}} or a list of Pnodes (See details).}
  \item{node}{Object of class \code{"character"} giving the
      name(s) of the node(s) that are referenced by the statistic.  Note
      that these are not the actual node objects, as the network could
      be different at each call.}
  \item{name}{Object of class \code{"character"} giving a
      function to be applied to the nodes. The function should have
      \code{signature(net="Pnet", node)}, where \code{node} could be
      either a \code{\link{Pnode}} or a list of Pnodes.}
  \item{\dots}{Other optional arguments for later extension.}
  \item{stat}{An object of class \code{\linkS4class{Statisitic}} which
    will be applied to the \code{net}}
  \item{net}{A \code{Pnet} to which the statistic will be applied.}
}
\details{

  The \code{\linkS4class{Statistic}} class represents a functional which
  can be applied to a Bayes net (a distribution, \code{\link{Pnet}}),
  which returns a value of interest.  Usually the functional is a
  function of the marginal or joint distribution of a number of nodes,
  \code{\link{Pnode}}.  Some connonical examples are the expected value
  and the median of the marginal distribution for a node.

  Because the functional can be applied to different networks, the nodes
  are referenced by name instead of actual node objects.  The
  \code{\link{calcStat}} method finds the nodes in the network, and then
  calls the refenced \code{fun} with arguments
  \code{signature(net="Pnet", node)}, where node can either be a node or
  list of nodes.  (Note that the network object may or may not be needed
  to calculate the statistic value).

  Note that the statistic is free to return any kind of value.  The mean
  of a discrete variable is typically numeric (using
  \code{\link{PnodeStateValues}} to link states of the node with numeric
  values).  The mode and median return a \code{\link[base]{factor}}
  variable, and the margin is a vector of values on the unit simplex. 

  The current statistics are currently supported are:

  \describe{
    \item{\code{\link{PnodeMargin}}}{Provides the marginal distribution
      of a node.}
    \item{\code{\link{PnodeEAP}}}{Provides the expected a posteriori
      (i.e., mean) of a node using numeric values for the state from
      \code{\link{PnodeStateValues}}.}
    \item{\code{\link{PnodeSD}}}{Provides the standard deviation of a
      node using numeric values for the state from
      \code{\link{PnodeStateValues}}.}
    \item{\code{\link{PnodeMedian}}}{Provides the median value for a
      node, that is if the states are ordered, the one which is reached
      at a probability mass of 0.5.}
    \item{\code{\link{PnodeMode}}}{Returns the most likely state for
      (the marginal distribution of) node.}
  }

}
\value{

  The \code{Statistic} function returns an object of class
  \code{\linkS4class{Statistic}}.

}
\references{

  Almond, R.G., Mislevy, R.J. Steinberg, L.S., Yan, D. and Willamson, D.
  M. (2015). \emph{{Bayesian} Networks in Educational Assessment}.
  Springer.  Chapter 13.

}
\author{Russell Almond}
\seealso{

  Class:
  \code{\linkS4class{Statistic}}

  \code{\link{name}}
  
  Avaliable Statistic functions:
  \code{\link{PnodeMargin}}, \code{\link{PnodeEAP}}, \code{\link{PnodeSD}}, 
  \code{\link{PnodeMedian}}, \code{\link{PnodeMode}}.

  

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
irt10.theta <- NetworkFindNode(irt10.base,"theta")
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
CompileNetwork(irt10.base) ## Netica requirement

calcStat(marginTheta,irt10.base)
calcStat(meanTheta,irt10.base)
calcStat(sdTheta,irt10.base)
calcStat(medianTheta,irt10.base)
calcStat(modeTheta,irt10.base)

DeleteNetwork(irt10.base)
}
}
\keyword{classes}
\keyword{graphs}
