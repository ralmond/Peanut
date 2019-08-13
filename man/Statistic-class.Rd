\name{Statistic-class}
\Rdversion{1.1}
\docType{class}
\alias{Statistic-class}
\alias{calcStat,Statistic-method}
\alias{name,Statistic-method}

\title{Class \code{"Statistic"}}
\description{

  A statistic is a functional that when applied to a Bayesian network
  returns a value.  Usually, the statistic is a function of the
  distribution of a single node, but it could also be a function of
  several nodes.  Statistic objets have a \code{\link{calcStat}} method,
  which when applied to a network, produces the value.  Lists of
  statistics are often maintained by Bayes net engines to report values
  at designated times (e.g., after new evidence arrives).

}
\section{Objects from the Class}{
Objects are created using the function \code{\link{Statistic}(fun, node,
  name, ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} giving an
      identifier for the statistic.}
    \item{\code{node}:}{Object of class \code{"character"} giving the
      name(s) of the node(s) that are referenced by the statistic.  Note
      that these are not the actual node objects, as the network could
      be different at each call.}
    \item{\code{fun}:}{Object of class \code{"character"} giving a
      function to be applied to the nodes. The function should have
      \code{signature(net="Pnet", node)}, where \code{node} could be
      either a \code{\link{Pnode}} or a list of Pnodes.}
  }
}
\section{Methods}{
  \describe{
    \item{calcStat}{\code{signature(stat = "Statistic", net)}: This method
      (a) finds the nodes referenced in \code{node}, (b) applies
      \code{fun} (using \code{do.call} to \code{net} and the actual nodes.}
    \item{name}{\code{signature(x = "Statistic")}: Returns the name of
      the statistic.}
  }
}
\references{

  Almond, R.G., Mislevy, R.J. Steinberg, L.S., Yan, D. and Willamson, D.
  M. (2015). \emph{{Bayesian} Networks in Educational Assessment}.
  Springer.  Chapter 13.

}
\author{Russell Almond}
\seealso{

  Avaliable Statistic functions:
  \code{\link{PnodeMargin}}, \code{\link{PnodeEAP}}, \code{\link{PnodeSD}}, 
  \code{\link{PnodeMedian}}, \code{\link{PnodeMode}}.

  Constructor function:
  \code{\link{Statistic}}

  \code{\link{calcStat}}, \code{\link{name}}
  

}
\examples{

\dontrun{

library(PNetica) ## Need a specific implementation

irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne",
                           sep=.Platform$file.sep))
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
