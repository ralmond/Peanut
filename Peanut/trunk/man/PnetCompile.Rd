\name{PnetCompile}
\alias{PnetCompile}
\title{Compiles a Parameterized Bayesian Network}
\description{

  This
  function requests that the Bayes net be compiled---transformed so that
  inference can be carried out.

}
\usage{
PnetCompile(net)
}
\arguments{
  \item{net}{A \code{\linkS4class{Pnet}} object to be compiled.}
}
\details{

  Many Bayesian network algorithm have two phases.  The graph is built
  as an acyclic directed graph. Before inference is carried out, the
  graph is transformed into a structure called a \emph{Junction Tree},
  \emph{Tree of Cliques} or \emph{Markov Tree} (Almond, 1995).

  This function requests that implementation specific processing,
  particularly, building the appropriate Markov Tree, be done for the
  net, so that it can be placed in inference mode instead of editing
  mode.

}
\value{
  The compile \code{net} argument should be returned.
}
\references{

  Almond, R. G. (1995).  \emph{Graphical Belief Models}.  Chapman and Hall.

}
\author{Russell Almond}
\note{
  
  It should be harmless to call this function on a net which is already
  compiled. 
}
\seealso{

  The following functions will likely return errors if the \code{net} is
  not compiled:
  \code{\link{PnodeEvidence}}, \code{\link{calcStat}},
  \code{\link{PnodeMargin}}, \code{\link{PnodeEAP}},
  \code{\link{PnodeSD}}, \code{\link{PnodeMedian}}, 
  \code{\link{PnodeMode}}. 

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

PnodeEvidence(irt10.items[[1]]) <- "Correct"

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

