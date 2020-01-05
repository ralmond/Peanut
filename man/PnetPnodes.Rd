\name{PnetPnodes}
\alias{PnetPnodes}
\alias{PnetPnodes<-}
\alias{PnodeNet}
\title{Returns a list of Pnodes associated with a Pnet}
\description{

  Each \code{\link{Pnet}} object maintains a list of \code{\link{Pnode}}
  objects which it is intended to set.  The function \code{PnetPnodes}
  accesses this list.  The function \code{PnodeNet} returns a
  back pointer to the \code{Pnet} from the \code{Pnode}.
  
}
\usage{
PnetPnodes(net)
PnetPnodes(net) <- value
PnodeNet(node)
}
\arguments{

  \item{net}{A \code{\link{Pnet}} object.}
  \item{node}{A \code{\link{Pnode}} object.}
  \item{value}{A list of \code{Pnode} objects associated with \code{net}.}

}
\details{

  The primary purpose of \code{PnetPnodes} is to provide a list of nodes
  which \code{\link{GEMfit}} and \code{\link{BuildAllTables}} will
  iterate to do their function.

  The function \code{PnodeNet} returns the network object associated
  with the \code{node} (this assumes that the implementation has
  back pointers).  Note that \code{node} may not be in the result of
  \code{PnetPnodes} (if for example, the conditional probability table
  of \code{node} is to remain fixed during a call to
  \code{\link{GEMfit}}).  This function is used by
  \code{\link{GetPriorWeight}} to get the default prior weight if
  \code{node} does not have that value set locally.
}
\value{

  The function \code{PnetPnodes} returns a list of \code{\link{Pnode}}
  objects associated with the net.  The expression \code{PnetPnodes(net)
    <- value} returns the \code{net}.

  The function \code{PnodeNet} returns the network (\code{\link{Pnet}})
  object that contains \code{node}.
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\note{

  The functions \code{PnetPnodes} and \code{PetPnodes<-} and
  \code{PnodeNet} are  abstract generic functions, and need specific
  implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.

}
\seealso{
  \code{\link{Pnet}}, \code{\link{Pnode}}, \code{\link{GetPriorWeight}},
  \code{\link{BuildAllTables}}, \code{\link{GEMfit}}

}
\examples{

 \dontrun{

library(PNetica)  ## Implementation of Peanut protocol
sess <- NeticaSession()
startSession(sess)
## Create network structure using RNetica calls
IRT10.2PL <- CreateNetwork("IRT10_2PL",session=sess)

theta <- NewDiscreteNode(IRT10.2PL,"theta",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta) <- effectiveThetas(PnodeNumStates(theta))
NodeProbs(theta) <- rep(1/PnodeNumStates(theta),PnodeNumStates(theta))

J <- 10 ## Number of items
items <- NewDiscreteNode(IRT10.2PL,paste("item",1:J,sep=""),
                         c("Correct","Incorrect"))
for (j in 1:J) {
  PnodeParents(items[[j]]) <- list(theta)
  PnodeStateValues(items[[j]]) <- c(1,0)
  PnodeLabels(items[[j]]) <- c("observables")
}
## Convert into a Pnet
IRT10.2PL <- Pnet(IRT10.2PL,priorWeight=10,pnode=items[2:J])
for (j in 2:J) {
  items[[j]] <- Pnode(items[[j]])
}

stopifnot(
  length(PnetPnodes(IRT10.2PL)) == J-1, # All except item 1
  PnodeNet(items[[2]]) == IRT10.2PL, 
  PnodeNet(items[[1]]) == IRT10.2PL # this is net membership, not
                                     # Pnodes field
)

PnetPnodes(IRT10.2PL) <- items ## Add back item 1
stopifnot(
  length(PnetPnodes(IRT10.2PL)) == J
)
DeleteNetwork(IRT10.2PL)
stopSession(sess)
}
}
\keyword{ attrib }

