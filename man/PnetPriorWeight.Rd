\name{PnetPriorWeight}
\alias{PnetPriorWeight}
\alias{PnetPriorWeight<-}
\alias{PnodePriorWeight}
\alias{PnodePriorWeight<-}
\alias{GetPriorWeight}
\title{Gets the weight to be associated with the prior table during EM learning}
\description{

  The EM learning algorithm \code{\link{GEMfit}} uses the built-in EM
  learning of the Bayes net to build expected count tables for each
  \code{\link{Pnode}}.  The expected count tables are a weighted average
  of the case data and the prior from the paramterized table.  This
  gives the weight, in number of cases, given to the prior.
  
}
\usage{
PnetPriorWeight(net)
PnetPriorWeight(net) <- value
PnodePriorWeight(node)
PnodePriorWeight(node) <- value
GetPriorWeight(node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{A \code{\link{Pnet}} object whose prior weight is to be
    accessed.}
  \item{node}{A \code{\link{Pnode}} object whose prior weight is to be
    accessed.}
  \item{value}{A nonnegative numeric vector giving the prior weight.
    This should either be a scalar or a vector with length equal to the
    number of rows of the conditional probability table.  In the case of
    \code{PnetPriorWeight} using a non-scalar value will produce
    unpredictable results.}
}
\details{

  Suppose that value of the node and all of its parents are fully
  observed, and let \eqn{X_{1i},\ldots,X_{ki}} be the observed counts
  for row \eqn{i}, and let \eqn{p_{1i},\ldots,p_{ki}} be the conditional
  probabilities for row \eqn{i}.  Then the posterior probabilities for
  row \eqn{i} can be found by normalizing
  \eqn{X_{1i}+w_ip_{1i},\ldots,X_{ki}+w_ip_{ki}}.  In the EM algorithm,
  the table is not fully observed but the expected value of
  \eqn{X_{1i},\ldots,X_{ki}} is used instead.

  This function gets or sets the vector \eqn{w_1,\ldots,w_I} (where
  \eqn{I} is the number of rows in the conditional probability table).
  If \code{value} is a scalar this is the same as giving all \eqn{w_i}
  the same value.

  The function \code{PnodePriorWeight} gets or sets the prior weight for
  a given node.  The function \code{PnetPriorWeight} gets or sets the
  default weight for all nodes (a property of the network).  Unless all
  nodes have the name number of parents with the same number of states,
  this should be a scalar.  The expression \code{GetPriorWeight(node)}
  gets the prior weight for the node or if that is null, it gets the
  default prior weight from the net (using the function
  \code{\link{PnodeNet}}.

}
\value{
  A numeric vector or scalar giving the weight or \code{NULL} if the
  default network weight is to be used.  
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}

\note{
  The \code{\link{GEMfit}} algorithm will update the prior weight for
  each node based on how much information is available for each row.
  Thus, even if the values are initially the same for each row, after
  calling \code{\link{GEMfit}} they usually will be different for each
  row. 

  The functions \code{PnetPriorWeight} and \code{PnodePriorWeight} are
  abstract generic functions, and they needs specific implementations.  See the
  \code{\link[PNetica]{PNetica-package}} for an example.


}
\seealso{
  \code{\link{Pnet}}, \code{\link{Pnode}}, \code{\link{PnodeNet}},
  \code{\link{BuildTable}}, \code{\link{GEMfit}}

}
\examples{

 \dontrun{

library(PNetica)  ## Implementation of Peanut protocol
## Create network structure using RNetica calls
IRT10.2PL <- CreateNetwork("IRT10_2PL")

theta <- NewDiscreteNode(IRT10.2PL,"theta",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta) <- effectiveThetas(NodeNumStates(theta))
NodeProbs(theta) <- rep(1/NodeNumStates(theta),NodeNumStates(theta))

J <- 10 ## Number of items
items <- NewDiscreteNode(IRT10.2PL,paste("item",1:J,sep=""),
                         c("Correct","Incorrect"))
for (j in 1:J) {
  NodeParents(items[[j]]) <- list(theta)
  NodeLevels(items[[j]]) <- c(1,0)
  NodeSets(items[[j]]) <- c("observables")
}

## Convert into a Pnet
IRT10.2PL <- Pnet(IRT10.2PL,priorWeight=10,pnodes=items)

## Convert nodes to Pnodes
for (j in 1:J) {
  items[[j]] <- Pnode(items[[j]])
}

PnodePriorWeight(items[[2]]) <- 5
## 5 states in parent, so 5 rows
PnodePriorWeight(items[[3]]) <- c(10,7,5,7,10)

stopifnot(
  abs(PnetPriorWeight(IRT10.2PL)-10) < .0001, 
  is.null(PnodePriorWeight(items[[1]])),
  abs(GetPriorWeight(items[[1]])-10) < .0001,
  abs(GetPriorWeight(items[[2]])-5) < .0001,
  any(abs(GetPriorWeight(items[[3]])-c(10,7,5,7,10)) < .0001)
)

PnetPriorWeight(IRT10.2PL) <- 15

stopifnot(
  abs(PnetPriorWeight(IRT10.2PL)-15) < .0001, 
  is.null(PnodePriorWeight(items[[1]])),
  abs(GetPriorWeight(items[[1]])-15) < .0001,
  abs(GetPriorWeight(items[[2]])-5) < .0001,
  any(abs(GetPriorWeight(items[[3]])-c(10,7,5,7,10)) < .0001)
)

DeleteNetwork(IRT10.2PL)
}
}
\keyword{ attrib }
