\name{Pnet}
\alias{Pnet}
\alias{Pnet.default}
\alias{is.Pnet}
\alias{as.Pnet}
\title{A Parameterized Bayesian network}
\description{

  A parameterized Bayesian network.  Note that this is a \dQuote{mixin}
  class, it is meant be added to another Bayesian network class to
  indicate that it contains parameterized nodes.
}
\usage{
is.Pnet(x)
as.Pnet(x)
Pnet(net, priorWeight=10, pnodes=list())
\method{Pnet}{default}(net, priorWeight=10, pnodes=list())


}
\arguments{
  \item{x}{A object to test to see if it a parameterized network, or to
    coerce into a parameterized network.}
  \item{net}{A network object which will become the core of the
    \code{Pnet}.  Note that this should probably already be another kind
    of network, e.g., a \code{\link[RNetica]{NeticaBN}} object.}
  \item{priorWeight}{A numeric vector providing the default prior weight
    for nodes.}
  \item{pnodes}{A list of objects which can be coerced into
    \code{node} objects.  Note that the function does not do the
    coercion.}
}
\details{

  The \code{Pnet} class is basically a protocol which any Bayesian
  network net object can follow to work with the tools in the Peanut
  package.  This is really an abstract class (in the java programming
  language, \code{Pnet} would be an interface rather than a class) which
  exploits the rather loose S3 object system.  In particular, a
  \code{Pnet} is any object for which \code{is.Pnet} returns true.  The
  default method looks for the string \code{"Pnet"} in the class list.

  A \code{Pnet} object has two \dQuote{"fields"} (implemented through
  the accessor methods).  The function \code{PnetPnodes} returns a list
  of parameterized nodes or \code{\link{Pnode}}s associate with the
  network.  The function \code{\link{PnetPriorWeight}} gets (or sets)
  the default weight to be used for each node.  

  The default constructor adds \code{"Pnet"} to the class of \code{net}
  and then sets the two fields using the accessor functions.  There is
  no default method for the \code{as.Pnet} function.

  The importance of the \code{Pnet} object is that it supports the
  \code{\link{GEMfit}} method which adjust the parameters of the
  \code{Pnode} objects to fit a set of case data.  In order to be
  compatible with \code{GEMfit}, the \code{Pnet} object must support
  four methods:  \code{\link{BuildAllTables}},
  \code{\link{calcPnetLLike}}, \code{\link{calcExpTables}}, and
  \code{\link{maxAllTableParams}}.

  The generic function \code{\link{BuildAllTables}} builds conditional
  probability tables from the current values of the parameters in all
  \code{Pnode}s.  The default method loops through all of the nodes in
  \code{\link{PnetPnodes}} and calls the function
  \code{\link{BuildTable}} on each.

  The generic function \code{\link{calcPnetLLike}} calculates the log
  likelihood of a set of cases given the current values of the
  parameters.  There is no default for this method as it implementation
  dependent. 

  The generic function \code{\link{calcExpTables}} calculates expected
  cross-tabs for all CPT for the \code{Pnode}s given a set of case
  data.  The easiest way to do this is to run the EM algorithm
  for an unconstrained hyper-Dirichlet model for one or two cycles.
  There is no default for this as it is implementation dependent.

  The generic function \code{\link{maxAllTableParams}} calculates the
  parameters that maximize the fit to the expected tables for each
  \code{Pnode}.  The default method loops over
  \code{\link{PnetPnodes}(net)} and applies the method
  \code{\link{maxCPTParam}} to each.

}
\value{
  The function \code{is.Pnet} returns a logical scalar indicating
  whether or not the object claims to follow the \code{Pnet} protocol.

  The function \code{as.Pnet} and \code{Pnet} convert the argument into
  a \code{Pnet} and return that.
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\seealso{

  Fields: \code{\link{PnetPriorWeight}}, \code{\link{PnetPnodes}}
    
  Generic Functions: 
  \code{\link{BuildAllTables}}, \code{\link{calcPnetLLike}},
  \code{\link{calcExpTables}}, \code{\link{maxAllTableParams}}

  Functions: \code{\link{GEMfit}}

  Related Classes: \code{\link{Pnode}}
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

## Draw random parameters
btrue <- rnorm(J)
lnatrue <- rnorm(J)/sqrt(3)
dump(c("btrue","lnatrue"),"IRT10.2PL.params.R")

## Convert nodes to Pnodes
for (j in 1:J) {
  items[[j]] <- Pnode(items[[j]],lnatrue[j],btrue[j])
}
BuildAllTables(IRT10.2PL)
is.Pnet(IRT10.2PL)
WriteNetworks(IRT10.2PL,"IRT10.2PL.true.dne")

DeleteNetwork(IRT10.2PL)
}

}
\keyword{ classes }
\keyword{ graphs }
\keyword{ interface }
