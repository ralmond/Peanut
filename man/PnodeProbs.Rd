\name{PnodeProbs}
\alias{PnodeProbs}
\alias{PnodeProbs<-}
\title{  Gets or sets the conditional probability table associated with a
  Netica node.
}
\description{

  A complete Bayesian networks defines a conditional probability
  distribution for a node given its parents.  If all the nodes are
  discrete, this comes in the form of a conditional probability table a
  multidimensional array whose first several dimensions follow the
  parent variable and whose last dimension follows the child variable.

}
\usage{
PnodeProbs(node)
PnodeProbs(node) <- value
}
\arguments{
  \item{node}{
    An active, discrete \code{\linkS4class{Pnode}} whose conditional
    probability table is to be accessed.
  }
  \item{value}{
    The new conditional probability table.  See details for the expected
    dimensions.
  }
}
\details{

  Let \code{node} be the node of interest and \code{parent\var{1}},
  \code{parent\var{2}}, ..., \code{parent\var{p}}, where \eqn{p} is
  the number of parents.  Let \code{\var{pdim} =
  sapply(\link{PnodeParents}(\var{node}), 
  \link{PnodeNumStates})} be a vector with the number of states for each parent.
  A parent configuration is defined by assigning each of the parent
  values to one of its possible states.  Each parent configuration
  defines a (conditional) probability distribution over the possible
  states of \var{node}.

  The result of \code{PnodeProbs(\var{node})} will be an array with dimensions
  \code{c(\var{pdim}, PnodeNumStates(\var{node}))}.  The first \eqn{p}
  dimensions will be named according to the
  \code{\link{PnodeParentNames}(\var{node})}.  The
  last dimension will be named according to the node itself.  The
  \code{dimnames} for the resulting array will correspond to the state
  names.  

  In the \code{CPTtools} package, this known as the
  \code{\link[CPTtools]{CPA}} format, and tools exist to convert between
  this form an a two dimensional matrix, or \code{\link[CPTtools]{CPF}}
  format. 
  
  The setter form expects an array of the same dimensions as an
  argument, although it does not need to have the dimnames set.

}
\value{

  A conditional probability array of class
  \code{c("\link{CPA}","array")}.  See \code{\link[CPTtools]{CPA}}.

}
\author{Russell Almond}
\note{

  All of this assumes that these are discrete nodes, that is
  \code{\link{isPnodeContinuous}(node)} will return false for both
  \code{node} and all of the parents, or that the continuous nodes have
  been discritized through the use of \code{\link{PnodeStateBounds}}. 

}
\seealso{
  \code{\linkS4class{Pnode}},
  \code{\link{BuildTable}},  \code{\link[CPTtools]{CPA}},
  \code{\link[CPTtools]{CPF}}, \code{\link[CPTtools]{normalize}()},
  \code{\link{PnodeParents}()},
  \code{\link{PnodeStates}()}
  
}

\examples{

\dontrun{ ## Requires implementation
sess <- NeticaSession()
startSession(sess)
abc <- CreateNetwork("ABC", session=sess)
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

PnodeParents(A) <- list()
PnodeParents(B) <- list(A)
PnodeParents(C) <- list(A,B)

PnodeProbs(A)<-c(.1,.2,.3,.4)
PnodeProbs(B) <- normalize(matrix(1:12,4,3))
PnodeProbs(C) <- normalize(array(1:24,c(A=4,B=3,C=2)))

Aprobs <- PnodeProbs(A)
Bprobs <- PnodeProbs(B)
Cprobs <- PnodeProbs(C)
stopifnot(
  CPTtools::is.CPA(Aprobs),
  CPTtools::is.CPA(Bprobs),
  CPTtools::is.CPA(Cprobs)
)

DeleteNetwork(abc)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ model }
