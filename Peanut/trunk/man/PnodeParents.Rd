\name{PnodeParents}
\alias{PnodeParents}
\alias{PnodeParents<-}
\title{ Gets or sets the parents of a parameterized node.}
\description{

  A parent of a child node is another node which has a
  link \emph{from} the parent \emph{to} the child.  This function
  returns the list of parents parents of the the node.  It
  allows the list of parents for the node to be set, altering the
  topology of the network (see details).
}
\usage{
PnodeParents(node)
PnodeParents(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{Pnode}} object whose parents are of interest.
  }
  \item{value}{
    A list of \code{\link{Pnode}} objects (or \code{NULL}s)
  which will become the new parents.  Order of the nodes is important.
  See details. 
  }
}
\details{
  At its most basic level, \code{PnodeParents()} reports on the topology
  of a network.  Suppose we add the links \code{A1 --> B},
  \code{A2 --> B},  and \code{A3 --> B} to the network.  Then
  \code{PnodeParents(B)} should return \code{list(A1, A2, A3)}.  The
  order of the inputs is important, because that this determines the
  order of the dimensions in the conditional probability table
  (\code{\link{BuildTable}()}).

  The parent list can be set.  This can accomplishes a number of
  different goals: it can replace a parent variable, it can add
  additional parents, it can remove extra parents, and it can reorder
  parents.  Changing the parents alters the topology of the network.
  Note that the network must always be acyclic directed graphs.  In
  particular, if changing the parent structure will result in a directed
  cycle,it will likely raise an error).

}
\value{

  A list of \code{\link{Pnode}} objects representing the
  parents in the order that they will be used to establish dimensions
  for the conditional probability table.  

  The setting variant returns the modified \var{child} object.

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnode}}, \code{\link{PnodeParentTvals}}

}
\examples{
\dontrun{
library(PNetica) ## Requires PNetica
sess <- NeticaSession()
startSession(sess)
abnet <- CreateNetwork("AB", session=sess)

anodes <- NewDiscreteNode(abnet, paste("A",1:3,sep=""))
B <- NewDiscreteNode(abnet,"B")

## Should be empty list
stopifnot(length(PnodeParents(B))==0)

PnodeParents(B) <- anodes
stopifnot(
  length(PnodeParents(B))==3,
  PnodeParents(B)[[2]] == anodes[[2]]
)

## Reorder nodes
PnodeParents(B) <- anodes[c(2:3,1)]
stopifnot(
  length(PnodeParents(B))==3,
  PnodeName(PnodeParents(B)[[2]])=="A3",
  all(nchar(names(PnodeParents(B)))==0)
)

## Remove a node.
PnodeParents(B) <- anodes[2:1]
stopifnot(
  length(PnodeParents(B))==2,
  PnodeName(PnodeParents(B)[[2]])=="A1",
  all(nchar(names(PnodeParents(B)))==0)
)

## Add a node
PnodeParents(B) <- anodes[3:1]
stopifnot(
  length(PnodeParents(B))==3,
  PnodeName(PnodeParents(B)[[3]])=="A1",
  all(nchar(names(PnodeParents(B)))==0)
)

## Remove all parents
PnodeParents(B) <- list()
stopifnot(
  length(PnodeParents(B))==0
)

DeleteNetwork(abnet)
stopSession(sess)
}}
\keyword{ interface }
\keyword{ graphs }
