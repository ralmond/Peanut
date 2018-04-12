\name{PnetFindNode}
\alias{PnetFindNode}
\title{
Finds nodes in a parameterized network.
}
\description{
  The function \code{PnetFindNode} finds a node in a
  \code{\link{Pnet}} with the given name.  If no node with the
  specified name found, it will return \code{NULL}. 
}
\usage{
PnetFindNode(net, name)
}

\arguments{
  \item{net}{
    The \code{\link{Pnet}} to search.
}
  \item{name}{
    A character vector giving the name or names of the desired nodes.
}
}

\details{
  Although each \code{\link{Pnode}} belongs to a single network, a
  network contains many nodes.  Within a network, a node is uniquely
  identified by its name.  However, nodes can be renamed (see
  \code{\link{PnodeName}()}).  

}

\value{
  The \code{\link{Pnode}} object or list of \code{Pnode}
  objects corresponding to \code{names}, 
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnode}}, \code{\link{Pnet}} 

  \code{\link{PnodeNet}} retrieves the network for the node.
}
\examples{
\dontrun{
library(PNetica) # Requires PNetica
sess <- NeticaSession()
startSession(sess)

tnet <- CreateNetwork("TestNet",sess)
nodes <- NewDiscreteNode(tnet,c("A","B","C"))

nodeA <- PnetFindNode(tnet,"A")
stopifnot (nodeA==nodes[[1]])

nodeBC <- PnetFindNode(tnet,c("B","C"))
stopifnot(nodeBC[[1]]==nodes[[2]])
stopifnot(nodeBC[[2]]==nodes[[3]])

DeleteNetwork(tnet)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ graphs }
\keyword{ utilities }

