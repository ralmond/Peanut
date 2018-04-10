\name{PnodeTitle}
\alias{PnodeTitle}
\alias{PnodeTitle<-}
\alias{PnodeDescription}
\alias{PnodeDescription<-}
\title{
  Gets the title or Description associated with a parameterized node node.
}
\description{

  The title is a longer name for a node which is not subject to the
  naming restrictions.  The description is a free form
  text associated with a node.

}
\usage{
PnodeTitle(node)
PnodeTitle(node) <- value
PnodeDescription(node)
PnodeDescription(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{Pnode}} object.
  }
  \item{value}{
    A character object giving the new title or description.
  }
}
\details{

  The title is meant to be a human readable alternative to the name,
  which is not limited to the variable name restrictions (i.e., it can
  contain spaces and punctuation).  The title may also affect how the
  node is displayed. 

  The description is any text the user chooses to attach to the node.  If
  \var{value} has length greater than 1, the vector is collapsed into a
  long string with newlines separating the components.
}
\value{
  A character vector of length 1 providing the title or description.
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnode}}, \code{\link{PnodeName}()} 
}
\examples{
\dontrun{
library(PNetica) ## Requires PNetica
sess <- NeticaSession()
startSession(sess)
net2 <- CreateNetwork("secondNet", session=sess)

firstNode <- NewDiscreteNode(net2,"firstNode")

PnodeTitle(firstNode) <- "My First Bayesian Network Node"
stopifnot(PnodeTitle(firstNode)=="My First Bayesian Network Node")

now <- date()
PnodeDescription(firstNode)<-c("Node created on",now)
stopifnot(PnodeDescription(firstNode) ==
  paste(c("Node created on",now),collapse="\n"))

## Print here escapes the newline, so is harder to read
cat(PnodeDescription(firstNode),"\n")

DeleteNetwork(net2)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ attribute }
