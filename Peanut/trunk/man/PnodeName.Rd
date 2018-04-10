\name{PnodeName}
\alias{PnodeName}
\alias{PnodeName<-}
\title{
  Gets or sets name of a parameterized node.
}
\description{

  Gets or sets the name of the node. Rules for names are implementation
  dependent, but they should generally conform to variable naming
  conventions (begin with a letter and only contain alphanumeric
  characters, no embeded spaces.)

}
\usage{
PnodeName(node)
PnodeName(node)<- value
}
\arguments{
  \item{node}{
    A \code{\link{Pode}} object that references the node.
  }
  \item{value}{
    An character vector of length 1 giving the new name.  
  }
}
\details{

  The \code{\link{PnodeTitle}()} function provides another way to name
  a node which is not subject to naming restrictions.

}
\value{
  The name of the node as a character vector of length 1.

  The setter method returns the \code{node} argument.
}
\author{
  Russell Almond
}

\seealso{
  \code{\link{Pnode}}, \code{\link{PnetFindNode}()},
  \code{\link{PnodeTitle}()},
}
\examples{
\dontrun{
library(PNetica) # Requires PNetica
sess <- NeticaSession()
startSession(sess)
net <- CreateNetwork("funNet", session=sess)

pnode <- NewDiscreteNode(net,"play")

stopifnot(PnodeName(pnode)=="play")
stopifnot(PnetFindNode(net,"play")==pnode)

NodeName(pnode)<-"work"
stopifnot(PnetFindNode(net,"work")==pnode)

DeleteNetwork(net)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ attribute }
