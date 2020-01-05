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
    A \code{\link{Pnode}} object that references the node.
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
\section{True Names}{

  True names are the names in the secret ancient lanugage which hold
  power over an object (Le Guin, 1968).

  Actually, this is a difficulty with implementations that place
  restrictions on the name of a network or node.  In particular, Netica
  restricts node names to alphanumeric characters and limits the length.
  This may make it difficult to match nodes by name with other parts of
  the system which do not have this restriction.  In this case the
  object may have both a \emph{true name}, which is returned by
  \code{PnodeName} and an internal \emph{use name} which is used by the
  implementation.
  

}
\author{
  Russell Almond
}
\references{

  Le Guin, U. K. (1968).  \emph{A Wizard of Earthsea.}  Parnassus Press.

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

PnodeName(pnode)<-"work"
stopifnot(PnetFindNode(net,"work")==pnode)

PnodeName(pnode) <- "Non-Netica Name"
stopifnot(PnetFindNode(net,"Non-Netica Name")==pnode)

DeleteNetwork(net)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ attribute }
