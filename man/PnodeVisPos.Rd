\name{PnodeVisPos}
\alias{PnodeVisPos}
\alias{PnodeVisPos<-}
\alias{PnetCanvas}
\alias{PnetCanvas<-}
\title{Get/Set information about the visual layout of the network}
\description{

  Each node in the networks can be associated with a visual position,
  an \eqn{(x,y)} co-ordinate.  The canvas a rectangular region which
  contains all of the nodes.
}
\usage{
PnodeVisPos(node)
PnodeVisPos(node) <- value
PnetCanvas(net)
PnetCanvas(net) <- value
}
\arguments{
  \item{node}{A \code{\linkS4class{Pnode}} whose position is in
    question.}
  \item{net}{A \code{\linkS4class{Pnet}} whose canvas is in question.}
  \item{value}{A vector of two numbers for a position or a list of two
  vectors for the canvas (see details).}
}
\details{

  The position is a vector of length 2 with the first element the
  \eqn{X} co-ordinate and the second the \eqn{Y}.  What is returned if a
  node does not have a set position is implementation dependent, it
  could be \code{(NA, NA)} or a default value such as \code{(0, 0)}.

  The canvas is marked by two corners of the rectangle that contains all
  of the nodes.  The default method returns the co-ordinates of the
  lower-left and upper-right positions.  (Note that \eqn{Y} coordinates
  are often reversed by various plotting routines.)  

}
\value{

  \code{PnodeVisPos} returns a numeric vector of lenght 2 giving the
  co-ordinates of the node.

  \code{PnetCanvas} returns a names list with two positions (numeric
  vectors of length two).  These should be labelled with the labels
  \code{ll} (lower left), \code{ul} (upper left), \code{lr} (lower
  right), or \code{ur} (upper right).

  The setters return the modified object.

}
\author{Russell Almond}
\note{

  There is a fair bit of this which is left to the implementation.  For
  example, some implementations may require integers, or forbid negative
  co-ordinates.  They may also vary as to whether each node is given a
  default position, or some kind of null value is allowed.  This latter
  should be represented as \code{(NA, NA)}.

  Also, how the canvas is represented is very implementation dependent.
  Implementations are free to offer any subset of the four corners which
  define the whole rectangle.  Some implementations (e.g.,
  \code{\link[RNetica=RNetica-package]{RNetica}}) may not actually
  define the canvas.  In this case, it can be calculated by finding
  a bounding rectangle that contains all of the nodes.  The method
  \code{PnetCanvas.default} calculates this, returning the lower-left and
  upper-right co-ordinates.  If no nodes have positions, it will return
  \code{list(ll=c(0,0), ur=c(0,0))}.

  

}
\seealso{
  \code{\link{pnet2Igraph}}
}
\examples{

\dontrun{

  PnodeVisPos(anode)
  PnodeVisPos(anode) <- c(3,4)

  PnetCanvas(agraph)
  PnetCanvas(agraph) <- list(ll=c(0,0),ur=c(10,10))

}

}
\keyword{ graph }
\keyword{ attribute }
\concept{ graph-layout }

