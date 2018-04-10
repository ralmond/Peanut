\name{PnodeStateValues}
\alias{PnodeStateValues}
\alias{PnodeStateValues<-}
\title{
  Accesses the numeric values associated with the state of a
  parameterized node.
}
\description{

  The values are a numeric value (on a standard normal scale) associated
  with the levels of a discrete \code{\link{Pnode}}.  This function
  fetches or retrieves the numeric values for the states of \var{node}.  

  Note that the default method for the funciton
  \code{\link{PnodeParentTvals}} uses the values of
  \code{PnodeStateValues} on the parent nodes.

}
\usage{
PnodeStateValues(node)
PnodeStateValues(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{Pnode}} whose levels are to be accessed.
  }
  \item{value}{
    A numeric vector of values which 
    should have length \code{length(\link{PnodeStates}(\var{node}))}. 
  }
}
\details{

  Note that the first time the \code{PnodeStateValues()} are set, the entire
  vector must be set.  After that point individual values may be
  changed. 

}
\value{
  A numeric vector of length \code{length(Pnodetates())},
  with names equal to the state names.  If levels have not be set, NAs
  will be returned.

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnode}},   \code{\link{PnodeStates}()}, 
  \code{\link{PnodeName}()}, \code{\link{PnodeStateTitles}()},
  \code{\link{PnodeParentTvals}()}
}
\examples{
\dontrun{
library(PNetica)##Requires PNetica
sess <- NeticaSession()
startSession(sess)
lnet <- CreateNetwork("LeveledNet", session=sess)

vnode <- NewDiscreteNode(lnet,"volt_switch",c("Off","Reverse","Forwards"))
stopifnot(
  length(PnodeStateValues(vnode))==3,
  names(PnodeStateValues(vnode)) == NodeStates(vnode),
  all(is.na(PnodeStateValues(vnode)))
)

## Don't run this until the levels for vnode have been set,
## it will generate an error.
try(PnodeStateValues(vnode)[2] <- 0)

PnodeStateValues(vnode) <- 1:3
stopifnot(
  length(PnodeStateValues(vnode))==3,
  names(PnodeStateValues(vnode)) == PnodeStates(vnode),
  PnodeStateValues(vnode)[2]==2
)

PnodeStateValues(vnode)["Reverse"] <- -2

DeleteNetwork(lnet)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ attribute }
