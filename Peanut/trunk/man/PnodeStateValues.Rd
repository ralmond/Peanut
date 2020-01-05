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

  This function behaves differently for discrete and continuous nodes
  (see \code{\link{isPnodeContinuous}}).  For discrete nodes, the states
  are numeric values associated with the states.  These are used in a
  number of ways, most importantly, as \code{\link{PnodeParentTvals}}.
  Note that the first time the \code{PnodeStateValues()} are set, the entire
  vector must be set.  After that point individual values may be
  changed.

  For continuous nodes, the state values are set by setting the
  \code{\link{PnodeStateBounds}} for the node.  The value is the
  midpoint of each interval.  (Note this produces an infinite state
  value if one of the state bounds in infinite).

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
  names(PnodeStateValues(vnode)) == PnodeStates(vnode),
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

## Continuous nodes get the state values from the bounds.
theta0 <- NewContinuousNode(lnet,"theta0")
stopifnot(length(PnodeStateValues(theta0))==0L)
norm5 <- 
   matrix(c(qnorm(c(.001,.2,.4,.6,.8)),
            qnorm(c(.2,.4,.6,.8,.999))),5,2,
          dimnames=list(c("VH","High","Mid","Low","VL"),
                        c("LowerBound","UpperBound")))
PnodeStateBounds(theta0) <- norm5
PnodeStateValues(theta0)  ## Note these are medians not mean wrt normal!
PnodeStateBounds(theta0)[1,1] <- -Inf
PnodeStateValues(theta0)  ## Infinite value!


DeleteNetwork(lnet)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ attribute }
