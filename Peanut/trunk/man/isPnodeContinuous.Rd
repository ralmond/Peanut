\name{isPnodeContinuous}
\alias{isPnodeContinuous}
\alias{PnodeStateBounds}
\alias{PnodeStateBounds<-}
\title{Functions for handling continuous nodes.}
\description{

  Continuous nodes are handled slightly differently from discrete
  nodes.  The function \code{isPnodeContinuous} returns a logical value
  indicating whether or not the node is continuous.

  Continuous nodes can behave like discrete nodes (for the purposes of
  building conditional probability tables, see \code{\link{BuildTable}})
  if states are created from ranges of values.  The function
  \code{PnodeStateBounds} accesses those ranges.

}
\usage{
isPnodeContinuous(node)
PnodeStateBounds(node)
PnodeStateBounds(node) <- value
}
\arguments{
  \item{node}{A \code{\link{Pnode}} object.}
  \item{value}{A k by 2 numeric matrix giving the upper and lower bound
    for each state.}
}
\details{

  Continuous, in this case, covers nodes whose possible states are
  numeric, either integer or real.  The current model supports these
  nodes in a discrete Bayesian network by discretizing them.  In
  particular, the range is broken up into a number of non-overlapping
  regions, each region corresponding to a state.

  For example, consider a variable which is a count, and the analyst
  wants to consider the values 0, 1, 2 or 3, and 4 or more.  This can be
  done by setting bounds on these states:

  \tabular{lrr}{
    "Zero" \tab -0.5 \tab 0.5 \cr
    "One"  \tab  0.5 \tab 1.5 \cr
    "TwoThree" \tab 1.5 \tab 3.5 \cr
    "FourPlus" \tab 3.5 \tab Inf \cr
  }

  This matrix is the \code{NodeStateBounds} for the node.  Note that the
  second column is the same as the first (offset by one).  Note also
  that infinite (\code{Inf} and \code{-Inf}) values are allowed.

  Setting the state bounds to a matrix with \eqn{k} rows, will make the
  variable behave as if it has \eqn{k} states.  

}
\note{

  This is rather strongly tied to how Netica treats continuous
  variables.  A different mechism might be necessary as Peanut is
  expanded to cover more implementations.

  Right now, the value is the midpoint of the interval.  This cause
  problems when converting to T-values.

}
\value{

  The function \code{isPnodeContinuous} returns a logical value.

  The function \code{PnodeStateBounds} returns a \eqn{k} by 2 numeric
  matrix giving the upper and lower bounds.  Note that if bounds have
  not been set for the node, then it will return a matrix with 0 rows.

}
\author{Russell Almond}
\seealso{

  \code{\link{Pnode}}, \code{\link{PnodeStateValues}},
  \code{\link{PnodeParentTvals}} 

}
\examples{

\dontrun{
library(PNetica) ## Requires implementation
sess <- NeticaSession()
startSession(sess)

tNet <- CreateNetwork("TestNet",session=sess)

theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
stopifnot (!isPnodeContinuous(theta1))

## This gives an error
out <- try(PnodeStateBounds(theta1))
stopifnot (is(out,'try-error'))

theta0 <- NewContinuousNode(tNet,"theta0")
stopifnot(nrow(PnodeStateBounds(theta0)) == 0L)

norm5 <- 
   matrix(c(qnorm(c(.001,.2,.4,.6,.8)),
            qnorm(c(.2,.4,.6,.8,.999))),5,2,
          dimnames=list(c("VH","High","Mid","Low","VL"),
                        c("LowerBound","UpperBound")))
PnodeStateBounds(theta0) <- norm5
PnodeStates(theta0)
PnodeStateBounds(theta0)
PnodeStateValues(theta0)  ## Note these are medians not mean wrt normal!

}
}
\keyword{ manip }

