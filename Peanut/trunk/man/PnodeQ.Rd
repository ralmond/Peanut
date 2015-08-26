\name{PnodeQ}
\alias{PnodeQ}
\alias{PnodeQ<-}
\title{Accesses a state-wise Q-matrix associated with a Pnode}
\description{

  The function \code{\link[CPTtools]{calcDPCTable}} has an argument
  \code{Q}, which allows the designer to specify that only certain
  parent variables are relevant for the state transition.  The function
  \code{PnodeQ} accesses the local Q-matrix for the \code{\link{Pnode}}
  \code{node}. 
}
\usage{
PnodeQ(node)
PnodeQ(node) <- value
}
\arguments{
  \item{node}{ A \code{\link{Pnode}} whose local Q-matrix is of interest}
  \item{value}{ A logical matrix with number of rows equal to the number
  of outcome states of \code{node} minus one and number of columns equal
  to the number of parents of \code{node}.  As a special case, if it has
  the value \code{TRUE} this is interpreted as a matrix of true values of
  the correct shape.}
}
\details{

  Consider a \code{\link[CPTtools]{partialCredit}} model, that is a
  \code{\link{Pnode}} for which the value of \code{\link{PnodeLink}} is
  \code{"partialCredit"}.  This model is represented as a series of
  transitions between the states \eqn{s+1} and \eqn{s} (in
  \code{\link[CPTtools]{calcDPCTable}} states are ordered from high to
  low). The log odds of this transition is expressed with a function
  \eqn{Z_{s}(eTheta)} where \eqn{Z_{s}()} is the value of
  \code{\link{PnodeRules}(node)} and \eqn{eTheta} is the result of the
  call \code{\link{PnodeParentTvals}(node)}.

  Let \eqn{q_{sj}} be true if the parent variable \eqn{x_j} is relevant
  for the transition between states \eqn{s+1} and \eqn{s}.  Thus the
  function which is evaluated to calculate the transition probabilities
  is \eqn{Z_{s}(eTheta[,Q[s,]])}; that is, the parent variables for
  which \eqn{q_{sj}} is false are filtered out.  The default value of
  \code{TRUE} means that no values are filtered.

  Note that this currently makes sense only for the
  \code{\link[CPTtools]{partialCredit}} link function.  The 
  \code{\link[CPTtools]{gradedResponse}} link function assumes that the
  curves are parallel and therefore all of the curves must have the same
  set of variables (and values for \code{\link{PnodeAlphas}}.  
}
\value{

  A logical matrix with number of rows equal to the number of outcome
  states of \code{node} minus one and number of columns equal to the
  number of parents of \code{node}, or the logical scalar \code{TRUE} if
  all parent variables are used for all transitions.
  
}
\references{

  Almond, R. G. (2013) Discretized Partial Credit Models for Bayesian
  Network Conditional Probability Tables.  Draft manuscript available
  from author.
  
  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\note{

  The functions \code{PnodeQ} and \code{PnodeQ<-} are  abstract generic
  functions, and need specific implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.

  The values of \code{\link{PnodeLink}}, \code{\link{PnodeRules}},
  \code{\link{PnodeQ}},  \code{\link{PnodeParentTvals}},
  \code{\link{PnodeLnAlphas}},  and \code{\link{PnodeBetas}} all need to
  be consistent for this to work correctly, but no error checking is
  done on any of the setter methods.


}
\seealso{
       \code{\link{Pnode}}, \code{\link{PnodeRules}}, 
       \code{\link{PnodeLink}}, \code{\link{PnodeLnAlphas}}, 
       \code{\link{PnodeAlphas}}, \code{\link{BuildTable}},
       \code{\link{PnodeParentTvals}}, \code{\link{maxCPTParam}}
       \code{\link[CPTtools]{calcDPCTable}}, \code{\link[CPTtools]{mapDPC}}

}
\examples{

\dontrun{
library(PNetica) ## Requires implementation

tNet <- CreateNetwork("TestNet")


theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
NodeProbs(theta2) <- rep(1/NodeNumStates(theta2),NodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
NodeParents(partial3) <- list(theta1,theta2)

partial3 <- Pnode(partial3,Q=TRUE, link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

## Default is all nodes relevant for all transitions   
stopifnot(
   length(PnodeQ(partial3)) == 1,
   PnodeQ(partial3) == TRUE
)

## Set up so that first skill only needed for first transition, second
## skill for second transition; adjust alphas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)
BuildTable(partial3)


partial4 <- NewDiscreteNode(tNet,"partial4",
                            c("Score4","Score3","Score2","Score1"))
NodeParents(partial4) <- list(theta1,theta2)
partial4 <- Pnode(partial4, link="partialCredit")
PnodePriorWeight(partial4) <- 10

## Skill 1 used for first transition, Skill 2 used for second
## transition, both skills used for the 3rd.

PnodeQ(partial4) <- matrix(c(TRUE,TRUE,
                             FALSE,TRUE,
                             TRUE,FALSE), 3,2, byrow=TRUE)
PnodeLnAlphas(partial4) <- list(Score4=c(.25,.25),
                                Score3=0,
                                Score2=-.25)
BuildTable(partial4)

DeleteNetwork(tNet)

}}
\keyword{ attrib }

