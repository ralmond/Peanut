\name{PnodeLnAlphas}
\alias{PnodeLnAlphas}
\alias{PnodeLnAlphas<-}
\alias{PnodeAlphas}
\alias{PnodeAlphas<-}
\alias{PnodeAlphas.default}
\alias{PnodeAlphas<-.default}
\title{Access the combination function slope parameters for a Pnode}
\description{

  In constructing a conditional probability table using the discrete
  partial credit framework (see \code{\link[CPTtools]{calcDPCTable}}),
  the effective thetas for each parent variable are combined into a
  single effect theta using a combination rule.  The expression
  \code{PnodeAlphas(node)} accesses the slope parameters associated with
  the combination function \code{\link{PnodeRules}(node)}.  The
  expression \code{PnodeLnAlphas(node)} which is used in
  \code{\link[CPTtools]{mapDPC}}.


}
\usage{
PnodeLnAlphas(node)
PnodeLnAlphas(node) <- value
PnodeAlphas(node)
PnodeAlphas(node) <- value
\method{PnodeAlphas}{default}(node)
\method{PnodeAlphas}{default}(node) <- value

}
\arguments{
  \item{node}{A \code{\link{Pnode}} object.}
  \item{value}{A numeric vector of (log) slope parameters or a list of
    such vectors (see details).  The length of the vector depends on the
    combination rules (see \code{\link{PnodeRules}}).  If
    a list, it should have length one less than the number of states in
    \code{node}.  For \code{PnodeAlphas(node) <- value}, \code{value}
    should only contain positive numbers.}
}
\details{

  Following the framework laid out in Almond (2015), the function
  \code{\link[CPTtools]{calcDPCTable}} calculates a conditional
  probability table using the following steps:
  \enumerate{
    \item{Each set of parent variable states is converted to a set of
      continuous values called \emph{effective thetas} (see
      \code{\link{PnodeParentTvals}}).  These are built into an array,
      \code{eTheta}, using \code{\link[base]{expand.grid}} where each
      column represents a parent variable and each row a possible
      configuration of parents.} 

    \item{For each state of the \code{node} except the last,
      the set of effective thetas is filtered using the local Q-matrix,
      \code{\link{PnodeQ}(node) = Q}.  Thus, the actual effect thetas
      for state \code{s} is \code{eTheta[,Q[s,]]}.}

    \item{For each state of the \code{node} except the last, the
      corresponding rule is applied to the effective thetas to get a
      single effective theta for each row of the table.  This step is
      essentially calls the expression:
      \code{do.call(rules[[s]],} \code{list(eThetas[,Q[s,]]),} 
      \code{PnodeAlphas(node)[[s]],} \code{PnodeBetas(node)[[s]])}.}

    \item{The resulting set of effective thetas are converted into
      conditional probabilities using the link function
      \code{\link{PnodeLink}(node)}.}
  }
      
  The function \code{\link{PnodeRules}} accesses the function used in step 3.
  It should should be the name of a function or a function with the
  general signature of a combination function described in
  \code{\link[CPTtools]{Compensatory}}.  The compensatory function is a
  useful model for explaining the roles of the slope parameters,
  \eqn{alpha}.  Let \eqn{theta_{i,j}} be the effective theta value for the
  \eqn{j}th parent variable on the \eqn{i}th row of the effective theta
  table, and let \eqn{alpha_{j}} be the corresponding slope parameter.
  Then the effective theta for that row is:
  \deqn{Z(theta_{i}) =
    (alpha_1theta_{i,1}+\ldots+alpha_Jtheta_{J,1})/C - beta ,}
  where \eqn{C=\sqrt(J)} is a variance stabilization constant and
  \eqn{beta} is a value derived from \code{\link{PnodeBetas}}.  The
  functions \code{\link[CPTtools]{Conjunctive}} and
  \code{\link[CPTtools]{Disjunctive}} are similar replacing the sum with
  a min or max respectively.

  In general, when the rule is one of
  \code{\link[CPTtools]{Compensatory}}, 
  \code{\link[CPTtools]{Conjunctive}}, or
  \code{\link[CPTtools]{Disjunctive}}, the the value of
  \code{PnodeAlphas(node)} should be a vector of the same length as the
  number of parents.  As a special case, if it is a vector of length 1,
  then a model with a common slope is used.  This looks the same in
  \code{\link[CPTtools]{calcDPCTable}} but has a different implication
  in \code{\link[CPTtools]{mapDPC}} where the parameters are constrained
  to be the same.
  
  The rules \code{\link[CPTtools]{OffsetConjunctive}}, and
  \code{\link[CPTtools]{OffsetDisjunctive}}, work somewhat differently,
  in that they assume there is a single slope and multiple intercepts.
  Thus, the \code{OffsetConjunctive} has equation:
  \deqn{Z(theta_{i}) = alpha min(theta_{i,1}-beta_1,
    \ldots,theta_{J,1}-beta_{J}) .}
  In this case the assumption is that \code{PnodeAlphas(node)} will be a
  scalar and \code{\link{PnodeBetas}(node)} will be a vector of length
  equal to the number of parents.

  If the value of \code{\link{PnodeLink}} is
  \code{\link[CPTtools]{partialCredit}}, then the link function can be
  different for each state of the \code{node}.  (If it is
  \code{\link[CPTtools]{gradedResponse}} then the curves need to be
  parallel and the slopes should be the same.)  If the value of
  \code{PnodeAlphas(node)} is a list (note:  list, not numeric vector or
  matrix), then a different set of slopes is used for each state
  transition.  (This is true whether \code{\link{PnodeRules}(node)} is a
  single function or a list of functions.  Note that if there is a
  different rule for each transition, they could require different
  numbers of slope parameters.)  The function 
  \code{\link[CPTtools]{calcDPCTable}} assumes the states are ordered
  from highest to lowest, and no transition is needed into the lowest
  state.

  Node that if the value of \code{\link{PnodeQ}(node)} is not a matrix
  of all \code{TRUE} values, then the effective number of parents for
  each state transition could be different.  In this case the value of
  \code{PnodeAlphas(node)} should be a list of vectors of different
  lengths (corresponding to the number of true entries in each row of 
  \code{\link{PnodeQ}(node)}).

  Finally, note that if we want the conditional probability table
  associated with \code{node} to be monotonic, then the
  \code{PnodeAlphas(node)} must be positive.  To ensure this, 
  \code{\link[CPTtools]{mapDPC}} works with the log of the slopes, not
  the raw slopes.  Similarly, \code{\link[CPTtools]{calcDPCTable}}
  expects the log slope parameters as its \code{lnAlphas} argument, not
  the raw slopes.  For that reason \code{PnodeLnAlphas(node)} is
  considered the primary function and a default method for
  \code{PnodeAlphas(node)} which simply takes exponents (or logs in the
  setter) is provided.  Note that a sensible range for the slope
  parameters is usually between 1/2 and 2, with 1 (0 on the log scale) as
  a sensible first pass value.

}
\value{

  A list of numeric vectors giving the slopes for the combination
  function of each state transition.   The vectors may be of different
  lengths depending on the value of \code{\link{PnodeRules}(node)} and
  \code{\link{PnodeQ}(node)}.  If the slopes are the same for all
  transitions (as is required with the
  \code{\link[CPTtools]{gradedResponse}} link function) then a single
  numeric vector instead of a list is returned.

}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

}
\author{Russell Almond}

\note{
  The functions \code{PnodeLnAlphas} and \code{PnodeLnAlphas<-} are
  abstract generic functions, and need specific implementations.
  The default methods for the functions \code{PnodeAlphas} and
  \code{PnodeAlphas<-}.  Depend on \code{PnodeLnAlphas} and
  \code{PnodeLnAlphas<-}, respectively.   See the  
  \code{\link[PNetica]{PNetica-package}} for an example.

  The values of \code{\link{PnodeLink}}, \code{\link{PnodeRules}},
  \code{\link{PnodeQ}},  \code{\link{PnodeParentTvals}},
  \code{\link{PnodeLnAlphas}},  and \code{\link{PnodeBetas}} all need to
  be consistent for this to work correctly, but no error checking is
  done on any of the setter methods.

}
\seealso{
       \code{\link{Pnode}}, \code{\link{PnodeQ}}, 
       \code{\link{PnodeRules}}, \code{\link{PnodeLink}}, 
       \code{\link{PnodeBetas}}, \code{\link{BuildTable}},
       \code{\link{PnodeParentTvals}}, \code{\link{maxCPTParam}}
       \code{\link[CPTtools]{calcDPCTable}}, \code{\link[CPTtools]{mapDPC}}
       \code{\link[CPTtools]{Compensatory}},
       \code{\link[CPTtools]{OffsetConjunctive}}

}
\examples{
\dontrun{
library(PNetica) ## Requires implementation

tNet <- CreateNetwork("TestNet")


theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
NodeProbs(theta1) <- rep(1/NodeNumStates(theta),NodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
NodeProbs(theta2) <- rep(1/NodeNumStates(theta),NodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
NodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

## slopes of 1 for both transitions
PnodeLnAlphas(partial3) <- c(0,0)
BuildTable(partial3)

## log slope 0 = slope 1
stopifnot(
   all(abs(PnodeAlphas(partial3) -1) <.0001)
)

## Make Skill 1 more important than Skill 2
PnodeLnAlphas(partial3) <- c(.25,-.25)
BuildTable(partial3)

## Make Skill 1 more important for the transition to ParitalCredit
## And Skill 2 more important for the transition to FullCredit
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=c(.25,-.25))
BuildTable(partial3)


## Set up so that first skill only needed for first transition, second
## skill for second transition; Adjust alphas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)
BuildTable(partial3)

## Using OffsetConjunctive rule requires single slope
PnodeRules(partial3) <- "OffsetConjunctive"
## Single slope parameter for each transition
PnodeLnAlphas(partial3) <- 0
PnodeQ(partial3) <- TRUE
PnodeBetas(partial3) <- c(0,1)
BuildTable(partial3)

## Separate slope parameter for each transition;  
## Note this will only different from the previous transition when
## mapDPC is called.  In the former case, it will learn a single slope
## parameter, in the latter, it will learn a different slope for each
## transition. 
PnodeLnAlphas(partial3) <- list(0,0)
BuildTable(partial3)


DeleteNetwork(tNet)
}
}
\keyword{ attrib }
