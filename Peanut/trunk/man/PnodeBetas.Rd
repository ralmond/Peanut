\name{PnodeBetas}
\alias{PnodeBetas}
\alias{PnodeBetas<-}
\title{Access the combination function slope parameters for a Pnode}
\description{

  In constructing a conditional probability table using the discrete
  partial credit framework (see \code{\link[CPTtools]{calcDPCTable}}),
  the effective thetas for each parent variable are combined into a
  single effect theta using a combination rule.  The expression
  \code{PnodeAlphas(node)} accesses the intercept parameters associated with
  the combination function \code{\link{PnodeRules}(node)}.    


}
\usage{
PnodeBetas(node)
PnodeBetas(node) <- value

}
\arguments{
  \item{node}{A \code{\link{Pnode}} object.}
  \item{value}{A numeric vector of intercept parameters or a list of
    such vectors (see details).  The length of the vector depends on the
    combination rules (see \code{\link{PnodeRules}}).  If
    a list, it should have length one less than the number of states in
    \code{node}.}
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
  \eqn{beta}.  Let \eqn{theta_{i,j}} be the effective theta value for the
  \eqn{j}th parent variable on the \eqn{i}th row of the effective theta
  table, and let \eqn{beta_{j}} be the corresponding slope parameter.
  Then the effective theta for that row is:
  \deqn{Z(theta_{i}) =
    (alpha_1theta_{i,1}+\ldots+alpha_Jtheta_{J,1})/C - beta ,}
  where \eqn{C=\sqrt(J)} is a variance stabilization constant and
  \eqn{alpha}s are derived from \code{\link{PnodeAlphas}}.  The
  functions \code{\link[CPTtools]{Conjunctive}} and
  \code{\link[CPTtools]{Disjunctive}} are similar replacing the sum with
  a min or max respectively.

  In general, when the rule is one of
  \code{\link[CPTtools]{Compensatory}}, 
  \code{\link[CPTtools]{Conjunctive}}, or
  \code{\link[CPTtools]{Disjunctive}}, the the value of
  \code{PnodeBetas(node)} should be a scalar.

    The rules \code{\link[CPTtools]{OffsetConjunctive}}, and
  \code{\link[CPTtools]{OffsetDisjunctive}}, work somewhat differently,
  in that they assume there is a single slope and multiple intercepts.
  Thus, the \code{OffsetConjunctive} has equation:
  \deqn{Z(theta_{i}) = alpha min(theta_{i,1}-beta_1,
    \ldots,theta_{J,1}-beta_{J}) .}
  In this case the assumption is that \code{\link{PnodeAlphas}(node)} will be a 
  scalar and \code{PnodeBetas(node)} will be a vector of length
  equal to the number of parents.  As a special case, if it is a vector
  of length 1, then a model with a common slope is used.  This looks the
  same in \code{\link[CPTtools]{calcDPCTable}} but has a different
  implication in \code{\link[CPTtools]{mapDPC}} where the parameters are
  constrained to be the same.
  
  When \code{node} has more than two states, there is a a different
  combination function for each transition.  (Note that
  \code{\link[CPTtools]{calcDPCTable}} assumes that the states are
  ordered from highest to lowest, and the transition functions 
  represent transition to the corresponding state, in order.)  There are
  always one fewer transitions than there states.  The meaning of the
  transition functions is determined by the the value of
  \code{\link{PnodeLink}}, however, both the
  \code{\link[CPTtools]{partialCredit}} and the 
  \code{\link[CPTtools]{gradedResponse}} link functions allow for
  different intercepts for the different steps, and the
  \code{gradedResponse} link function requires that the intercepts be in
  decreasing order (highest first).  To get a different intercept for
  each transition, the value of \code{PnodeBetas(node)} should be a
  list. 


  If the value of \code{PnodeRules(node)} is a list, then a different
  combination rule is used for each transition.  Potentially, this could
  require a different number of intercept parameters for each row.
  Also, if the value of \code{\link{PnodeQ}(node)} is not a matrix
  of all \code{TRUE} values, then the effective number of parents for
  each state transition could be different.  In this case, if the
  \code{\link{OffsetConjunctive}} or \code{\link{OffsetDisjunctive}}
  rule is used the value of \code{PnodeBetas(node)} should be a list of
  vectors of different lengths (corresponding to the number of true
  entries in each row of \code{\link{PnodeQ}(node)}).

}
\value{

  A list of numeric vectors giving the intercepts for the combination
  function of each state transition.  The vectors may be of different
  lengths depending on the value of \code{\link{PnodeRules}(node)} and
  \code{\link{PnodeQ}(node)}.  If the intercepts are the same for all
  transitions then a single numeric vector instead of a list is
  returned.

  Note that the setter form may destructively modify the Pnode object
  (this depends on the implementation).

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
 
  The functions \code{PnodeLnBetas} and \code{PnodeLnBetas<-} are
  abstract generic functions, and need specific implementations.  See
  the \code{\link[PNetica]{PNetica-package}} for an example.

  The values of \code{\link{PnodeLink}}, \code{\link{PnodeRules}},
  \code{\link{PnodeQ}},  \code{\link{PnodeParentTvals}},
  \code{\link{PnodeLnAlphas}},  and \code{\link{PnodeBetas}} all need to
  be consistent for this to work correctly, but no error checking is
  done on any of the setter methods.

}
\seealso{
       \code{\link{Pnode}}, \code{\link{PnodeQ}}, 
       \code{\link{PnodeRules}}, \code{\link{PnodeLink}}, 
       \code{\link{PnodeLnAlphas}}, \code{\link{BuildTable}},
       \code{\link{PnodeParentTvals}}, \code{\link{maxCPTParam}}
       \code{\link[CPTtools]{calcDPCTable}}, \code{\link[CPTtools]{mapDPC}}
       \code{\link[CPTtools]{Compensatory}},
       \code{\link[CPTtools]{OffsetConjunctive}}

}
\examples{
\dontrun{
library(PNetica) ## Requires implementation
sess <- NeticaSession()
startSession(sess)

tNet <- CreateNetwork("TestNet",session=sess)


theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta1) <- effectiveThetas(PnodeNumStates(theta1))
PnodeProbs(theta1) <- rep(1/PnodeNumStates(theta1),PnodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta2) <- effectiveThetas(PnodeNumStates(theta2))
PnodeProbs(theta2) <- rep(1/PnodeNumStates(theta2),PnodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
PnodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="gradedResponse")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)


## increasing intercepts for both transitions
PnodeBetas(partial3) <- list(FullCredit=1,PartialCredit=0)
BuildTable(partial3)

stopifnot(
   all(abs(do.call("c",PnodeBetas(partial3)) -c(1,0) ) <.0001)
)


## increasing intercepts for both transitions
PnodeLink(partial3) <- "partialCredit"
## Full Credit is still rarer than partial credit under the partial
## credit model
PnodeBetas(partial3) <- list(FullCredit=0,PartialCredit=0)
BuildTable(partial3)


stopifnot(
   all(abs(do.call("c",PnodeBetas(partial3)) -c(0,0) ) <.0001)
)

## Switch to rules which use multiple intercepts
PnodeRules(partial3) <- "OffsetConjunctive"

## Make Skill 1 more important for the transition to ParitalCredit
## And Skill 2 more important for the transition to FullCredit
PnodeLnAlphas(partial3) <- 0
PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=c(.25,-.25))
BuildTable(partial3)


## Set up so that first skill only needed for first transition, second
## skill for second transition; Adjust betas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)
BuildTable(partial3)


## Can also do this with special parameter values
PnodeQ(partial3) <- TRUE
PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=c(0,Inf))
BuildTable(partial3)


DeleteNetwork(tNet)
stopSession(sess)
}
}
\keyword{ attrib }
