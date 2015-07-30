\name{PnodeRules}
\alias{PnodeRules}
\alias{PnodeRules<-}
\title{Accesses the combination rules for a Pnode}
\description{

  In constructing a conditional probability table using the discrete
  partial credit framework (see \code{\link[CPTtools]{calcDPCTable}}),
  the effective thetas for each parent variable are combined into a
  single effect theta using a combination rule.  The function
  \code{PnodeRules} accesses the combination function associated with a
  \code{\link{Pnode}}.  

}
\usage{
PnodeRules(node)
PnodeRules(node) <- value
}
\arguments{
  \item{node}{A \code{\link{Pnode}} object.}
  \item{value}{The name of a combination function, the combination
    function or a list of names/combination functions (see details).  If
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
      single effective theta for each row of the table.  Essentially:
      \code{do.call(rules[[s]],list(eThetas[,Q[s,]]),PnodeAlphas(node)[[s]],
        PnodeBetas(node)[[s]])}.}

    \item{The resulting set of effective thetas are converted into
      conditional probabilities using the link function
      \code{\link{PnodeLink}(node)}.}
  }
      
  The function \code{PnodeRules} accesses the function used in step 3.
  It should should be the name of a function or a function with the
  general signature of a combination function described in
  \code{\link[CPTtools]{Compensatory}}.  Predefined choices include
  \code{\link[CPTtools]{Compensatory}},
  \code{\link[CPTtools]{Conjunctive}},
  \code{\link[CPTtools]{Disjunctive}}, 
  \code{\link[CPTtools]{OffsetConjunctive}}, and
  \code{\link[CPTtools]{OffsetDisjunctive}}.  Note that the first three
  choices expect that there will be multiple alphas, one for each
  parent, and the latter two expect that there will be multiple betas,
  one for each beta.  The value of \code{\link{PnodeAlphas}} and
  \code{\link{PnodeBetas}} should be set to match.

  If the value of \code{\link{PnodeLink}} is
  \code{\link[CPTtools]{partialCredit}}, then the link function can be
  different for state of the \code{node}.  (If it is
  \code{\link[CPTtools]{gradedResponse}} then the curves need to be
  parallel and it should be the same.)  If the value of
  \code{PnodeRules(node)} is a list (note:  list, not character vector),
  then a different rule is used for each state transition.  The function
  \code{\link[CPTtools]{calcDPCTable}} assumes the states are ordered
  from highest to lowest, and no transition is needed into the lowest
  state. 

}
\value{

  A character scalar giving the name of a combination function or a
  combination function object, or a list of the same.  If a list, its
  length is one less than the number of states of \code{node}.
  
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

  The functions \code{PnodeRules} and \code{PnodeRules<-} are  abstract
  generic functions, and need specific implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.

  The values of \code{\link{PnodeLink}}, \code{\link{PnodeRules}},
  \code{\link{PnodeQ}},  \code{\link{PnodeParentTvals}},
  \code{\link{PnodeLnAlphas}},  and \code{\link{PnodeBetas}} all need to
  be consistent for this to work correctly, but no error checking is
  done on any of the setter methods.

}
\seealso{
       \code{\link{Pnode}}, \code{\link{PnodeQ}}, 
       \code{\link{PnodeLink}}, \code{\link{PnodeLnAlphas}}, 
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
NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
NodeProbs(theta2) <- rep(1/NodeNumStates(theta2),NodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
NodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

stopifnot(
   PnodeRules(partial3) == "Compensatory"
)

## Use different rules for different levels
## Compensatory for 2nd transition, conjunctive for 1st
## Note:  Position is important, names are just for documentation. 
PnodeRules(partial3) <- list(FullCredit="Compensatory",
                             PartialCredit="Conjunctive")
BuildTable(partial3)

DeleteNetwork(tNet)
}
}
\keyword{ attrib }
