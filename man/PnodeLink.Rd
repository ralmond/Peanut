\name{PnodeLink}
\alias{PnodeLink}
\alias{PnodeLink<-}
\title{Accesses the link function associated with a Pnode}
\description{

  In constructing a conditional probability table using the discrete
  partial credit framework (see \code{\link[CPTtools]{calcDPCTable}}),
  the effective thetas for each row of the table is converted into a
  vector of probabilities using the link function.  The function
  \code{PnodeLink} accesses the link function associated with a
  \code{\link{Pnode}}.  


}
\usage{
PnodeLink(node)
PnodeLink(node) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{A \code{\link{Pnode}} object.}
  \item{value}{The name of a link function or function object which can
    serve as the link function.}

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
      conditional probabilities using the link function.}
  }

  A link function is a function of three arguments.  The first is a
  matrix of effective theta values with number of rows equal to the
  number of rows of the conditional probability matrix and number of
  columns equal to the number of states of \code{node} minus one
  (ordered from highest to lowest).  The second is an optional link
  scale, the third is a set of names for the states which is used to
  give column names to the output matrix.  The second and third both
  default to \code{NULL}.
  
  Currently two link functions are \code{\link[CPTtools]{partialCredit}}
  and \code{\link[CPTtools]{gradedResponse}}.  Note that the function
  \code{gradedResponse} assumes that the effective thetas in each row
  are in increasing order.  This puts certain restrictions on the
  parameter values.  Generally, the can only be guaranteed if each state
  of the variable uses the same combination rules (see
  \code{\link{PnodeRules}(node)}), slope parameters (see
  \code{\link{PnodeAlphas}(node)}) and Q-matrix (see
  \code{\link{PnodeQ}(node)}).  Also, the intercepts (see
  \code{\link{PnodeBetas}(node)}) should be in decreasing order. The
  \code{partialCredit} model has fewer restrictions.

  The value of \code{\link{PnodeLinkScale}(node)} is fed to the link
  function.  Currently, this is unused; but the DiBello-normal model
  (see \code{\link[CPTtools]{calcDNTable}}) uses it.  So the link scale
  parameter is for
  future expansion.
  

}
\value{
  A character scalar giving the name of a combination function or a
  combination function object.

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

  The functions \code{PnodeLink} and \code{PnodeLink<-} are  abstract
  generic functions, and need specific implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.

  A third normal link function, which would use the scale parameter, is
  planned but not yet implemented.  
  
}
\seealso{
       \code{\link{Pnode}}, \code{\link{PnodeQ}}, \code{\link{PnodeRules}}
       \code{\link{PnodeLinkScale}}, \code{\link{PnodeLnAlphas}}, 
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

## Usual way to set link is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="gradedResponse")
PnodePriorWeight(partial3) <- 10
PnodeBetas(partial3) <- list(FullCredit=1,PartialCredit=0)
BuildTable(partial3)

## increasing intercepts for both transitions
PnodeLink(partial3) <- "partialCredit"
## Full Credit is still rarer than partial credit under the partial
## credit model
PnodeBetas(partial3) <- list(FullCredit=0,PartialCredit=0)
BuildTable(partial3)

## Can use different slopes with partial credit
## Make Skill 1 more important for the transition to ParitalCredit
## And Skill 2 more important for the transition to FullCredit
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=c(.25,-.25))
BuildTable(partial3)

## Can also use Q-matrix to select skills
## Set up so that first skill only needed for first transition, second
## skill for second transition; Adjust alphas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)
BuildTable(partial3)

DeleteNetwork(tNet)
}
}
\keyword{ attrib }
