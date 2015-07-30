\name{PnodeParentTvals}
\alias{PnodeParentTvals}
\title{Fetches a list of numeric variables corresponding to parent states}
\description{

  In constructing a conditional probability table using the discrete
  partial credit framework (see \code{\link[CPTtools]{calcDPCTable}}),
  each state of each parent variable is mapped onto a real value called
  the effective theta.  The function
  \code{PnodeParentTvals} returns a list of effective theta values for each
  parent variable. 

}
\usage{
PnodeParentTvals(node)
}
\arguments{
  \item{node}{A \code{\link{Pnode}} object.}
}
\details{

    Following the framework laid out in Almond (2015), the function
  \code{\link[CPTtools]{calcDPCTable}} calculates a conditional
  probability table using the following steps:
  \enumerate{
    \item{Each set of parent variable states is converted to a set of
      continuous values called \emph{effective thetas}.  These are built
      into an array, \code{eTheta}, using
      \code{\link[base]{expand.grid}} where each 
      column represents a parent variable and each row a possible
      configuration of parents.} 

    \item{For each state of the \code{node} except the last,
      the set of effective thetas is filtered using the local Q-matrix,
      \code{\link{PnodeQ}(node) = Q}.  Thus, the actual effect thetas
      for state \code{s} is \code{eTheta[,Q[s,]]}.  The value of
      \code{\link{PnodeRules}(node)} determines which combination
      function is used.}

    \item{For each state of the \code{node} except the last, the
      corresponding rule is applied to the effective thetas to get a
      single effective theta for each row of the table.  Essentially:
      \code{do.call(rules[[s]],list(eThetas[,Q[s,]]),PnodeAlphas(node)[[s]],
        PnodeBetas(node)[[s]])}.}

    \item{The resulting set of effective thetas are converted into
      conditional probabilities using the link function.}
  }

  This function is responsible for the first step of this process.
  \code{PnodeParentTvals(node)} should return a list corresponding to
  the parents of \code{node}, and each element should be a numeric
  vector corresponding to the states of the appropriate parent
  variable.  It is passed to \code{\link[base]{expand.grid}} to produce
  the table of parent variables for each row of the CPT.

  Note that in item response theory, ability (theta) values are assumed
  to have a unit normal distribution in the population of interest.
  Therefore, appropriate tvals are quantiles of the normal
  distribution.  In particular, they should correspond to the marginal
  distribution of the parent variable.  The function
  \code{\link[CPTtools]{effectiveThetas}} produces equally spaced (wrt
  the normal measure) theta values (corresponding to a uniform
  distribution of the parent).  Unequally spaced values can be produced
  by using appropriate values of the \code{\link[stats]{qnorm}}
  function, e.g. \code{qnorm(c(.875,.5,.125))} will produce effective
  thetas corresponding to a marginal distribution of (0.25, 0.5, 0.25)
  (note that each value is in the midpoint of the interval).

}
\value{

  \code{PnodeParentTvals(node)} should return a list corresponding to
  the parents of \code{node}, and each element should be a numeric
  vector corresponding to the states of the appropriate parent
  variable.  If there are no parent variables, this will be a list of no
  elements.    

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

  The function \code{PnodeParentTvals} is an abstract
  generic functions, and need specific implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.

  In particular, it is probably a mistake to using different effective
  theta values for different parent variables in different contexts,
  therefor, the cleanest implementation is to associate the effective
  thetas with the parent variables and simply have
  \code{PnodeParentTvals} fetch them on demand.  Thus the
  implementation in \code{PNetica} is simply,
  \code{lapply(NodeParents(node), NodeLevels)}.


}
\seealso{

       \code{\link{Pnode}}, \code{\link{PnodeQ}}, \code{\link{PnodeRules}}, 
       \code{\link{PnodeLink}}, \code{\link{PnodeLnAlphas}}, 
       \code{\link{PnodeBetas}}, \code{\link{BuildTable}},
       \code{\link{PnodeParentTvals}}, \code{\link{maxCPTParam}}
       \code{\link[CPTtools]{calcDPCTable}}, \code{\link[CPTtools]{mapDPC}}
       \code{\link[CPTtools]{effectiveThetas}},
       \code{\link[base]{expand.grid}}, \code{\link[stats]{qnorm}}

}
\examples{

\dontrun{
library(PNetica) ## Requires implementation

tNet <- CreateNetwork("TestNet")

theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
## This next function sets the effective thetas for theta1
NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("High","Mid","Low"))
## This next function sets the effective thetas for theta2
NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
NodeProbs(theta2) <- rep(1/NodeNumStates(theta2),NodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
NodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")

PnodeParentTvals(partial3)
do.call("expand.grid",PnodeParentTvals(partial3))


DeleteNetwork(tNet)
}
}
\keyword{ attrib }

