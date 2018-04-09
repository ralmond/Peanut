\name{PnodeLinkScale}
\alias{PnodeLinkScale}
\alias{PnodeLinkScale<-}
\title{Accesses the link function scale parameter associated with a Pnode}
\description{

  In constructing a conditional probability table using the discrete
  partial credit framework (see \code{\link[CPTtools]{calcDPCTable}}),
  the effective thetas for each row of the table is converted into a
  vector of probabilities using the link function.  The function
  \code{PnodeLink} accesses the scale parameter of the link function
  associated with a \code{\link{Pnode}}.

}
\usage{
PnodeLinkScale(node)
PnodeLinkScale(node) <- value
}
\arguments{
  \item{node}{A \code{\link{Pnode}} object.}
  \item{value}{A positive numeric value, or \code{NULL} if the scale
    parameter is not used for the link function.}

}
\details{

  The link function used in constructing the conditional probability
  table is controlled by the value of \code{\link{PnodeLink}(node)}.  One
  of the arguments to the link function is a scale parameter, the
  expression \code{PnodeLinkScale(node)} provides the link scale
  parameter associated with the node.

  This is mostly for future expansion.  Currently, neither of the  two
  link functions defined in the \code{CPTtools} package,
  \code{\link[CPTtools]{partialCredit}}  and
  \code{\link[CPTtools]{gradedResponse}}, require a link scale
  parameter.  However, the DiBello-normal model
  (see \code{\link[CPTtools]{calcDNTable}}) uses a link scale parameter
  so it may be useful in the future.


}
\value{

  The value of the link scale parameter, or \code{NULL} if it is not
  needed.

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

  The functions \code{PnodeLinkScale} and \code{PnodeLinkScale<-} are
  abstract generic functions, and need specific implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.  Even though
  they are not currently used, they must be defined and return a value
  (even just \code{NULL}).

  A third normal link function, which would use the scale parameter, is
  planned but not yet implemented.  

}

\seealso{
       \code{\link{Pnode}}, \code{\link{PnodeQ}},
       \code{\link{PnodeRules}}, 
       \code{\link{PnodeLinkScale}}, \code{\link{PnodeLnAlphas}}, 
       \code{\link{PnodeBetas}}, \code{\link{BuildTable}}, 
       \code{\link{PnodeParentTvals}}, \code{\link{maxCPTParam}}, 
       \code{\link[CPTtools]{calcDPCTable}},
       \code{\link[CPTtools]{mapDPC}}, 
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
partial3 <- Pnode(partial3,rules="Compensatory", link="gradedResponse")
PnodePriorWeight(partial3) <- 10

stopifnot(
  is.null(PnodeLinkScale(partial3))
)

PnodeLinkScale(partial3) <- 1.0

stopifnot(
  all(abs(PnodeLinkScale(partial3)-1)<.0001)
)

DeleteNetwork(tNet)
}
}
\keyword{ attrib }
