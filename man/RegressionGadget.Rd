\name{RegressionGadget}
\alias{RegressionGadget}
\alias{MakeRegressionGadget}
\title{Shiny gadget for editinging compensatory pnodes}
\description{

  These functions open a shiny application (in a browser window or other
  location) for editing a \code{\link{Pnode}} object.  To reduce the
  complexity, the display assumes that \code{\link{PnodeLink}(pnode)} is
  \code{\link[CPTtools]{partialCredit}} or
  \code{\link[CPTtools]{gradedResponse}}, and that
  \code{\link{PnodeLink}(pnode)} is \code{\link[CPTtools]{Compensatory}}
  (\code{Conjunctive} or \code{Disjunctive} are also possibilities, but
  usually, the \code{\link{OffsetGadget}} is a better parameterization
  for these rules).
  

}
\usage{
MakeRegressionGadget(pnode, useR2 = PnodeNumParents(pnode) > 0L,
color = "sienna")
RegressionGadget(pnode, useR2 = PnodeNumParents(pnode) > 0L,
color="sienna",viewer=shiny::paneViewer())
}
\arguments{
  \item{pnode}{A \code{\link{Pnode}} object to be modified.}
  \item{color}{A base color to use for barcharts (see
    \code{\link[CPTtools]{barchart.CPF}}).  Execute \code{colors()} for
    a list of choices.}
  \item{useR2}{A logical value.  If true, the link scale parameter is
    convered to an \eqn{R^2} parameter in the gadget.}
  \item{viewer}{This is passed to the \code{viewer} argument of \code{shiny::\link[shiny]{runGadget}}.}
}
\details{

    The \code{RegressionGadget} assumes that:
    \itemize{
    \item{The link function is \code{\link[CPTtools]{normalLink}}.}
    \item{There is a single rule for all states, and that rule is
      \code{\link[CPTtools]{Compensatory}}.}
    \item{There is no inner Q-matrix, i.e., \code{\link{PnodeQ}(pnode)=TRUE}.}
    \item{There is a single beta value.}
    \item{That \code{\link{PnodeLinkScale}(pnode)} has a positive value.}
  }

  This model is essentially a latent regression of the effective thetas
  onto the latent theta of the child space.  The link scale parameter
  gives the residual variance (see \code{\link[CPTtools]{normalLink}}).

  An alternative parameterization, often more familiar to experts, uses
  the multiple \eqn{R^2} instead of the residual variance.  The
  \code{useR2} flag determines how the link scale parameter is conveyed
  on the gadget.


}
\value{
  The function \code{MakeRegressionGadget} returns a list of two
  functions, \code{ui} and \code{server}.  These are meant to be passed
  to \code{shiny::\link[shiny]{runApp}} to generate the actual app.

  The function \code{RegressionGadget} will return the \code{pnode}
  object or throw a \sQuote{Cancel-Error}.

}
\references{
  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\seealso{

  \code{\link{Pnode}}, \code{\link[CPTtools]{calcDPCFrame}},
  \code{\link[CPTtools]{barchart.CPF}}

  \code{\link{OffsetGadget}}, \code{\link{CompensatoryGadget}},
  \code{\link{DPCGadget}}

}
\examples{
\dontrun{
library(PNetica) ## Requires implementation
sess <- NeticaSession()
startSession(sess)

tNet <- CreateNetwork("TestNet",sess)

theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta1) <- effectiveThetas(PnodeNumStates(theta1))
PnodeProbs(theta1) <- rep(1/PnodeNumStates(theta1),PnodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta2) <- effectiveThetas(PnodeNumStates(theta2))
PnodeProbs(theta2) <- rep(1/PnodeNumStates(theta1),PnodeNumStates(theta2))

## RegressionGadget

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
PnodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="normalLink")
PnodePriorWeight(partial3) <- 10
PnodeLinkScale(partial3) <- 1.0
BuildTable(partial3)

partial3 <- RegressionGadget(partial3)
partial3 <- RegressionGadget(partial3,useR2=FALSE)

## This expression can be used inside an Rmarkdown document
gadget <- MakeRegressionGadget(partial3)
shinyApp(gadget$ui,gadget$server,options(height=2000))

## No parent case
theta2 <- Pnode(theta2,link="normalLink",linkScale=1)
theta2 <- RegressionGadget(theta2)


DeleteNetwork(tNet)
stopSession(sess)
}

}
\keyword{ manip }
\keyword{ interface }
\concept{ CPT }
\concept{ normalLink}
\concept{ Compensatory }

