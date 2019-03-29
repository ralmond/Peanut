\name{NodeGadget}
\alias{CompensatoryGadget}
\alias{OffsetGadget}
\alias{RegressionGadget}
\alias{DPCGadget}
\title{Shiny gadget for editing parameterized nodes (PNodes)}
\description{

  These functions open a shiny application (in a browser window) for
  editing a \code{\link{Pnode}} object.  The various functions make
  assumptions about the relevant parameters to reduce unneeded
  complexity. 

}
\usage{
CompensatoryGadget(pnode, color="firebrick")
OffsetGadget(pnode, color="plum")
RegressionGadget(pnode, useR2 = PnodeNumParents(pnode)>0L,
         color="sienna")
DPCGadget(pnode, color="steelblue")
}
\arguments{
  \item{pnode}{A \code{\link{Pnode}} object to be modified.}
  \item{useR2}{Logical value, if true (default for nodes with at least
    one parent), then R-squared will be used instead of the actual link
    scale parameter on the graphical input.}
  \item{color}{A base color to use for barcharts (see
    \code{\link[CPTtools]{barchart.CPF}}).  Execute \code{colors()} for
    a list of choices.}
}
\details{

  Each function puts limits on the number of parameters.

  The \code{CompensatoryGadget} assumes that:
  \itemize{
    \item{The link function is \code{\link[CPTtools]{partialCredit}} or
      \code{\link[CPTtools]{gradedResponse}}.}
    \item{There is a single rule for all states, and 
      \code{\link{PnodeQ}(pnode)=TRUE}.}
    \item{One of the multiple-\emph{a} rules:
      \code{\link[CPTtools]{Compensatory}}, \code{Conjunctive} or
      \code{Disjunctive} is used, so that there is one alpha for each
      parent.}
    \item{There is one beta for each state except the last, which is a
      reference state.}
  }
  It is most useful for compensatory models.

  The \code{OffsetGadget} assumes that:
  \itemize{
    \item{The link function is \code{\link[CPTtools]{partialCredit}} or
      \code{\link[CPTtools]{gradedResponse}} (although the latter only
      works correctly if there are only two states in the child variable.}
    \item{There is a single rule for all states, and 
      \code{\link{PnodeQ}(pnode)=TRUE}.}
  }
  This is most useful for when the \code{pnode} is a proficiency
  variable, as the normal link is the inverse of the discretization used
  by \code{\link{PnodeParentTvals}}.

  

  The \code{RegressionGadget} assumes that:
  \itemize{
    \item{The link function is \code{\link[CPTtools]{normalLink}} and a
      link scale parameter is needed.}
    \item{There is a single rule for all states, and 
      \code{\link{PnodeQ}(pnode)=TRUE}.}
    \item{One of the multiple-\emph{a} rules:
      \code{\link[CPTtools]{Compensatory}}, \code{Conjunctive} or
      \code{Disjunctive} is used, so that there is one alpha for each
      parent. The alphas are called slopes.}
    \item{There is one beta, and the probabilities are controlled by the
      spread. Negative beta is used and called an intercept.}
  }
  The link scale parameter can be specified either directly, or via
  R-squared.  In the no parent case, direct parameter is needed.  In the
  case of multiple parents, the default is to specify the R-squared and
  calculate the link scale based on the slopes and R-squared.  This
  behavior can be overridden with the \code{useR} model.  This gadget
  works for variables with no parents (the others all assume at least
  one parent).

  

}
\value{
  
  If the user presses \dQuote{Done} on the interface, the result is a
  modified version of the final \code{pnode} argument.

  If the user presses \dQuote{Cancel} a \sQuote{Cancel-Error} is raised,
  and \code{pnode} is not modified (even if \code{pnode} is a reference
  class object).
  
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

}
\examples{
\dontrun{
library(PNetica) ## Requires implementation
sess <- NeticaSession()
startSession(sess)

tNet <- CreateNetwork("TestNet",sess)

theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
NodeProbs(theta2) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta2))

## CompensatoryGadget

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
NodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

partial3 <- CompensatoryGadget(partial3)

## OffsetGadget

PnodeRules(partial3) <- "OffsetConjunctive"
## Single slope parameter for each transition
PnodeLnAlphas(partial3) <- 0
PnodeQ(partial3) <- TRUE
PnodeBetas(partial3) <- c(0,1)
BuildTable(partial3)

partial3 <- OffsetGadget(partial3)

## Regression Gadget

PnodeRules(partial3) <- "Compensatory"
PnodeLink(partial3) <- "normalLink"
PnodeLinkScale(partial3) <- 1.0

partial3 <- RegressionGadget(partial3)

## Single parent case
theta2 <- Pnode(theta2,c(),0,link="normalLink",linkScale=1)
theta2 <- RegressionGadget(theta2)

## Complex case with Q-matrix
## Set up so that first skill only needed for first transition, second
## skill for second transition; Adjust alphas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)

partial3 <- DPCGadget(partial3)

}
}
\keyword{ manip }
\keyword{ interface }
