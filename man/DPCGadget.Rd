\name{DPCGadget}
\alias{DPCGadget}
\alias{MakeDPCGadget}
\title{Shiny gadget for editinging compensatory pnodes}
\description{

  These functions open a shiny application (in a browser window or other
  location) for editing a \code{\link{Pnode}} object.  This is the most
  complex version taking advantage of all of the complex choices
  available using the partial credit link (i.e.,
  \code{\link{PnodeLink}(pnode)} is
  \code{\link[CPTtools]{partialCredit}}).  In particular, it allows 
  specification of different combination rules for each state
  transition.  The rules can be either the multiple-\emph{a} type (e.g.,
  \code{\link[CPTtools]{Compensatory}}) or muliptle-\emph{b} type (e.g.,
  \code{\link[CPTtools]{OffsetConjunctive}} or
  \code{\link[CPTtools]{OffsetDisjunctive}}).  It even allows for a
  subset of parents to be used in each transition by specifying an inner
  \emph{Q}-matrix (though \code{\link{PnodeQ}}).  To manage this
  complexity, each transition is displayed on a separate tab of the
  interface.  
  

}
\usage{
MakeDPCGadget(pnode, color = "steelblue")
DPCGadget(pnode, color="steelblue",viewer=shiny::paneViewer())
}
\arguments{
  \item{pnode}{A \code{\link{Pnode}} object to be modified.}
  \item{color}{A base color to use for barcharts (see
    \code{\link[CPTtools]{barchart.CPF}}).  Execute \code{colors()} for
    a list of choices.}
  \item{viewer}{This is passed to the \code{viewer} argument of \code{shiny::\link[shiny]{runGadget}}.}
}
\details{

    The \code{DPCGadget} assumes that:
    \itemize{
    \item{The link function is \code{\link[CPTtools]{partialCredit}}.}
    \item{There is a list of rules, one for each state transition (i.e.,
      one for all states except the last and lowest value.  Alternatively,
      if a single value is given it is used for all transitions.}
    \item{The value of \code{\link{PnodeQ}(pnode)} is a logical matrix
      with rows corresponding to state transitions and columns
      corresponding to parent variables.  If any cell value is false,
      that parent variable is not used to calculate the effective theta
      for that cell transition.  As a special case, if
      \code{\link{PnodeQ}(pnode)} equals \code{TRUE}, then it is
      considered to be a matrix with all elements true.}
  }
  
  Both \code{\link{PnodeAlphas}(node)} and
  \code{\link{PnodeBetas}(node)} should be a list of vectors.  The
  length of each vector should be either one or the number of relevant
  (after filtering with the inner \emph{Q}-matrix) parent variables.
  Which it needs to be depends on whether the rule for that transition
  is a multiple-\emph{a} type (alphas should match number of parents) or
  multiple-\emph{b} type (betas should match number of parents).  In
  either case, if a single value is given and a longer list is expected,
  it will be replicated across the parents.  The length of the lists
  should match the state transitions (with the first one corresponding
  to the  transition from the 2nd highest state to the highest, the
  second the transition to the 2nd highest state and so forth).  No
  entry is needed for the lowest state.  Once again, if a single vector
  is given in place of the list, it will be replicated as needed.

  To recap, the outer (list) structure corresponds to state transitions
  and the inner (vector) structure corresponds to the parent variables.
  It is recommended to use labeled vectors and lists to annotate the
  structure. 

}
\value{
  The function \code{MakeDPCGadget} returns a list of two
  functions, \code{ui} and \code{server}.  These are meant to be passed
  to \code{shiny::\link[shiny]{runApp}} to generate the actual app.

  The function \code{DPCGadget} will return the \code{pnode}
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

  \code{\link{OffsetGadget}}, \code{\link{RegressionGadget}},
  \code{\link{CompensatoryGadget}}

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

## DPCGadget

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
PnodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

partial3 <- DPCGadget(partial3)

## This expression can be used inside an Rmarkdown document
gadget <- MakeDPCGadget(partial3)
shinyApp(gadget$ui,gadget$server,options(height=2000))

## More complex example showing off some of the options:
## Set up so that first skill only needed for first transition, second
## skill for second transition; Adjust alphas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeLnAlphas(partial3) <- list(FullCredit=c(theta1=-.25,theta2=.25),
                                PartialCredit=0)

partial3 <- DPCGadget(partial3)


DeleteNetwork(tNet)
stopSession(sess)
}

}
\keyword{ manip }
\keyword{ interface }
\concept{ CPT }
\concept{ Compensatory }
\concept{ Conjuctive }
\concept{ Disjunctive }
\concept{ gradedResponse }
\concept{ partialCredit }

