\name{CompensatoryGadget}
\alias{CompensatoryGadget}
\alias{MakeCompensatoryGadget}
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
MakeCompensatoryGadget(pnode, color = "firebrick")
CompensatoryGadget(pnode, color="firebrick",viewer=shiny::paneViewer())
}
\arguments{
  \item{pnode}{A \code{\link{Pnode}} object to be modified.}
  \item{color}{A base color to use for barcharts (see
    \code{\link[CPTtools]{barchart.CPF}}).  Execute \code{colors()} for
    a list of choices.}
  \item{viewer}{This is passed to the \code{viewer} argument of \code{shiny::\link[shiny]{runGadget}}.}
}
\details{

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


}
\value{
  The function \code{MakeCompensatoryGadget} returns a list of two
  functions, \code{ui} and \code{server}.  These are meant to be passed
  to \code{shiny::\link[shiny]{runApp}} to generate the actual app.

  The function \code{CompensatoryGadget} will return the \code{pnode}
  object or throw a \sQuote{Cancel-Error}.

}
\references{
  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\note{

  Although the addition of the `MakeCompensatoryGadget` was specifically
  designed to allow the embedding of the gadget in Rmarkdown shiny
  documents, the default Rmarkdown layout algorithms do not work
  properly.

  Adding the following markdown code:
  \preformatted{```{css, echo = FALSE}
.shiny-frame{height: 1000px;}
```  
   } 
  will adjust the space allocated for the gadget to 1000 pixels,
  allowing for sufficient room for display.

}
\author{Russell Almond}
\seealso{

  \code{\link{Pnode}}, \code{\link[CPTtools]{calcDPCFrame}},
  \code{\link[CPTtools]{barchart.CPF}}

  \code{\link{OffsetGadget}}, \code{\link{RegressionGadget}},
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

## CompensatoryGadget

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
PnodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

partial3 <- CompensatoryGadget(partial3)

## This expression can be used inside an Rmarkdown document
gadget <- MakeCompensatoryGadget(partial3)
shinyApp(gadget$ui,gadget$server,options(height=2000))

DeleteNetwork(tNet)
stopSession(sess)
}

}
\keyword{ manip }
\keyword{ interface }
\concept{ CPT }
\concept{ Compensatory }
\concept{ partialCredit }
\concept{ gradedResponse }

