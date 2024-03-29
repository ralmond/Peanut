\name{Peanut-package}
\alias{Peanut-package}
\alias{Peanut}
\docType{package}
\title{
\packageTitle{Peanut}
}
\description{
\packageDescription{Peanut}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{Peanut}

Peanut (a corruption of Parameterized network or \code{\link{Pnet}}) is
an object oriented layer on top of the tools for constructing
conditional probability tables for Bayesian networks in the
\code{\link[CPTtools:CPTtools-package]{CPTtools}} package.  In
particular, it defines a \code{\link{Pnode}} (parameterized node) object
which stores all of the arguments necessary to use to the
\code{\link[CPTtools]{calcDPCTable}} function to build the conditional
probability table for the node.

The \code{\link{Pnet}} object is a Bayesian network containing a number
of \code{\link{Pnode}}s.  It supports two key operations,
\code{\link{BuildAllTables}} which sets the values of the conditional
probabilities based on current parameters and \code{\link{GEMfit}} which
adjusts the parameters to match a set of cases.

Like the \code{\link[DBI:DBI-package]{DBI}} package, this class consists
mostly of generic functions which need to be implement for specific
Bayes net implementations.  The package
\code{\link[PNetica:PNetica-package]{PNetica}} provides an implementation
of the \code{Peanut} generic functions using the \code{\link[RNetica]{RNetica}}
package.  All of the Netica-dependent code is isolated in the
\code{PNetica} package, to make it easier to create other implementations.
}

\section{Index}{

\packageIndices{Peanut}

}
\author{
\packageAuthor{Peanut}

Maintainer: \packageMaintainer{Peanut}
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

  Almond, R. G., Mislevy, R. J., Steinberg, L. S., Yan, D. and
  Williamson, D. M. (2015) \emph{Bayesian Networks in Educational
  Assessment}.  Springer.  (ISBN 978-1-4939-2124-9).


}
\keyword{ package }
\keyword{ graphs }
\seealso{
  \describe{
    \item{\code{\link[PNetica:PNetica-package]{PNetica}}}{An
      implementation of the Peanut object model using
      \code{\link[RNetica:RNetica-package]{RNetica}}.} 
    \item{\code{\link[CPTtools:CPTtools-package]{CPTtools}}}{A
      collection of implementation independent Bayes net utilities.}
    }
}
\examples{

\dontrun{
library(PNetica) ## Requires implementation
sess <- NeticaSession()
startSession(sess)

## Building CPTs
tNet <- CreateNetwork("TestNet",session=sess)


theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta1) <- effectiveThetas(NodeNumStates(theta1))
NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta2) <- effectiveThetas(NodeNumStates(theta2))
NodeProbs(theta2) <- rep(1/NodeNumStates(theta2),NodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
PnodeParents(partial3) <- list(theta1,theta2)

partial3 <- Pnode(partial3,Q=TRUE, link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

## Set up so that first skill only needed for first transition, second
## skill for second transition; adjust alphas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)
BuildTable(partial3)
partial4 <- NewDiscreteNode(tNet,"partial4",
                            c("Score4","Score3","Score2","Score1"))
NodeParents(partial4) <- list(theta1,theta2)
partial4 <- Pnode(partial4, link="partialCredit")
PnodePriorWeight(partial4) <- 10

## Skill 1 used for first transition, Skill 2 used for second
## transition, both skills used for the 3rd.

PnodeQ(partial4) <- matrix(c(TRUE,TRUE,
                             FALSE,TRUE,
                             TRUE,FALSE), 3,2, byrow=TRUE)
PnodeLnAlphas(partial4) <- list(Score4=c(.25,.25),
                                Score3=0,
                                Score2=-.25)
BuildTable(partial4)

## Fitting Model to data

irt10.base <- ReadNetworks(system.file("testnets", "IRT10.2PL.base.dne",
                                       package="PNetica"),
                           session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- PnetFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  ## Add node to list of observed nodes
  PnodeLabels(irt10.items[[1]]) <-
     union(PnodeLabels(irt10.items[[1]]),"onodes")

}


casepath <- system.file("testdat", "IRT10.2PL.200.items.cas", package="PNetica")


BuildAllTables(irt10.base)
PnetCompile(irt10.base) ## Netica requirement

item1 <- irt10.items[[1]]
priB <- PnodeBetas(item1)
priA <- PnodeAlphas(item1)
priCPT <- NodeProbs(item1)

gemout <- GEMfit(irt10.base,casepath)


DeleteNetwork(irt10.base)
DeleteNetwork(tNet)
stopSession(sess)
}}
