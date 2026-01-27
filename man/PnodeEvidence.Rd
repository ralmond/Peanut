\name{PnodeEvidence}
\alias{PnodeEvidence}
\alias{PnodeEvidence<-}
\title{Accesses the value to which a given node has been instantiated.}
\description{

  Inference is a Bayesian network involves setting the state of a
  particular node to one of its possible states, either because the
  state has been observed, or because it has been hypothesized.  This
  processis is often called \emph{instantiaion}.  This function returns
  the value (state) to which the node has been instantiated, or in the
  setter form set it.  Depending on the implementation logic, the beliefs may
  be immediately updated or be updated on demand.

}
\usage{
PnodeEvidence(node)
PnodeEvidence(node) <- value
}
\arguments{
  \item{node}{A \code{\linkS4class{Pnode}} object whose instantiated
    value will be accessed.}
  \item{value}{The value that the node will be instantiated to, see
    details.} 
}
\details{

  Currently, \code{Peanut} supports two ways of representing nodes,
  discrete and continuous (see \code{\link{isPnodeContinuous}}).
  The current \code{\link[PNetica]{PNetica-package}} implemenation
  discritizes continuous nodes, using the \code{\link{PnodeStateBounds}}
  to map real numbers to states of the observables.  Functions
  implementing these generic functions may treat these values
  differently.

  The behavior depends on the class of the \code{value} argument:
  \describe{
    \item{character or factor}{The character of factor should represent
      a state of the node.  The node will be instantiated to that
      state.}
    \item{numeric scalar}{For continuous nodes, the node will be
      instantiated to that value.  For discritized continuous nodes, the
      node will be instantiated to the state in which the value lies
      (see \code{\link{PnodeStateBounds}}).}
    \item{difftime scalar}{The value is first converted to a numeric
      value with units of seconds.  This can be overridden in the
      implementation.}
    \item{numeric vector of length \code{\link{PnodeNumStates}}}{The
      number should represent likelihoods, and this will enter
      appropriate virual evidence for the node.}
    \item{NULL}{This will retract any existing evidence associated with
      the node.}
  }
}
\value{

  The getter function \code{PnodeEvidence} will return one of the value forms
  described in details.  If the node is not instantiated, it will return
  \code{NULL}.

  The setter function \code{PnodeEvidence<-} returns the node argument
  invisibly. 

}
\author{Russell Almond}
\note{

  The current options for this function make a lot of sense with
  Netica.  There may be other modes that are not covered for other
  implementations. 

}
\seealso{

  The function \code{\link{PnetCompile}} usually needs to be run before
  this function has meaning.

  The functions \code{\link{PnodeStates}} and
  \code{\link{PnodeStateBounds}} define the legal values for the value
  argument. 

}
\examples{
\dontrun{

library(PNetica) ## Need a specific implementation
sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(system.file("testnets", "IRT10.2PL.base.dne",
                                        package="PNetica"),
                           session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- PnetFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  
}

BuildAllTables(irt10.base)
PnetCompile(irt10.base) ## Netica requirement

stopifnot (is.na(PnodeEvidence(irt10.items[[1]])))

PnodeEvidence(irt10.items[[1]]) <- "Correct"
stopifnot(PnodeEvidence(irt10.items[[1]])=="Correct")

PnodeEvidence(irt10.items[[1]]) <- NULL
stopifnot (is.na(PnodeEvidence(irt10.items[[1]])))

PnodeEvidence(irt10.items[[1]]) <- c(Correct=.6,Incorrect=.3)
stopifnot(all.equal(PnodeEvidence(irt10.items[[1]]),
                    c(Correct=.6,Incorrect=.3),
                    tol=3*sqrt(.Machine$double.eps) ))

foo <- NewContinuousNode(irt10.base,"foo")

stopifnot(is.na(PnodeEvidence(foo)))

PnodeEvidence(foo) <- 1
stopifnot(PnodeEvidence(foo)==1)

DeleteNetwork(irt10.base)
stopSession(sess)
}
}
\keyword{ graphs }
