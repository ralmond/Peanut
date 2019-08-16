\name{calcPnetLLike}
\alias{calcPnetLLike}
\title{Calculates the log likelihood for a set of data under a Pnet model}
\description{

  The function \code{calcPnetLLike} calculates the log likelihood for a
  set of data contained in \code{cases} using the current values of the
  conditional probability table in a \code{\link{Pnet}}.  If it is
  called after a call to \code{\link{BuildAllTables}(net)} this will be
  the current value of the parameters.

}
\usage{
calcPnetLLike(net, cases)
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object representing a parameterized network.}
  \item{cases}{An object representing a set of cases.  Note the type of
    object is implementation dependent.  It could be either a data frame
    providing cases or a filename for a case file.}
}
\details{

  This function provides the convergence test for the
  \code{\link{GEMfit}} algorithm.  The \code{Pnet} represents a model
  (with parameters set to the value used in the current iteration of the
  EM algorithm) and \code{cases} a set of data.  This function gives the
  log likelihood of the data.

  This is a generic function shell.  It is assumed that either (a) the
  native Bayes net implementation provides a way of calculating the log
  likelihood of a set of cases, or (b) it provides a way of calculating
  the likelihood of a single case, and the log likelihood of the case
  set can be calculated though iteration.  In either case, the value of
  \code{cases} is implementation dependent.  In
  \code{\link[PNetica]{PNetica-package}} the \code{cases} argument
  should be a filename of a Netica case file (see
  \code{\link[RNetica]{write.CaseFile}}).

}
\value{

  A numeric scalar giving the log likelihood of the data in the case
  file.
  
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\note{

  The function \code{calcPnetLLike} is an abstract generic functions,
  and it needs specific implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.

}
\seealso{

  \code{\link{Pnet}}, \code{\link{GEMfit}}, \code{\link{calcExpTables}},
  \code{\link{maxAllTableParams}}

}
\examples{

\dontrun{

library(PNetica) ## Need a specific implementation
sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne",
                           sep=.Platform$file.sep),
                           session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- NetworkFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
}
CompileNetwork(irt10.base) ## Netica requirement

casepath <- paste(library(help="PNetica")$path,
                           "testdat","IRT10.2PL.200.items.cas",
                           sep=.Platform$file.sep)
## Record which nodes in the casefile we should pay attention to
NetworkNodesInSet(irt10.base,"onodes") <-
   NetworkNodesInSet(irt10.base,"observables")

llike <- calcPnetLLike(irt10.base,casepath)

DeleteNetwork(irt10.base)

}
}
\keyword{ graphs }

