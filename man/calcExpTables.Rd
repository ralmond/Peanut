\name{calcExpTables}
\alias{calcExpTables}
\title{Calculate expected tables for a parameterized network}
\description{

  The performs the E-step of the GEM algorithm by running the internal
  EM algorithm of the host Bayes net package on the \code{cases}.  After
  this is run, the posterior parameters for each conditional probability
  distribution should be the expected cell counts, that is the expected
  value of the sufficient statistic, for each \code{\link{Pnode}} in the
  \code{net}.  

}
\usage{
calcExpTables(net, cases, Estepit = 1, tol = sqrt(.Machine$double.eps))
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object}
  \item{cases}{An object representing a set of cases.  Note the type of
    object is implementation dependent.  It could be either a data frame
    providing cases or a filename for a case file.}
  \item{Estepit}{An integer scalar describing the number of steps the
    Bayes net package should take in the internal EM algorithm.}
  \item{tol}{A numeric scalar giving the stopping tolerance for the
    Bayes net package internal EM algorithm.}
}
\details{

  The \code{\link{GEMfit}} algorithm uses a generalized EM algorithm to
  fit the parameterized network to the given data.  This loops over the
  following steps:
  \describe{
    \item{E-step}{Run the internal EM algorithm of the Bayes net package
      to calculate expected tables for all of the tables being learned.
      The function \code{calcExpTables} carries out this step.}
    \item{M-step}{Find a set of table parameters which maximize the fit
      to the expected counts by calling \code{\link[CPTtools]{mapDPC}}
      for each table.  The function \code{\link{maxAllTableParams}} does
      this step.}
    \item{Update CPTs}{Set all the conditional probability tables in the
      network to the new parameter values.  The function
      \code{\link{BuildAllTables}} does this.}
    \item{Convergence Test}{Calculate the log likelihood of the
      \code{cases} under the new parameters and stop if no change.  The
      function \code{\link{calcPnetLLike}} calculates the log likelihood.}
  }

  The function \code{calcExpTables} performs the E-step.  It assumes
  that the native Bayes net class which \code{net} represents has a
  function which does EM learning with hyper-Dirichlet priors.  After
  this internal EM algorithm is run, then the posterior should contain
  the expected cell counts that are the expected value of the sufficient
  statistics, i.e., the output of the E-step.  Note that the function
  \code{\link{maxAllTableParams}} is responsible for reading these from
  the network.

  The internal EM algorithm should be set to use the current value of
  the conditional probability tables (as calculated by
  \code{\link{BuildTable}(node)} for each node) as a starting point.
  This starting value is given a prior weight of
  \code{\link{GetPriorWeight}(node)}.  Note that some Bayes net
  implementations allow a different weight to be given to each row of
  the table.  The prior weight counts as a number of cases, and should
  be scaled appropriately for the number of cases in \code{cases}.  

  The parameters \code{Estepit} and \code{tol} are passed to the
  internal EM algorithm of the Bayes net.  Note that the outer EM
  algorithm assumes that the expected table counts given the current
  values of the parameters, so the default value of one is sufficient.
  (It is possible that a higher value will speed up convergence, the
  parameter is left open for experimentation.)  The tolerance is largely
  irrelevant as the outer EM algorithm does the tolerance test.

}
\value{

  The \code{net} argument is returned invisibly.

  As a side effect, the internal conditional probability tables in the
  network are updated as are the internal weights given to each row of
  the conditional probability tables.
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\note{

  The function \code{calcExpTables} is an abstract generic functions, 
  and it needs specific implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.

  This function assumes that the host Bayes net implementation (e.g.,
  \code{\link[RNetica]{RNetica-package}}):  (1) \code{net} has an EM
  learning function, (2) the EM learning supports hyper-Dirichlet
  priors, (3) it is possible to recover the hyper-Dirichlet posteriors
  after running the internal EM algorithm.
  
}
\seealso{
  \code{\link{Pnet}}, \code{\link{GEMfit}}, \code{\link{calcPnetLLike}},
  \code{\link{maxAllTableParams}}

}
\examples{
\dontrun{

library(PNetica) ## Need a specific implementation
sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(system.file("testnets","IRT10.2PL.base.dne",
                                       package="Peanut"),
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
PnetCompile(irt10.base) ## Netica requirement

casepath <- system.file("testdat","IRT10.2PL.200.items.cas", package="PNetica")


item1 <- irt10.items[[1]]

priorcounts <- sweep(PnodeProbs(item1),1,GetPriorWeight(item1),"*")

calcExpTables(irt10.base,casepath)

postcounts <- sweep(PnodeProbs(item1),1,PnodePostWeight(item1),"*")

## Posterior row sums should always be larger.
stopifnot(
  all(apply(postcounts,1,sum) >= apply(priorcounts,1,sum))
)

DeleteNetwork(irt10.base)
stopSession(sess)
}

}
\keyword{ manip }
