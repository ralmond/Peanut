\name{GEMfit}
\alias{GEMfit}
\title{Fits the parameters of a parameterized network using the GEM algorithm}
\description{

  A \code{\link{Pnet}} is a description of a parameterized Bayesian
  network, with each \code{\link{Pnode}} giving the parameterization for
  its conditional probabilty table.  This function uses a generalized EM
  algorithm to find the values of the parameters for each \code{Pnode}
  which maximize the posterior probability of the data in \code{cases}.  

}
\usage{
GEMfit(net, cases, tol = sqrt(.Machine$double.eps),
       maxit = 100, Estepit = 1, Mstepit = 3)
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object}
  \item{cases}{An object representing a set of cases.  Note the type of
    object is implementation dependent.  It could be either a data frame
    providing cases or a filename for a case file.}
  \item{tol}{A numeric scalar giving the stopping tolerance for the
    for the EM algorithm.}
  \item{maxit}{An integer scalar giving the maximum number of iterations
    for the outer EM algorithm.}
  \item{Estepit}{An integer scalar giving the number of steps the
    Bayes net package should take in the internal EM algorithm during
    the E-step.} 
  \item{Mstepit}{An integer scalar giving the number of steps that
    should be taken by \code{\link[CPTtools]{mapDPC}} during the M-step.}
}
\details{

  The \code{\link{GEMfit}} algorithm uses a generalized EM algorithm to
  fit the parameterized network to the given data.  This loops over the
  following steps:
  \describe{
    \item{E-step}{Run the internal EM algorithm of the Bayes net package
      to calculate expected tables for all of the tables being learned.
      The function \code{\link{calcExpTables}} carries out this step.}
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

  Note that although this is not a generic function, the four main
  component functions:  \code{\link{calcExpTables}},
  \code{\link{maxAllTableParams}}, \code{\link{BuildAllTables}}, and
  \code{\link{calcPnetLLike}}.   In particular, the \code{cases}
  argument is passed to \code{\link{calcExpTables}} and
  \code{\link{calcPnetLLike}} and must be whatever the host Bayes net
  package regards as a collection of cases.  In
  \code{\link[PNetica]{PNetica-package}} the \code{cases} argument
  should be a filename of a Netica case file (see
  \code{\link[RNetica]{write.CaseFile}}).

  The parameter \code{tol} controls the convergence checking.  In
  particular, the algorithm stops when the difference in log-likelihood
  (as computed by \code{\link{calcPnetLLike}}) between iterations is
  less than \code{tol} in absolute value.  If the number of iterations
  exceeds \code{maxit} the algorithm will stop and report lack of
  convergence.  
  
  The E-step and the M-step are also both iterative; the parameters
  \code{Estepit} and \code{Mstepit} control the number of iterations
  taken in each step respectively.  As the goal of the E-step is to
  calculate the expected tables of counts, the default value of 1 should
  be fine.  Although the algorithm should eventually converge for any
  value of \code{Mstepit}, different values may affect the convergence
  rate, and analysts may need to experiement with application specific
  values of this parameter.
  
}
\value{

  A list with three elements:
  \item{converged }{A logical flag indicating whether or not the
    algorithm reached convergence.}
  \item{iter }{An integer scalare giving the number of iterations of the
    outer EM loop taken by the alorithm (plus 1 for the starting point).}
  \item{llikes }{A numeric vector of length \code{iter} giving the
    values of the log-likelihood after each iteration.  (The first value
    is the initial log likelihood.)}


  As a side effect the \code{\link{PnodeLnAlphas}} and
  \code{\link{PnodeBetas}} fields of all nodes in
  \code{\link{PnetPnodes}(net)}) are updated to better fit the expected
  tables, and the internal conditional probability tables are updated to
  match the new parameter values.

}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\note{

  Note that although this is not a generic function, the four main
  component functions:  \code{\link{calcExpTables}},
  \code{\link{maxAllTableParams}}, \code{\link{BuildAllTables}}, and
  \code{\link{calcPnetLLike}}.   All four must have specific
  implementations for this function to work.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.

  These funcitons assume that the host Bayes net implementation (e.g.,
  \code{\link[RNetica]{RNetica-package}}):  (1) \code{net} has an EM
  learning function, (2) the EM learning supports hyper-Dirichlet
  priors, (3) it is possible to recover the hyper-Dirichlet posteriors
  after running the internal EM algorithm.

}
\seealso{

  \code{\link{Pnet}}, \code{\link{calcExpTables}}, \code{\link{calcPnetLLike}},
  \code{\link{maxAllTableParams}}, \code{\link{BuildAllTables}}

}
\examples{

\dontrun{

library(PNetica) ## Need a specific implementation

irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne",
                           sep=.Platform$file.sep))
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- NetworkFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  
}


casepath <- paste(library(help="PNetica")$path,
                           "testdat","IRT10.2PL.200.items.cas",
                           sep=.Platform$file.sep)
## Record which nodes in the casefile we should pay attention to
NetworkNodesInSet(irt10.base,"onodes") <-
   NetworkNodesInSet(irt10.base,"observables")


BuildAllTables(irt10.base)
CompileNetwork(irt10.base) ## Netica requirement

item1 <- irt10.items[[1]]
priB <- PnodeBetas(item1)
priA <- PnodeAlphas(item1)
priCPT <- NodeProbs(item1)

gemout <- GEMfit(irt10.base,casepath)

postB <- PnodeBetas(item1)
postA <- PnodeAlphas(item1)
postCPT <- NodeProbs(item1)

## Posterior should be different
stopifnot(
  postB != priB, postA != priA
)


### The network that was used for data generation.
irt10.true <- ReadNetworks(paste(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.true.dne",
                           sep=.Platform$file.sep))
irt10.true <- as.Pnet(irt10.true)  ## Flag as Pnet, fields already set.
irt10.ttheta <- NetworkFindNode(irt10.true,"theta")
irt10.titems <- PnetPnodes(irt10.true)
## Flag titems as Pnodes
for (i in 1:length(irt10.titems)) {
  irt10.titems[[i]] <- as.Pnode(irt10.titems[[i]])
}
NetworkNodesInSet(irt10.true,"onodes") <-
   NetworkNodesInSet(irt10.true,"observables")

BuildAllTables(irt10.true)
CompileNetwork(irt10.true) ## Netica requirement




DeleteNetwork(irt10.base)
DeleteNetwork(irt10.true)

}
}
\keyword{ graphs }
\keyword{ manip }
