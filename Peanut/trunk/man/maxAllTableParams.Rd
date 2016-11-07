\name{maxAllTableParams}
\alias{maxAllTableParams}
\alias{maxAllTableParams.default}
\alias{maxCPTParam}
\title{Find optimal parameters of Pnet or Pnode to match expected tables}
\description{

  These functions assume that an expected count contingency table can be
  built from the network.  They then try to find the set of parameters
  maximizes the probability of the expected contingency table with
  repeated calls to \code{\link[CPTtools]{mapDPC}}.  The
  function \code{maxCPTParam} maximizes a single \code{\link{Pnode}} and
  the function \code{maxAllTableParams} maximizes all Pnodes (i.e., the
  value of \code{\link{PnetPnodes}(net)} in a \code{\link{Pnet}}.

}
\usage{
maxAllTableParams(net, Mstepit = 5, tol = sqrt(.Machine$double.eps), debug=TRUE)
\method{maxAllTableParams}{default}(net, Mstepit = 5, tol = sqrt(.Machine$double.eps), debug=TRUE)
maxCPTParam(node, Mstepit = 5, tol = sqrt(.Machine$double.eps)) 
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object giving the parameterized network.}
  \item{node}{A \code{\link{Pnode}} object giving the parameterized node.}
  \item{Mstepit}{A numeric scalar giving the number of maximization
    steps to take.  Note that the maximization does not need to be run
    to convergence.} 
  \item{tol}{A numeric scalar giving the stopping tolerance for the
    maximizer.}
  \item{debug}{A logical scalar.  If true, information about which node
    is being worked on is printed.}
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
      for each table.  The function \code{maxAllTableParams} does
      this step.}
    \item{Update CPTs}{Set all the conditional probability tables in the
      network to the new parameter values.  The function
      \code{\link{BuildAllTables}} does this.}
    \item{Convergence Test}{Calculate the log likelihood of the
      \code{cases} under the new parameters and stop if no change.  The
      function \code{\link{calcPnetLLike}} calculates the log likelihood.}
  }

  The function \code{maxAllTableParams} performs the M-step of this
  operation.  Under the \emph{global parameter independence} assumption,
  the parameters for the conditional probability tables for different
  nodes are independent given the sufficient statistics; that is, the
  expected contingency tables.  The default method of
  \code{maxAllTableParams} calls \code{maxCPTParam} on each node in
  \code{\link{PnetPnodes}(net)}.

  After the hyper-Dirichlet EM algorithm is run by
  \code{\link{calcExpTables}}, a hyper-Dirichlet prior should be
  available for each conditional probability table.  As the parameter of
  the Dirichlet distribution is a vector of pseudo-counts, the output of
  this algorithm should be a table of pseudo counts.  Often this is
  stored as the updated conditional probability table and a vector of
  row weights indicating the strength of information for each row.
  Using the \code{\link[RNetica]{RNetica-package}}, this is calculated
  as: \code{sweep(\link[RNetica]{NodeProbs}(item1),1,
                  \link[RNetica]{NodeExperience}(item1),"*")}

  The function \code{maxCPTParm} is essentially a wrapper which extracts
  the table of pseudo-counts from the network and then calls
  \code{\link[CPTtools]{mapDPC}} to maximize the parameters, updating
  the parameters of \code{node} to the result.

  The parameters \code{Mstepit} and \code{tol} are passed to
  \code{\link[CPTtools]{mapDPC}} to control the gradient descent
  algorithm used for maximization.  Note that for a generalized EM
  algorithm, the M-step does not need to be run to convergence, a couple
  of iterations are sufficient.  The value of \code{Mstepit} may
  influence the speed of convergence, so the optimal value may vary by
  application.  The tolerance is largely
  irrelevant (if \code{Mstepit} is small) as the outer EM algorithm does
  the tolerance test. 

                
}
\value{

  The expression \code{maxCPTParam(node)} returns \code{node} invisibly.
  The expression \code{maxAllTableParams(net)} returns \code{net} invisibly.

  As a side effect the \code{\link{PnodeLnAlphas}} and
  \code{\link{PnodeBetas}} fields of \code{node} (or
  all nodes in \code{\link{PnetPnodes}(net)}) are updated to better fit
  the expected tables.
  
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\note{

  The function \code{maxCPTParam} is an abstract generic function, 
  and it needs specific implementations.  See the 
  \code{\link[PNetica]{PNetica-package}} for an example.  A default
  implementation is provides for \code{maxAllTableParams} which loops
  through calls to \code{maxCPTParam} for each node in
  \code{\link{PnetPnodes}(net)}.  

  This function assumes that the host Bayes net implementation (e.g.,
  \code{\link[RNetica]{RNetica-package}}):  (1) \code{net} has an EM
  learning function, (2) the EM learning supports hyper-Dirichlet
  priors, (3) it is possible to recover the hyper-Dirichlet posteriors
  after running the internal EM algorithm.

}
\seealso{

  \code{\link{Pnet}}, \code{\link{Pnode}}, \code{\link{GEMfit}},
  \code{\link{calcPnetLLike}},  \code{\link{calcExpTables}},
  \code{\link[CPTtools]{mapDPC}} 

  
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

calcExpTables(irt10.base,casepath)

maxAllTableParams(irt10.base)

postB <- PnodeBetas(item1)
postA <- PnodeAlphas(item1)
BuildTable(item1)
postCPT <- NodeProbs(item1)

## Posterior should be different
stopifnot(
  postB != priB, postA != priA
)


DeleteNetwork(irt10.base)

}
}
\keyword{ manip }
