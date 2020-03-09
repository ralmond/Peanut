\name{PnodePostWeight}
\alias{PnodePostWeight}
\title{Fetches the posterior weight associated with a node}
\description{

  Before running \code{\link{GEMfit}}, nodes are given a
  prior weight (\code{\link{PnodePriorWeight}}) indicating how much
  weight should be given to the prior distribution.  After running
  the \code{\link{calcExpTables}} step, there will be a posterior weight
  giving the total weight of the prior plus data.

}
\usage{
PnodePostWeight(node)
}
\arguments{
  \item{node}{A \code{\linkS4class{Pnode}} object.}
}
\details{

  Let \eqn{s} be a configuration of the parent variables, which
  corresponds to a row of the CPT of \code{node}
  (\code{\link{PnodeProbs}(node)}).  Let \eqn{\bold{p}_s = (p_{s,1},
  \ldots, p_{s,K})} be the corresponding row of the conditional
  probability table and let \eqn{n_s} be the corresponding prior weight
  (an element of code{\link{PnodePriorWeight}(node)}).  The corresponding
  row of the effective Dirichlet prior for that row is \eqn{\alpha_s =
  (\alpha_{s,1}, \ldots, \alpha_{s,K})}, where
  \eqn{\alpha_{s,1}=p_{s,1}n_s}. Note that the matrix \eqn{\bold{P}} and
  the vector \eqn{\bold{n}} (stacking the conditional probability
  vectors and the prior weights) are sufficient statistics for the
  conditional probability distribution of \code{node}.

  The function \code{\link{calcExpTables}} does the E-step (and some of
  the M-step) of the \code{\link{GEMfit}} algorithm.  Its output is new
  values for the sufficient statistics, \eqn{\tilde{\bold{P}}} and
  \eqn{\tilde{\bold{n}}}.  At this point, the function
  \code{\link{PnodeProbs}} should return \eqn{\tilde{\bold{P}}}
  (although possibly as an array rather than a matrix) and
  \code{PnodePostWeight(node)} returns \eqn{\tilde{\bold{n}}}.

  Although the \code{PnodePostWeight(node)} is used in the next step,
  \code{\link{maxAllTableParams}}, it is not retained for the next round
  of the \code{\link{GEMfit}} algorithm, instead the
  \code{PnodePriorWeight(node)} is used for the next time
  \code{\link{calcExpTables}} is run.

  Often, \code{\link{PnodePriorWeight}(node)} is set to a scalar,
  indicating that every row should be given the same weight, e.g.,
  \code{10}.  In this case, \code{PnodePostWeight(node)} will usually be
  vector valued as different numbers of data points correspond to each
  row of the CPT.  Furthermore, unless the parent variables are fully
  observed, the \code{PnodePostWeight(node)} are unlikely to be integer
  valued even if the prior weights are integers.  However, the posterior
  weights should always be at least as large as the prior weights.

}
\value{
  A vector of numeric values corresponding to the rows of the CPT of
  \code{node}.  An error may be produced if \code{\link{calcExpTables}}
  has not yet been run.
}
\references{

  Almond, R. G. (2015) An IRT-based parameterization for conditional
  probability tables.  In Agosta, J. M. and Carvalho, R. N. (Eds.)
  \emph{Proceedings of the Twelfth UAI Bayesian Modeling Application
  Workshop (BMAW 2015).}  \emph{CEUR Workshop Proceedings,} \bold{1565},
  14--23.  \url{http://ceur-ws.org/Vol-1565/bmaw2015_paper4.pdf}.

}
\author{Russell Almond}
\seealso{
  \code{\link{PnodePriorWeight}}, \code{\link{GEMfit}},
  \code{\link{calcExpTables}}, \code{\link{maxAllTableParams}}

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

casepath <- paste(library(help="PNetica")$path,
                           "testdat","IRT10.2PL.200.items.cas",
                           sep=.Platform$file.sep)

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
\keyword{ graph }

