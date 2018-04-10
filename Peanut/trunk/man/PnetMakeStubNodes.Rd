\name{PnetMakeStubNodes}
\alias{PnetMakeStubNodes}
\alias{PnetRemoveStubNodes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creates (or removes) references to nodes in a network}
\description{

  A \emph{stub node} is a reference in a \emph{spoke} network to a node
  in a \emph{hub} network.  The function \code{PnetMakeStubNodes} makes
  stub nodes in the spoke network.  The function \code{RemoveStubNodes}
  removes them.

}
\usage{
PnetMakeStubNodes(net, nodes)
PnetRemoveStubNodes(net, nodes)
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object in which the stub nodes will be
    created or removed.  This is generally a spoke (evidence model) network.}
  \item{nodes}{A list of \code{\link{Pnode}} objects.  In the case of
    \code{PnetMakeStubNodes} these are the nodes in the hub model which
    are to be copied.  In the case of \code{PnetRemoveStubNodes} these
    are the stub nodes to be removed.}
}
\details{

  In the hub-and-spoke model, spoke models (evidence models) reference
  nodes in the central hub model (proficiency model in educational
  applications).  The \emph{stub node} is a node (or pseudo-node) in the
  spoke model which is actually a reference to a node in the hub model.
  In the operation \code{\link{PnetAdjoin}} when the spoke model is
  combined with the hub model, the stubs are replaced with the actual
  nodes they represent.

  The pair of functions \code{PnetMokeStubNodes} and
  \code{PnetRemoveStubNodes} are used inside of \code{\link{Qmat2Pnet}}
  to create the necessary references to the proficiency nodes (in the
  columns of the \eqn{Q}-matrix) while building the conditional
  probability tables for the observable nodes (the rows of the
  \eqn{Q}-matrix).  The function \code{PnetMakeStubNodes} gets called
  before the conditional probability tables are built, and the function
  \code{PnetRemoveStubNodes} gets called after all conditional
  probability tables are built.

}
\value{

  The function \code{PnetMakeStubNodes} returns a list of the newly
  created stub nodes.

  The return of the function \code{PnetRemoveStubNodes} is
  implementation dependent, and is called mainly for its side effects.

  Both functions destructively modify the \code{net} argument.
}
\references{

    Almond, R. G. (presented 2017, August). Tabular views of Bayesian
  networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
    Modeling Application Workshop 2017}. Symposium conducted at the
  meeting of Association for Uncertainty in Artificial Intelligence,
  Sydney, Australia. (International) Retrieved from
  \url{http://bmaw2017.azurewebsites.net/} 


}
\author{Russell Almond}
\note{

  The behavior of these functions will depend a lot on the underlying
  implementation, and they should be thought of as a pair.  The function
  \code{PnetMakeStubNodes} gets called before constructing the
  conditional probability tables, and \code{PnetRemoveStubNodes}.  For
  example, this could be used to give the nodes the official hub node
  name while constructing the conditional probability tables and then
  rename them to something else.

  In the \code{\link[PNetica]{PNetica-package}} implementation, the
  function \code{PnetMakeStubNodes} copies the nodes from the hub to the
  spoke, and the function \code{PnetRemoveStubNodes} deletes them (which
  if they are attached as a parent, automatically creates a stub node in
  Netica). 

}
\seealso{

  \code{\link{PnetHub}(\var{spoke})} give the name of the hub node for a
  given spoke.

  The function \code{\link{PnetAdjoin}(\var{hub},\var{spoke})} merges
  hub and spoke networks replacing the stubs with the originals in the
  hub network.

  The function \code{\link{Qmat2Pnet}} uses \code{PnetMakeStubNodes} and
  \code{PnetRemoveStubNodes} internally.
  
}
\examples{
\dontrun{
library(PNetica) ## Needs PNetica
sess <- NeticaSession()
startSession(sess)

PM <- ReadNetworks(file.path(library(help="PNetica")$path, "testnets",
     "miniPP-CM.dne"), session=sess)
EM1 <- ReadNetworks(file.path(library(help="PNetica")$path, "testnets",
      "PPcompEM.dne"), session=sess)

## Find the target node and its parents.
obs <- PnetFindNode(EM1,"CompensatoryObs")
pars <- PnetFindNode(PM,c("NTL","POfMom"))

## Make stub nodes for the parents
stubs <- PnetMakeStubNodes(EM1,pars)
## Set them as the parents
PnodeParents(obs) <- stubs

## Build the CPT
PnodeLink(obs) <- "partialCredit"
PnodeRules(obs) <- "Compensatory"
PnodeAlphas(obs) <- c(NTL=0.9,POfMom=1.1)
PnodeBetas(obs) <- 0.3
PnodeQ(obs) <- TRUE
BuildTable(obs)

##Done, now remove the stubs
PnetRemoveStubNodes(EM1,stubs)

DeleteNetwork(list(PM,EM1))
stopSession(sess)
}
}
\keyword{ manip }
\keyword{ graph }

