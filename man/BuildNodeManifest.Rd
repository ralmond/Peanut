\name{BuildNodeManifest}
\alias{BuildNodeManifest}
\title{Builds a table describing a set of Pnodes}
\description{

  A node manifest is a table where each line describes one state of a
  node in a Bayesian network.  As a node manifest may contain nodes from
  more than one network, the key for the table is the first two columns:
  \dQuote{Model} and \dQuote{NodeName}.  The primary purpose is that
  this can be given to a Node \link{Warehouse} to create nodes on
  demand. 

}
\usage{
BuildNodeManifest(Pnodelist)
}
\arguments{
  \item{Pnodelist}{
    A list of \code{\link{Pnode}} objects from which the table will be
    built. 
  }
}
\details{

  A node manifest is a table (data frame) which describes a collection
  of nodes.  It contains mostly meta-data about the nodes, and not the
  information about the relaitonships between the nodes which is
  contained in the \eqn{Q}-matrix (\code{\link{Pnet2Qmat}}) or the
  \eqn{\Omega}-Matrix (\code{\link{Pnet2Omega}}).  The role of the node
  manifest is to be used as to create a Node \link{Warehouse} which is
  an argument to the \code{\link{Qmat2Pnet}} and
  \code{\link{Omega2Pnet}} commands, creating nodes as they are
  referenced.  Hence it contains the information about the node which is
  not part of the \eqn{Q} or \eqn{\Omega} matrix.

  The \eqn{Q}-matrix can span multiple Bayesian networks.  The same
  variable can appear with the same name but slightly different
  definitions in two different networks.  Consequently, the key for this
  table is the \dQuote{Model} and \dQuote{NodeName} columns (usually the
  first two).  The function \code{\link{ManifestData}} when applied to a
  node warehouse should have a key of length 2 (model and node name) and
  will return multiple lines, one line corresponding to each state of
  the data frame.

  The columns \dQuote{ModelHub}, \dQuote{NodeTitle},
  \dQuote{NodeDescription} and \dQuote{NodeLabels} provide meta-data
  about the node.  They may be missing empty strings, indicating that
  meta-data is unavailable.

  The columns \dQuote{Nstates} and \dQuote{StateName} are required.
  The number of states should be an integer (2 or greater) and there
  should be as many rows with this model and node name as there are
  states.  Each should have a unique value for \dQuote{StateName}.  The
  \dQuote{StateTitle}, \dQuote{StateDescription} and \dQuote{StateValue}
  are optional, although if the variable is to be used as a parent
  variable, it is strongly recommended to set the state values.

  
}
\value{

  An object of class \code{\link[base]{data.frame}} with the following
  columns. 
  
  \bold{Node-level Key Fields}:
  \item{Model}{A character value giving the name of the Bayesian network
  to which this node belongs.  Corresponds to the value of
  \code{\link{PnodePnet}}. }
  \item{NodeName}{A character value giving the name of the node.  All
  rows with the same value in the model and node name columns are
  assumed to reference the same node. Corresponds to the value of
  \code{\link{PnodeName}}.}

  \bold{Node-level Fields}:
  \item{ModelHub}{If this is a spoke model (meant to be attached to a
  hub) then this is the name of the hub model (i.e., the name of the
  proficiency model corresponding to an evidence model. Corresponds to
  the value of 
  \code{\link{PnetHub}(PnodePnet(\var{node}))}.}
  \item{NodeTitle}{A character value containing a slightly longer
  description of the node, unlike the name this is not generally
  restricted to variable name formats.  Corresponds to the value of
  \code{\link{PnodeTitle}}.}
  \item{NodeDescription}{A character value describing the node, meant
  for human consumption (documentation).  Corresponds to the value of
  \code{\link{PnodeDescription}}.}
  \item{NodeLabels}{A comma separated list of identifiers of sets which
  this node belongs to.  Used to identify special subsets of nodes
  (e.g., high-level nodes or observeable nodes.  Corresponds to the value of
  \code{\link{PnodeLabels}}.}
  
  \bold{State-level Key Fields}:
  \item{Nstates}{The number of states.  This should be an integer
  greater than or equal to 2.  Corresponds to the value of
  \code{\link{PnodeNstates}}.}
  \item{StateName}{The name of the state.  This should be a string value
  and it should be different for every row within the subset of rows
  corresponding to a single node. Corresponds to the value of
  \code{\link{PnodeStateNames}}.}

  \bold{State-level Fields}:
  \item{StateTitle}{A longer name not subject to variable naming
  restrictions.  Corresponds to the value of
  \code{\link{PnodeStateTitles}}.}
  \item{StateDescription}{A human readable description of the state
  (documentation).  Corresponds to the value of
  \code{\link{PnodeStateDescriptions}}.}
  \item{StateValue}{A real numeric value assigned to this state.
  \code{\link{PnodeStateValues}}. }
  
}
\references{
Almond, R. G. (presented 2017, August). Tabular views of Bayesian
networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
  Modeling Application Workshop 2017}. Symposium conducted at the
meeting of Association for Uncertainty in Artificial Intelligence,
Sydney, Australia. (International) Retrieved from \url{http://bmaw2017.azurewebsites.net/}
}
\author{Russell Almond}

\seealso{

  Node functions called to find node meta-data:
  \code{\link{PnodeName}}, \code{\link{PnodeTitle}},
  \code{\link{PnodePnet}}, \code{\link{PnetHub}},
  \code{\link{PnodeDescription}}, \code{\link{PnetLabels}}.
  \code{\link{PnodeNstates}}, \code{\link{PnodeStateNames}},
  \code{\link{PnodeStateTitles}}, \code{\link{PnodeStateDescriptions}},
  \code{\link{PnodeStateValues}}.

  Used in the construction of Network \code{\link{Warehouse}}s.

  Similar to the function \code{\link{BuildNetManifest}}.


}
\examples{

## This expression provides an example Node manifest
nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

\dontrun{
library(PNetica) ## Requires PNetica
curd <- getwd()
setwd(file.path(library(help="PNetica")$path, "testnets"))

Nets <- ReadNetworks(c("miniPP-CM.dne", "PPcompEM.dne",
                       "PPconjEM.dne", "PPtwostepEM.dne"), session=sess)


CM <- Nets[[1]]
EMs <- Nets[-1]

nodeman <- BuildNodeManifest(lapply(NetworkAllNodes(CM),as.Pnode))

for (n in 1:length(EMs)) {
  nodeman <- rbind(nodeman,
                    BuildNodeManifest(lapply(NetworkAllNodes(EMs[[n]]),
                                             as.Pnode)))
}
## Exclude node labels from this test, as they could appear in arbitrary order
stopifnot(all.equal(nodeman[,-6],nodeman1[,-6]))

nl <- strsplit(nodeman$NodeLabels,",")
nl1 <- strsplit(nodeman1$NodeLabels,",")
stopifnot(all(mapply(setequal,nl,nl1)))

## This is the node warehouse for PNetica
Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)
phyd <- WarehouseData(Nodehouse,c("miniPP_CM","Physics"))
p3 <- MakePnode.NeticaNode(CM,"Physics",phyd)

setwd(curd)
stopSession(sess)

}
}

\keyword{ interface }
\keyword{ graphs }
