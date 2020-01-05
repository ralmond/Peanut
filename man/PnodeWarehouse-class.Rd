\name{PnodeWarehouse-class}
\Rdversion{1.1}
\docType{class}
\alias{PnodeWarehouse-class}

\title{Class \code{"PnodeWarehouse"}}
\description{

  A \code{\link{Warehouse}} objects which holds and builds
  \code{\linkS4class{Pnode}} objects.  In particular, its
  \code{\link{WarehouseManifest}} contains a node manifest (see
  \code{\link{BuildNodeManifest}}) which contains information about how
  to build the nodes if they are not present.  Note that the key of the
  node manifest is the name of both the network and the node.

}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.

  Classes can register as belonging to this abstract class.  The trick
  for doing this is:
  \code{
    setIs("NodehouseClass","PnodeWarehouse")
  }

  Currently \code{\link[PNetica]{NNWarehouse}} is an example of an object
  of this class.

}
\section{Methods}{

  Note that for all of these methods, the \code{name} should be a vector
  of two elements, the network name and the node name. Thus each network
  defines its own namespace for variables.  
  
  \describe{

    \item{\link{WarehouseSupply}}{\code{signature(warehouse =
        "PnodeWarehouse", name = "character")}.  This finds a node with
      the appropriate name in the specified network.  If one does not
      exist, it is created using the metadata in the manifest.}

    \item{\link{WarehouseFetch}}{\code{signature(warehouse =
        "PnodeWarehouse", name = "character")}.  This fetches the node
      with the given name in the named network, or returns \code{NULL}
      if it has not been built.}

    \item{\link{WarehouseMake}}{\code{signature(warehouse =
        "PnodeWarehouse", name = "character")}.  This creates the node 
      using the meta-data in the Manifest.}

    \item{\link{WarehouseFree}}{\code{signature(warehouse =
        "PnodeWarehouse", name = "character")}.  This removes the node
        from the warehouse inventory.}

    \item{\link{ClearWarehouse}}{\code{signature(warehouse =
        "PnodeWarehouse")}.  This removes all nodes
        from the warehouse inventory.}

    \item{\link{is.PnodeWarehouse}}{\code{signature(obj =
        "PnodeWarehouse")}.  This returns \code{TRUE}.}

    \item{\link{WarehouseManifest}}{\code{signature(warehouse =
        "PnodeWarehouse")}.  This returns the data frame with
      instructions on how to build nodes. (see Details)}

    \item{\link{WarehouseManifest<-}}{\code{signature(warehouse =
        "PnodeWarehouse", value="data.frame")}.  This sets the data
      frame with instructions on how to build nodes.(see Details)}

    \item{\link{WarehouseData}}{\code{signature(warehouse =
        "PnodeWarehouse", name="character")}.  This returns the portion
      of the data frame with instructions on how to build a particular
      node. This is generally one row for each state of the node.
      (see Details)}

  }


}
\details{

  The \code{PnetWarehouse} either supplies prebuilt nodes or builds them
  from the instructions found in the manifest.  Nodes exist inside
  networks, so the key for a node is a pair \code{(Model,NodeName)}.
  Thus, two nodes in different networks can have identical names.

  The function \code{WarehouseSupply} will attempt to:
  \enumerate{
    \item{Find an existing node with name \code{NodeName} in a network
      with name \code{Model}.}
    \item{Build a new node in the named network using the metadata in
      the manifest.} 
  }

  The manifest is an object of type \code{\link[base]{data.frame}} where
  the columns have the values show below.  The key is the combination of
  the \dQuote{Model} and \dQuote{NodeName} columns.  There should be one
  row with this combination of variables for each state of the
  variable.  In particular, the number of rows should equal the value of
  the \code{Nstates} column in the first row with that model--variable
  combination.  The \dQuote{StateName} column should be unique for each
  row. 

  The arguments to \code{WarehouseData} should be a character vector of
  length two, \code{(Model,NodeName)}.  It will return a
  \code{data.frame} with one row for each state of the variable.

  \describe{

    \item{\bold{Node-level Key Fields}}{:}
    \item{Model}{A character value giving the name of the Bayesian network
      to which this node belongs.  Corresponds to the value of
      \code{\link{PnodeNet}}. }
    \item{NodeName}{A character value giving the name of the node.  All
      rows with the same value in the model and node name columns are
      assumed to reference the same node. Corresponds to the value of
      \code{\link{PnodeName}}.}

    \item{\bold{Node-level Fields}}{:}
    \item{ModelHub}{If this is a spoke model (meant to be attached to a
      hub) then this is the name of the hub model (i.e., the name of the
      proficiency model corresponding to an evidence model). Corresponds to
      the value of \code{\link{PnetHub}(PnodeNet(\var{node}))}.}
    \item{NodeTitle}{A character value containing a slightly longer
      description of the node, unlike the name this is not generally
      restricted to variable name formats.  Corresponds to the value of
      \code{\link{PnodeTitle}}.}
    \item{NodeDescription}{A character value describing the node, meant
      for human consumption (documentation).  Corresponds to the value of
      \code{\link{PnodeDescription}}.}
    \item{NodeLabels}{A comma separated list of identifiers of sets which
      this node belongs to.  Used to identify special subsets of nodes
      (e.g., high-level nodes or observeable nodes).  Corresponds to the
      value of \code{\link{PnodeLabels}}.}
  
    \item{\bold{State-level Key Fields}}{:}
    \item{Continuous}{A logical value.  If true, the variable will be
      continuous, with states corresponding to ranges of values.  If false,
      the variable will be discrete, with named states.}
    \item{Nstates}{The number of states.  This should be an integer
      greater than or equal to 2.  Corresponds to the value of
      \code{\link{PnodeNumStates}}.}
    \item{StateName}{The name of the state.  This should be a string value
      and it should be different for every row within the subset of rows
      corresponding to a single node. Corresponds to the value of
      \code{\link{PnodeStates}}.}

    \item{\bold{State-level Fields}}{:}
    \item{StateTitle}{A longer name not subject to variable naming
      restrictions.  Corresponds to the value of
      \code{\link{PnodeStateTitles}}.}
    \item{StateDescription}{A human readable description of the state
      (documentation).  Corresponds to the value of
      \code{\link{PnodeStateDescriptions}}.}
    \item{StateValue}{A real numeric value assigned to this state.
      \code{\link{PnodeStateValues}}. Note that this has different meaning
      for discrete and continuous variables.  For discrete variables, this
      associates a numeric value with each level, which is used in
      calculating the \code{\link{PnodeEAP}} and \code{\link{PnodeSD}}
      functions.  In the continuous case, this value is ignored and the
      midpoint between the \dQuote{LowerBounds} and \dQuote{UpperBounds}
      are used instead.}
    \item{LowerBound}{This servers as the lower bound for each partition
      of the continuous variagle. \code{-Inf} is a legal value for the
      first or last row.}
    \item{UpperBound}{This is only used for continuous variables, and the
      value only is needed for one of the states.  This servers as the
      upper bound of range each state.  Note the upper
      bound needs to match the lower bounds of the next state. \code{Inf}
      is a legal value for the first or last row.}
  }

  
}
\author{Russell Almond}

\note{

  The test for matching upper and lower bounds is perhaps too strict.
  In particular, if the upper and lower bounds mismatch by the least
  significant digit (e.g., a rounding difference) they will not match.
  This is a frequent cause of errors.

}
\seealso{
  \code{\link{Warehouse}}, \code{\link{WarehouseManifest}},
  \code{\link{BuildNodeManifest}}

  Implementation in the \code{PNetica} package:
  \code{\link[PNetica]{NNWarehouse}},
  \code{\link[PNetica]{MakePnode.NeticaNode}}
}
\examples{
showClass("PnodeWarehouse")
\dontrun{
library(PNetica) ## Requires PNetica
sess <- NeticaSession()
startSession(sess)

## This expression provides an example Node manifest
nodeman1 <- read.csv(file.path(library(help="Peanut")$path, "auxdata",
                           "Mini-PP-Nodes.csv"),
                     row.names=1,stringsAsFactors=FALSE)

nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

## Network and node warehouse, to create networks and nodes on demand.
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")

Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)

CM <- WarehouseSupply(Nethouse,"miniPP_CM")
WarehouseSupply(Nethouse,"PPdurAttEM")


WarehouseData(Nodehouse,c("miniPP_CM","Physics"))
WarehouseSupply(Nodehouse,c("miniPP_CM","Physics"))

WarehouseData(Nodehouse,c("PPdurAttEM","Attempts"))
WarehouseSupply(Nodehouse,c("PPdurAttEM","Attempts"))

WarehouseData(Nodehouse,c("PPdurAttEM","Duration"))
WarehouseSupply(Nodehouse,c("PPdurAttEM","Duration"))



}
}
\keyword{classes}
