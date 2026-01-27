\name{WarehouseManifest}
\alias{WarehouseManifest}
\alias{WarehouseManifest<-}
\alias{WarehouseData}
\alias{WarehouseInventory}
\title{Manipulates the manifest for a warehouse}
\description{

  A \code{\link{Warehouse}} is an object which can either retrieve an
  existing object or create a new one on demand.  The \emph{manifest} is
  a \code{\link[base]{data.frame}} which contains data used for building
  the objects managed by the warehouse on demand.  The function
  \code{WarehouseManifest} access the entire manifest and
  \code{WarehouseData} extracts the warehouse data for a single item.
  \code{WarehouseInventory} returns a list of objects which have already
  been built.

}
\usage{
WarehouseManifest(warehouse)
WarehouseManifest(warehouse) <- value
WarehouseData(warehouse, name)
WarehouseInventory(warehouse)
}
\arguments{
  \item{warehouse}{A \code{\link{Warehouse}} object}
  \item{value}{A \code{data.frame} which provides the new manifest
    data.  The required columns depend on the type of data managed by
    the warehouse}
  \item{name}{A character vector which provides a key for a single
    object in the warehouse.}
}
\details{

  The \code{\link{Warehouse}} design pattern is a combination of a
  factory and a cache.  The idea is that if an object is needed, the
  warehouse will search the cache and return it if it already exists.
  If it does not exits, the warehouse will create it using the data in
  the \emph{manifest}.  The manifest is a \code{data.frame} with one or
  more columns serving as keys.  The function \code{ManifestData}
  extracts the data necessary to create a given object.

  Two kinds of warehouses are needed in the Peanut interface:
  \emph{net warehouses} and \emph{node warehouses}.

  \emph{Net Warehouse}.  A network warehouse will return an already
  existing network, read the network from disk, or build it from scratch
  as needed.  The required fields for a network warehouse manifest are
  given in the documentation for \code{\link{BuildNetManifest}}.  The
  key is the \dQuote{Name} column which should be unique for each row.
  The \var{name} argument to \code{WarehouseData} should be a character
  scalar corresponding to name, and it will return a \code{data.frame}
  with a single row.

  \emph{Node Warehouse}.  A network warehouse will return an already
  existing node in a network, or build it from scratch
  as needed.  The required fields for a network warehouse manifest are
  given in the documentation for \code{\link{BuildNodeManifest}}.  Note
  that node names are only unique within a network, so the key is the
  pair of columns \dQuote{Model} and \dQuote{NodeName}.  If the variable
  has more than 2 states, there may be more than two rows of the
  manifest which correspond to that node.  These should have unique
  values for the field \dQuote{StateName}.  The \var{name} argument to
  \code{WarehouseData} should be a character vector with the first
  element being the model name and the section the node name.  That
  function will return a \code{data.frame} with multiple rows (depending
  on the number of states).
  

}
\value{

  The function \code{WarehouseManifest} returns a
  \code{\link[base]{data.frame}} giving the complete warehouse
  manifest.  The function \code{WarehouseData} returns selected rows
  from that \code{data.frame}.

  The setter function returns the \code{warehouse} object.

  The function \code{WarehouseInventory} returns a data frame where each
  row corresponds to the key of an object which has been built.
}
\references{
Almond, R. G. (presented 2017, August). Tabular views of Bayesian
networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
  Modeling Application Workshop 2017}. Symposium conducted at the
meeting of Association for Uncertainty in Artificial Intelligence,
Sydney, Australia. (International) Retrieved from \url{http://bmaw2017.azurewebsites.net/}
}
\author{Russell Almond}
\note{

  The best way to build a manifest is probably to call
  \code{\link{BuildNetManifest}} or \code{\link{BuildNodeManifest}} on a
  couple of objects and use that to build a skeleton, which can then be
  edited with the specific needed data.
  
}
\seealso{
  \code{\link{Warehouse}}, \code{\link{BuildNetManifest}},
  \code{\link{BuildNodeManifest}}
  
}
\examples{
## This provides an example network manifest.
netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv", 
                                package="Peanut"),
                    row.names=1,stringsAsFactors=FALSE)


## This provides an example node manifest
nodeman1 <- read.csv(system.file("auxdata", "Mini-PP-Nodes.csv", 
                                package="Peanut"),
                     row.names=1,stringsAsFactors=FALSE)

\dontrun{
library(PNetica)  ## Example requires PNetica
sess <- NeticaSession()
startSession(sess)


## BNWarehouse is the PNetica Net Warehouse.
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")
stopifnot(all.equal(WarehouseManifest(Nethouse),netman1))

stopifnot(all.equal(WarehouseData(Nethouse,"miniPP_CM"),
   netman1["miniPP_CM",]))

netman2 <- netman1
netman2["miniPP_CM","Pathname"] <- "mini_CM.dne"
WarehouseManifest(Nethouse) <- netman2

stopifnot(all.equal(WarehouseData(Nethouse,"miniPP_CM"),
   netman2["miniPP_CM",]))


Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)

WarehouseData(Nodehouse,c("miniPP_CM","Physics"))


stopSession(sess)

}
}
\keyword{ interface }
\keyword{ graphs }
