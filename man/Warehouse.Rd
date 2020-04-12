\name{Warehouse}
\alias{Warehouse}
\alias{WarehouseSupply}
\alias{WarehouseSupply,ANY-method}
\alias{WarehouseFetch}
\alias{WarehouseMake}
\alias{WarehouseFree}
\alias{ClearWarehouse}
\alias{is.valid}
\alias{is.PnodeWarehouse}
\alias{is.PnetWarehouse}
\title{
  A cache for Pnets or Pnodes
}
\description{
  A warehouse is an object which stores a collection of
  \code{\link{Pnode}}s or \code{\link{Pnet}}s.  When requested, it will
  supply the given object.  If the object already exists, it is
  returned.  If it does not yet exist, it is built using meta-data in
  the warehouse's manifest.
}
\usage{
WarehouseSupply(warehouse, name)
\S4method{WarehouseSupply}{ANY}(warehouse, name)
WarehouseFetch(warehouse, name)
WarehouseMake(warehouse, name)
WarehouseFree(warehouse, name)
ClearWarehouse(warehouse)
is.valid(warehouse,object)
is.PnetWarehouse(obj)
is.PnodeWarehouse(obj)
}
\arguments{
  \item{warehouse}{
    A warehouse object from which the object is to be created.
  }
  \item{name}{A character vector giving the name of the object.  Note
    that for net warehouses, the key is usually has length one, but for
    node warehouses, this usuall has the form (\var{model},\var{node}). 
  }
  \item{obj}{An object whose type is to be determined.}
  \item{object}{An object to be tested to see if it a valid object from
    this warehouse.}
}
\details{

  The warehouse is a combination of a cache and a factory.  The idea is
  that when a Pnet or Pnode object is needed, it is requested from the
  corresponding warehouse.  If the object exists, it is returned.  If
  the object does not exist, then the information in the manifest (see
  \code{\link{WarehouseManifest}()} is used to create a new object.  The key
  function is \code{WarehouseSupply(\var{warehouse},\var{name})}; this function
  looks for an object corresponding to \var{name} in \code{warehouse}.
  If it exists, it is returned, if not a new one is created.

  The generic functions
  \code{WarehouseFetch(\var{warehouse},\var{name})} and 
  \code{WarehouseMake(\var{warehouse},\var{name})} implement the supply
  protocol.  \code{WarehouseFetch(\var{warehouse},\var{name})} searches
  for an object corresponding to \var{name} in the \code{warehouse} and
  returns it if it exists or returns \code{NULL} if it does not.  The
  generic function \code{WarehouseMake(\var{warehouse},\var{name})}
  creates the object using the data in the manifest.

  The \code{WarehouseFree} and \code{WarehouseClear} functions complete
  the Warehouse prototocl.  These respectively remove the named object
  from the cache, and clear the cache.  Note that these may our may not
  make sense with the implementation.  (In the current
  \link[PNetica]{PNetica-package} implementation, the cache is
  maintained by the underlying \link[RNetica]{RNetica} objects,
  and hence it doesn't make sense to free an object without deleting
  it.)

  Each warehouse has a manifest which supplies the necessary data to
  build a praticular object.  The generic function
  \code{\link{WarehouseManifest}()} accesses the manifest, which
  generally takes the form of a \code{\link[base]{data.frame}} object.
  The functions \code{\link{BuildNetManifest}()} and
  \code{\link{BuildNodeManifest}()} build manifests for network and node
  objects respectively.  The generic function
  \code{\link{WarehouseData}(\var{warehouse},\var{name})} returns the
  rows of the manifest which correspond to a paraticular \code{name}.

  The Peanut package is concerned with two kinds of warehouses:  Pnet
  warehouses and Pnode warehouses.  Pnet warehouses contain 
  Pnets, and the key is the name of the network.  Each Pnet corresponds
  to a single line in the manifest, and the \var{name} is a character
  scalar.  A Pnet warehouse should return true when the generic function
  \code{is.PnetWarehouse()} is called.

  Pnode warehouses contain Pnodes, and the \var{name} is a
  character vector of length 2, with structure (\var{netname},
  \var{nodename}).  This is because nodes with the same name will
  frequently exist in two different networks.  Currently the manifest
  for a node contains one line for each possible state of the node.
  A Pnode warehouse should return true when the generic function
  \code{is.PnodeWarehouse()} is called.

  The function \code{is.valid} checks to see if the object is of a type
  mananged by the warehouse, and that it has valid data.  In particular,
  the \code{\link[RNetica]{RNetica}} package uses pointers to objects in Netica
  space (as might other implementations).  The \code{is.valid()}
  function checks that the current \code{\link{Pnet}} and
  \code{\link{Pnode}} object point to valid objects in the external heap
  if this is applicable to the application.


  The warehouse object is an abstract class, and implementing classes
  need to provide methods for the generic functions
  \code{WarehouseFetch()}, \code{WarehouseMake()},
  \code{WarehouseFree()}, \code{WarehouseData()},
  \code{WarehouseManifest()}, \code{\link{WarehouseCopy}()},
  \code{\link{is.legal.name}()},\code{\link{as.legal.name}()},
  \code{is.valid()}
  and \code{ClearWarehouse()} as well as one
  of the generic functions \code{is.PnetWarehouse} or
  \code{is.PnodeWarehouse}.

  There are two reference implementations in
  \code{\link[PNetica]{BNWarehouse}} and
  \code{\link[PNetica]{NNWarehouse}} (network and node warehouses
  respectively).  Both of these take advantage of the fact that the
  session and network objects in \code{\link[RNetica]{RNetica}}
  have built in environments which cache the networks and nodes
  respectively.  The \code{\link[RGAutils]{Warehouse-class}} object is a
  generic implementation that also may be of some use to potential
  implementors. 


  
}
\value{
  The return type of most functions will depend on the type of the
  warehouse.  In most cases, the functions return an object of the type
  of the warehouse.

  \item{Pnet Warehouses}{These return \code{TRUE} from the function
  \code{is.PnetWarehouse()}, and an object of type \code{\link{Pnet}} or
  \code{NULL} from the functions \code{WarehouseSupply()},
  \code{WarehouseFetch()}, and \code{WarehouseMake()}.  \code{NULL} is
  returned when the requested net is not in the warehouse or the
  manifest.}
  \item{Pnode Warehouses}{These return \code{TRUE} from the function
  \code{is.PnodeWarehouse()}, and an object of type \code{\link{Pnode}} or
  \code{NULL} from the functions \code{WarehouseSupply()},
  \code{WarehouseFetch()}, and \code{WarehouseMake()}.  \code{NULL} is
  returned when the requested net is not in the warehouse or the
  manifest.}

  The returns from the functions \code{WarehouseFree()} and
  \code{ClearWarehouse()} are arbitrary depending on the implementation.

}
\references{

Almond, R. G. (presented 2017, August). Tabular views of Bayesian
networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
  Modeling Application Workshop 2017}. Symposium conducted at the
meeting of Association for Uncertainty in Artificial Intelligence,
Sydney, Australia. (International) Retrieved from \url{http://bmaw2017.azurewebsites.net/}

Chambers, J. M. (2016) \emph{Extending R}.  CRC Press.
}
\author{Russell G. Almond}
\note{

The cache part of the warehouse, almost certainly needs to be
implemented using the reference class system of Chambers (2016).  In
particular, an \code{\link[base]{environment}} object provides the kind
of persistent storage and object persistance and uniqueness necessary
(this breaks the usual functional programming paradigm of R).

}
\seealso{

  Other warehouse functions:
  \code{\link{WarehouseCopy}}, \code{\link{is.legal.name}}

  These functions support the manifest process.
  \code{\link{WarehouseManifest}()}, \code{\link{WarehouseData}()} 

  These functions construct manifests:
  \code{\link{BuildNetManifest}()}, \code{\link{BuildNodeManifest}()} 

  These functions use the warehouse to build networks:
  \code{\link{Omega2Pnet}} \code{\link{Qmat2Pnet}}

}
\examples{

\dontrun{
## Requires PNetica package
library(PNetica)
sess <- NeticaSession()
startSession(sess)

### This tests the manifest and factory protocols.

nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1, stringsAsFactors=FALSE)


### Test Net building
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")
stopifnot(is.PnetWarehouse(Nethouse))

setwd(paste(library(help="PNetica")$path, "testnets",sep=.Platform$file.sep))
CM <- WarehouseSupply(Nethouse,"miniPP_CM")
stopifnot(is.null(WarehouseFetch(Nethouse,"PPcompEM")))
EM1 <- WarehouseMake(Nethouse,"PPcompEM")

EMs <- lapply(c("PPcompEM","PPconjEM", "PPtwostepEM", "PPdurAttEM"),
              function(nm) WarehouseSupply(Nethouse,nm))

### Test Node Building with already loaded nets

Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)
stopifnot(is.PnodeWarehouse(Nodehouse))

phyd <- WarehouseData(Nodehouse,c("miniPP_CM","Physics"))

p3 <- MakePnode.NeticaNode(CM,"Physics",phyd)

phys <- WarehouseSupply(Nodehouse,c("miniPP_CM","Physics"))
stopifnot(p3==phys)

for (n in 1:nrow(nodeman1)) {
  name <- as.character(nodeman1[n,c("Model","NodeName")])
  if (is.null(WarehouseFetch(Nodehouse,name))) {
    cat("Building Node ",paste(name,collapse="::"),"\n")
    WarehouseSupply(Nodehouse,name)
  }
}

WarehouseFree(Nethouse,PnetName(EM1))
stopifnot(!is.valid(Nethouse,EM1))



stopSession(sess)

}}
\keyword{ class }
\keyword{ interface }
