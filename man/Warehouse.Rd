\name{Warehouse}
\alias{WarehouseSupply}
\alias{WarehouseFetch}
\alias{WarehouseMake}
\alias{WarehouseFree}
\alias{ClearWarehouse}
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
WarehouseFetch(warehouse, name)
WarehouseMake(warehouse, name)
WarehouseFree(warehouse, name)
ClearWarehouse(warehouse)
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
  \item{obj}{An object whose type is to be determined.
  }
}
\details{

  The warehouse is a combination of a cache and a factory.  The idea is
  that when a Pnet or Pnode object is needed, it is requested from the
  corresponding warehouse.  If the object exists, it is returned.  If
  the object does not exist, then the information in the manifest (see
  \code{WarehouseManifest}()} is used to create a new object.  The key
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
  maintained by the underlying \link[RNetica]{RNetica-package} objects,
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
  \code{is.PnetWarehouse}()} is called.

  Pnode warehouses contain Pnodes, and the \var{name} is a
  character vector of length 2, with structure (\var{netname},
  \var{nodename}).  This is because nodes with the same name will
  frequently exist in two different networks.  Currently the manifest
  for a node contains one line for each possible state of the node.
  A Pnode warehouse should return true when the generic function
  \code{is.PnodeWarehouse}()} is called.

  The warehouse object is an abstract class, and implementing classes
  need to provide methods for the generic functions
  \code{WarehouseFetch()}, \code{WarehouseMake()},
  \code{WarehouseFree()}, \code{WarehouseData()},
  \code{WarehouseManifest()}, and \code{ClearWarehouse()} as well as one
  of the generic functions \code{is.PnetWarehouse} or
  \code{is.PnodeWarehouse}.

  There are two reference implementations in
  \code{\link[PNetica]{BNWarehouse-class}} and
  \code{\link[PNetica]{NNWarehouse-class}} (network and node warehouses
  respectively).  Both of these take advantage of the fact that the
  session and network objects in \code{\link[RNetica]{RNetica-package}}
  have built in environments which cache the networks and nodes
  respectively.  The \code{\link[RGAUtils]{Warehouse-class}} object is a
  generic implementation that also may be of some use to potential implementors.
  
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (warehouse, name) 
{
    val <- WarehouseFetch(warehouse, name)
    if (is.null(val)) 
        val <- WarehouseMake(warehouse, name)
    val
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
