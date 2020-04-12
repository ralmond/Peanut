\name{PnetWarehouse-class}
\Rdversion{1.1}
\docType{class}
\alias{PnetWarehouse-class}
\alias{PnetWarehouse}

\title{Class \code{"PnetWarehouse"}}
\description{

  A \code{\link{Warehouse}} object which holds and builds
  \code{\linkS4class{Pnet}} objects.  In particular, its
  \code{\link{WarehouseManifest}} contains a network manifest (see
  \code{\link{BuildNetManifest}}) which contains information about how
  to either load the networks from the file system, or build them on
  demand.
  
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.

  Classes can register as belonging to this abstract class.  The trick
  for doing this is:
  \code{
    setIs("NethouseClass","PnetWarehouse")
  }

  Currently \code{\link[PNetica]{BNWarehouse}} is an example of an object
  of this class.

}
\section{Methods}{

  \describe{

    \item{\link{WarehouseSupply}}{\code{signature(warehouse =
        "PnetWarehouse", name = "character")}.  This finds a network
      with the appropriate name.  If one does not exist, it is created
      by reading it from the pathname specified in the manifest.  If
      no file exists at the pathname, a new blank network with the
      properities specified in the manifest is created.}

    \item{\link{WarehouseFetch}}{\code{signature(warehouse =
        "PnetWarehouse", name = "character")}.  This fetches the network
      with the given name, or returns \code{NULL} if it has not been
      built.} 

    \item{\link{WarehouseMake}}{\code{signature(warehouse =
        "PnetWarehouse", name = "character")}.  This loads the network
      from a file or builds the network using the data in the Manifest.}

    \item{\link{WarehouseFree}}{\code{signature(warehouse =
        "PnetWarehouse", name = "character")}.  This removes the network
        from the warehouse inventory.}

    \item{\link{ClearWarehouse}}{\code{signature(warehouse =
        "PnetWarehouse")}.  This removes all networks
        from the warehouse inventory.}

    \item{\link{is.PnetWarehouse}}{\code{signature(obj =
        "PnetWarehouse")}.  This returns \code{TRUE}.}

    \item{\link{WarehouseManifest}}{\code{signature(warehouse =
        "PnetWarehouse")}.  This returns the data frame with
      instructions on how to build networks. (see Details)}

    \item{\link{WarehouseManifest<-}}{\code{signature(warehouse =
        "PnetWarehouse", value="data.frame")}.  This sets the data
      frame with instructions on how to build networks.(see Details)}

    \item{\link{WarehouseData}}{\code{signature(warehouse =
        "PnetWarehouse", name="character")}.  This returns the portion
      of the data frame with instructions on how to build a particular
      network. (see Details)}

    \item{\link{WarehouseUnpack}}{\code{signature(warehouse =
        "PnetWarehouse", serial="list")}.  This restores a serialized
      network, in particular, it is used for saving network state across
      sessions.  See \code{\link{PnetSerialize}} for an example.}

  }
}
\details{

  The \code{PnetWarehouse} either supplies prebuilt nets or builds them
  from the instructions found in the manifest.  In particular, the
  function \code{WarehouseSupply} will attempt to:
  \enumerate{
    \item{Find an existing network with \code{name}.}
    \item{Try to read the network from the location given in the
      \code{Pathname} column of the manifest.}
    \item{Build a blank network, using the metadata in the manifest.}
  }

  The manifest is an object of type \code{\link[base]{data.frame}} where
  the columns have the values show below.  The key is the \dQuote{Name}
  column which should be unique for each row. The \var{name} argument to
  \code{WarehouseData} should be a character scalar corresponding to
  name, and it will return a \code{data.frame} with a single row.

  \describe{
    \item{Name}{A character value giving the name of the network.  This
      should be unique for each row and normally must conform to variable
      naming conventions.  Corresponds to the function \code{\link{PnetName}}.}
    \item{Title}{An optional character value giving a longer human readable name
      for the netowrk. Corresponds to the function \code{\link{PnetTitle}}.}
    \item{Hub}{If this model is incomplete without being joined to another
      network, then the name of the hub network.  Otherwise an empty
      character vector. Corresponds to the function \code{\link{PnetHub}}.}
    \item{Pathname}{The location of the file from which the network should
      be read or to which it should be written.  Corresponds to the function
      \code{\link{PnetPathname}}.} 
    \item{Description}{An optional character value documenting the purpose
      of the network. Corresponds to the function
      \code{\link{PnetDescription}}.} 
  }
  
  The function \code{\link{BuildNetManifest}} will build a manifest for
  an existing collection of networks.
  
}
\author{
  Russell Almond
}
\note{

  In the \code{PNetica} implementation, the
  \code{\link[PNetica]{BNWarehouse}} implementatation contains an
  embedded \code{\link[RNetica]{NeticaSession}} object.  When
  \code{WarehouseSupply} is called, it attempts to satisfy the demand by
  trying in order:
  \enumerate{
    \item{Search for the named network in the active networks in the
      session.}
    \item{If not found in the session, it will attempt to load the
      network from the \code{Pathname} field in the manifest.}
    \item{If the network is not found and there is not file at the
      target pathename, a new blank network is built and the appropriate
      fields are set from the metadata.}
  }

}
\seealso{

  \code{\link{Warehouse}}, \code{\link{WarehouseManifest}},
  \code{\link{BuildNetManifest}}

  Implementation in the \code{PNetica} package:
  \code{\link[PNetica]{BNWarehouse}},
  \code{\link[PNetica]{MakePnet.NeticaBN}}

}
\examples{
\dontrun{
library(PNetica)  ## Example requires PNetica

sess <- NeticaSession()
startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1, stringsAsFactors=FALSE)
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")

CM <- WarehouseSupply(Nethouse, "miniPP_CM")
EM <- WarehouseSupply(Nethouse, "PPcompEM")



DeleteNetwork(list(CM,EM))
stopSession(sess)
}
}
\keyword{classes}
\keyword{graphs}

