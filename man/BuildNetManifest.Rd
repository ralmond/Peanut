\name{BuildNetManifest}
\alias{BuildNetManifest}
\title{Builds a network manifest from a list of Pnets
}
\description{

  A network manifest is a table of meta data about a colleciton of
  networks.  Each line corresponds to the specific network.  This
  manifest can be used by a network warehouse (\code{\link{Warehouse}})
  to recreate the network on demand.
}
\usage{
BuildNetManifest(Pnetlist)
}
\arguments{
  \item{Pnetlist}{
    A list of \code{\link{Pnet}} objects which will appear in the
    network manifest.
  }
}
\details{

  A network manifest is a table (data frame) which describes a collection
  of networks.  It contains meta-data about the networks, and not the
  information about the nodes, contained in the node manifest
  (\code{\link{BuildNodeManifest}}) or the relaitonships between the
  nodes which is contained in the \eqn{Q}-matrix
  (\code{\link{Pnet2Qmat}}) or the \eqn{\Omega}-Matrix
  (\code{\link{Pnet2Omega}}).  The role of the net
  manifest is to be used as to create a Net \link{Warehouse} which is
  an argument to the \code{\link{Qmat2Pnet}} and
  \code{\link{Omega2Pnet}} commands, creating networks as they are
  referenced.  

  The \dQuote{Name} column of the table contains the network name and is
  a key to the table (so it should be unique).  It corresponds to
  \code{\link{PnetName}}.  The \dQuote{Title} (\code{\link{PnetTitle}})
  and \dQuote{Description} (\code{\link{PnetDescription}}) columns contain
  optional meta-data about the node.  The \dQuote{Pathname}
  (\code{\link{PnetPathname}}) column
  contiains the location of the file to which the network should be
  written and from which it can be read.  The \dQuote{Hub}
  (\code{\link{PnetHub}}) is for spoke models (evidence models) some of
  whose variables are defined in a hub network.  This the network in
  question is meant to be a spoke, then this field points at the
  corresponding hub.
}
\section{Logging}{

  \code{BuildNetManifest} uses the
  \code{\link[futile.logger]{flog.logger}} mechanism to log progress.
  To see progress messages, use
  \code{\link[futile.logger]{flog.threshold}(DEBUG)} (or \code{TRACE}).

}
\value{

  An object of type \code{\link[base]{data.frame}} where the columns
  have the following values.
  \item{Name}{A character value giving the name of the network.  This
  should be unique for each row and normally must conform to variable
  naming conventions.  Corresponds to the function \code{\link{PnetName}}.}
  \item{Title }{An optional character value giving a longer human readable name
  for the netowrk. Corresponds to the function \code{\link{PnetTitle}}.}
  \item{Hub}{If this model is incomplete without being joined to another
  network, then the name of the hub network.  Otherwise an empty
  character vector. Corresponds to the function \code{\link{PnetHub}}.}
  \item{Pathname}{The location of the file from which the network should
  be read or to which it should be written.  Corresponds to the function
  \code{\link{PnetPathname}}.} 
  \item{Description}{An optional character value documenting the purpose
  of the network. Corresponds to the function \code{\link{PnetDescription}}.}

  Note that the name column is regarded as a primary key to the table.
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

  Network functions called to find network data:
  \code{\link{PnetName}}, \code{\link{PnetTitle}},
  \code{\link{PnetPathname}}, \code{\link{PnetHub}},
  \code{\link{PnetDescription}}

  Used in the construction of Network \code{\link{Warehouse}}s (see
  \code{\link{WarehouseManifest}}).

  Similar to the function \code{\link{BuildNodeManifest}}.
  

}
\examples{
## This provides an example network manifest.
netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1, stringsAsFactors=FALSE)

\dontrun{
library(PNetica)  ## Example requires PNetica

sess <- NeticaSession()
startSession(sess)

netpath <- file.path(library(help="PNetica")$path, "testnets")
netnames <- paste(c("miniPP-CM","PPcompEM","PPconjEM","PPtwostepEM",
                  "PPdurAttEM"),"dne",sep=".")


Nets <- ReadNetworks(file.path(netpath,netnames),
                     session=sess)

netman <- BuildNetManifest(Nets)
stopifnot(all.equal(netman,netman1))

## BNWarehouse is the PNetica Net Warehouse.
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")

stopSession(sess)

}
}
\keyword{ interface }
\keyword{ graphs }
