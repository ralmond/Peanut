\name{WarehouseCopy}
\alias{WarehouseCopy}
\alias{WarehouseCopy,default-method}
\title{Copies and object in the warehouse}
\description{

  The creates an object with a new name that shares the appropriate
  properties with the original object.  How this is handled depends on
  the type of objects conatined in the Warehouse.  The new object is
  placed in the warehouse inventory, although not in the manifest.

}
\usage{
WarehouseCopy(warehouse, obj, newname)
}
\arguments{
  \item{warehouse}{A \code{\link{Warehouse}} object.}
  \item{obj}{An object generated from the warehouse.}
  \item{newname}{A new name for the warehouse object.}
}
\details{

  For the \code{\link{PnetWarehouse}}, this is essentially a copy net
  function. The \code{newname} is the name of the new network, and it
  will be an error if this is the same as the name of an existing network.

  For the \code{\link{PnodeWarehouse}}, this is essentially a copy node
  function.  Note that in this case, the new name must be a character
  vector with two element: the new net name, and the new node name.  If
  the new net name is not the same as the net containing the \code{obj}
  argument, then the node will be copied into the new net.  If it is the
  same, then a new node will be added to the existing net.  Note that at
  least one part of the name (the net name or the node name) must be
  different. 

}
\value{

  A new object of the same type as \code{obj}, that is a new object
  managed by the \code{warehouse}.

}
\author{Russell Almond}
\seealso{
  \code{\link{Warehouse}}, \code{\link{WarehouseFree}}
}
\examples{

\dontrun{## Requires implementation
library(PNetica)

sess <- NeticaSession()
startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
table.dir <- system.file("auxdata", package="Peanut")
net.dir <- system.file("testnets", package="PNetica")

netman1 <- read.csv(file.path(table.dir,"Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)

CM <- WarehouseSupply(Nethouse, "miniPP_CM")
SM1 <- WarehouseCopy(Nethouse, CM, "Student1")

stopifnot(length(NetworkAllNodes(CM))==length(NetworkAllNodes(SM1)))

## This expression provides an example Node manifest
nodeman1 <- read.csv(file.path(table.dir,"Mini-PP-Nodes.csv"),
                     row.names=1,stringsAsFactors=FALSE)
Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)
NNodes <- length(NetworkAllNodes(CM))

IterD <- NetworkAllNodes(CM)$IterativeD

## Copy within net.
Explain <- WarehouseCopy(Nodehouse,IterD,c("miniPP_CM","Explanation"))
stopifnot(PnodeName(Explain)=="Explanation",
          PnetName(PnodeNet(Explain))=="miniPP_CM",
          all.equal(PnodeStates(IterD),PnodeStates(Explain)))


## Copy betwee nets.
Explain1 <- WarehouseCopy(Nodehouse,Explain,c("Student1","Explanation"))
stopifnot(PnodeName(Explain1)=="Explanation",
          PnetName(PnodeNet(Explain1))=="Student1",
          all.equal(PnodeStates(IterD),PnodeStates(Explain1)))




}
}
\keyword{ objects }
\keyword{ manip }
