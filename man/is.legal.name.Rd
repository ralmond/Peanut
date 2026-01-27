\name{is.legal.name}
\alias{is.legal.name}
\alias{as.legal.name}
\title{Checks to see if names are valid for objects in warehouse.}
\description{

  Objects in a warehouse may have restrictions on the names that are
  allowed.  For example, Netica nodes and nets must have names that
  follow common variable naming convention (alphanumeric, starts with a
  letter, no embedded spaces, etc.).  The function \code{is.legal.name}
  checks the name type, and \code{as.legal.name} munges the name so that
  it is legal.
}
\usage{
is.legal.name(warehouse, name)
as.legal.name(warehouse, name)
}
\arguments{
  \item{warehouse}{A warehouse which defines the type of object.}
  \item{name}{A character vector giving names to be tested or munged.}
}
\value{

  For \code{is.valid.name}, a logical value returning the result of each
  test.

  For \code{as.valid.name}, a character vector with the modified names.

}
\author{Russell Almond}

\note{

  The \code{\link[PNetica]{BNWarehouse}} and
  \code{\link[PNetica]{NNWarehouse}} have a \code{prefix} field which is
  used to ensure that names always start with a letter.

}
\examples{
\dontrun{
## Requires PNetica
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
                       address=net.dir,prefix="S")

stopifnot(is.legal.name(Nethouse,c("CamelCase","Embedded Space")) ==
          c(TRUE,FALSE),
          as.legal.name(Nethouse,"100c3") == "S100c3")


## This expression provides an example Node manifest
nodeman1 <- read.csv(file.path(table.dir,"Mini-PP-Nodes.csv"),
                     row.names=1,stringsAsFactors=FALSE)
Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess,prefix="V")

stopifnot(is.legal.name(Nodehouse,c("Neg1","-1")) ==
          c(TRUE,FALSE),
          as.legal.name(Nodehouse,1) == "V1")


}
}
\keyword{ manip }

