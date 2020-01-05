\name{PnetPathname}
\alias{PnetPathname}
\alias{PnetPathname<-}
\title{Returns the path associated with a network.}
\description{

  A \code{\link{Pnet}} is associated with a filename where it is
  stored.  This value should get set when the network is read or
  written.  Note that this will usually be the name of the network with
  a implementation file type.

}
\usage{
PnetPathname(net)
PnetPathname(net) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{A \code{\link{Pnet}} Bayesian network.}
  \item{value}{A character scalar giving the pathname for the network.}
}
\value{

  The getter form returns a character vector of length 1.  The setter
  form return the \code{Pnet} argument.

}
\author{Russell Almond}
\seealso{
  \code{\link{Pnet}}
}
\examples{
\dontrun{
library(PNetica) # Requires PNetica
sess <- NeticaSession()
startSession(sess)
curd <- getwd()
setwd(file.path(library(help="PNetica")$path, "testnets"))

PM <- ReadNetworks("miniPP-CM.dne", session=sess)
stopifnot(PnetPathname(PM)=="miniPP-CM.dne")
PnetPathname(PM) <- "StudentModel1.dne"
stopifnot(PnetPathname(PM)=="StudentModel1.dne")

DeleteNetwork(PM)

stopSession(sess)
setwd(curd)
}
}
\keyword{ graph }
\keyword{ interface }
\keyword{ attribute }
