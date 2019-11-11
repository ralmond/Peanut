\name{PnetName}
\alias{PnetName}
\alias{PnetName<-}
\title{
  Gets or Sets the name of a Netica network.
}
\description{

Gets or sets the name of the network. Names must generally conform to
the network naming convention of the host Bayesian network system. In
particular, they should probably follow the rules for R variable names.
}
\usage{
PnetName(net)
PnetName(net) <- value
}

\arguments{
  \item{net}{
    A \code{\link{Pnet}} object.
}
  \item{value}{
    A character scalar containing the new name.
}
}
    
\details{
  Network names must conform to the rules for the host Bayes net
  system`q.  Trying to set the network to a name that does not 
  conform to the rules will produce an error, as will trying to set the
  network name to a name that corresponds to another different network.

  The \code{\link{PnetTitle}()} function provides another way to name
  a network which is not subject to the variable restrictions.
}

\value{
The name of the network as a character vector of length 1.

  The setter method returns the modified object.

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnet}}, \code{\link{PnetTitle}()} 
}
\examples{

\dontrun{
library(PNetica) ## Requires PNetica
sess <- NeticaSession()
startSession(sess)
net <- CreateNetwork("funNet",sess)
stopifnot(PnetName(net)=="funNet")

PnetName(net)<-"SomethingElse"
stopifnot(PnetName(net)=="SomethingElse")

}

}
\keyword{ graph }
\keyword{ interface }
\keyword{ attribute }