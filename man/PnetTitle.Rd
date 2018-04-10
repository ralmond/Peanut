\name{PnetTitle}
\alias{PnetTitle}
\alias{PnetDescription}
\alias{PnetTitle<-}
\alias{PnetDescription<-}
\title{
Gets the title or comments associated with a parameterized network.
}
\description{

The title is a longer name for a network which is not subject to naming
restrictions. The description is free form text used to document the
network.  Both fields are optional.
}
\usage{
PnetTitle(net)
PnetTitle(net) <- value
PnetDescription(net)
PnetDescription(net) <- value
}
\arguments{
  \item{net}{
    A \code{\link{Pnet}} object.
  }
  \item{value}{
    A character object giving the new title or description.
  }
}

\details{
  The title is meant to be a human readable alternative to the name,
  which is not limited to the network naming restrictions.  

  The text is any text the user chooses to attach to the network.  If
  \code{value} has length greater than 1, the vector is collapsed into a
  long string with newlines separating the components.
}

\value{
  A character vector of length 1 providing the title or description.

  Setter methods return the object.
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnet}}, \code{\link{PnetName}()} 

}
\examples{
\dontrun{
library(PNetica) ## Requires PNetica
sess <- NeticaSession()
startSession(sess)

firstNet <- CreateNetwork("firstNet",sess)

PnetTitle(firstNet) <- "My First Bayesian Network"
stopifnot(PnetTitle(firstNet)=="My First Bayesian Network")

now <- date()
PnetDescription(firstNet)<-c("Network created on",now)
## Print here escapes the newline, so is harder to read
cat(PnetDescription(firstNet),"\n")
stopifnot(PnetDescription(firstNet) ==
  paste(c("Network created on",now),collapse="\n"))


DeleteNetwork(firstNet)

stopSession(sess)
}
}
\keyword{ graph }
\keyword{ interface }
\keyword{ attribute }
