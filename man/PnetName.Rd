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
\section{True Names}{

  True names are the names in the secret ancient lanugage which hold
  power over an object (Le Guin, 1968).

  Actually, this is a difficulty with implementations that place
  restrictions on the name of a network or node.  In particular, Netica
  restricts node names to alphanumeric characters and limits the length.
  This may make it difficult to match nodes by name with other parts of
  the system which do not have this restriction.  In this case the
  object may have both a \emph{true name}, which is returned by
  \code{PnodeName} and an internal \emph{use name} which is used by the
  implementation.
  

}
\author{
  Russell Almond
}
\references{

  Le Guin, U. K. (1968).  \emph{A Wizard of Earthsea.}  Parnassus Press.

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

DeleteNetwork(net)
stopSession(sess)
}

}
\keyword{ graph }
\keyword{ interface }
\keyword{ attribute }
