\name{PnetHub}
\alias{PnetHub}
\alias{PnetHub<-}
\title{Returns the name of the hub net if this is a spoke net.}
\description{

  The hub-and-spoke model divides a complete model up into a central hub
  model (call a proficiency or competency model in educational
  applications) and spoke models (or evidence models) which reference
  variables in the hub network.  If a network is a spoke, then the field
  \code{PnetHub} should be set to the name of the corresponding hub
  network. 

}
\usage{
PnetHub(net)
PnetHub(net) <- value
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object whose hub name is to be accessed.}

  \item{value}{A character scalar giving the name of the new hub network.}

}
\value{

  The getter method returns either a character vector of length 1 giving
  the name of the hub, or \code{NA} or the empty string if no hub is set.

  The setter method returns the \code{net} argument.
  
}
\references{

    Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
    adaptive testing.  \emph{Applied Psychological Measurement}, 23,
    223--238. 

    Almond, R., Herskovits, E., Mislevy, R. J., & Steinberg,
    L. S. (1999). Transfer of information between system and evidence
    models. In Artificial Intelligence and Statistics 99, Proceedings
    (pp. 181--186). Morgan-Kaufman

  Almond, R. G. (presented 2017, August). Tabular views of Bayesian
  networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
    Modeling Application Workshop 2017}. Symposium conducted at the
  meeting of Association for Uncertainty in Artificial Intelligence,
  Sydney, Australia. (International) Retrieved from
  \url{http://bmaw2017.azurewebsites.net/} 

}
\author{Russell Almond}
\seealso{
  \code{\link{Pnet}}, \code{\link{PnetAdjoin}} (for merging hub and
  spoke), \code{\link{Qmat2Pnet}}, \code{\link{Pnet2Qmat}}
}
\examples{
\dontrun{
library(PNetica) # Requires PNetica
sess <- NeticaSession()
startSession(sess)
curd <- getwd()
setwd(file.path(library(help="PNetica")$path, "testnets"))

PM <- ReadNetworks("miniPP-CM.dne", session=sess)
stopifnot(PnetHub(PM)=="")


EM1 <- ReadNetworks("PPcompEM.dne", session=sess)
stopifnot(PnetHub(EM1)=="miniPP_CM")

foo <- CreateNetwork("foo",sess)
stopifnot(is.na(PnetHub(foo)))
PnetHub(foo) <- PnetName(PM)
stopifnot(PnetHub(foo)=="miniPP_CM")

DeleteNetwork(list(PM,EM1,foo))
stopSession(sess)
setwd(curd)

}
}
\keyword{ graph }
\keyword{ interface }
\keyword{ attribute }

