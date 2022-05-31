\name{defaultAlphas}
\alias{defaultAlphas}
\alias{defaultBetas}
\title{Reshapes alpha or beta vector based on rule and parents}
\description{
  
  Combination rules can be sorted into multiple-a rules (e.g.,
  \code{\link[CPTtools]{Compensatory}}) and multiple-b rules (e.g.,
  \code{\link[CPTtools]{OffsetConjunctive}}).  The function
  \code{\link[CPTtools]{isOffsetRule}} distinguishes between the two
  types.  These functions adjust the log alpha or beta matrix to the
  correct length depending on the rule and parents of the \var{node}
  argument. 

}
\usage{
defaultAlphas(node, rule)
defaultBetas(node, rule)
}
\arguments{
  \item{node}{A \code{\link{Pnode}} object whose
    \code{\link{PnodeLnAlphas}} or \code{\link{PnodeBetas}} field is to
    be set.
}
\item{rule}{A character scalar giving the name of a combination rule.
}
}
\value{

  A vector of zeros of a suitable length to be used as a default value
  for \code{\link{PnodeLnAlphas}(\var{node})} or
  \code{\link{PnodeBetas}(\var{node})}.

}
\author{Russell Almond}
\note{
  These are used in the PNetica implementation of the
  \code{\link{Pnode}} constructor.
}
\seealso{
  \code{\link{Pnode}}, \code{\link{PnodeLnAlphas}},
  \code{\link{PnodeBetas}}, \code{\link[CPTtools]{isOffsetRule}}
}
\examples{
\dontrun{
library(PNetica) # Requires PNetica
sess <- NeticaSession()
startSession(sess)


EM1 <- ReadNetworks(system.file("testnets", "PPcompEM.dne",
                                package="PNetica"),
                    session=sess)
EM2 <- ReadNetworks(system.file("testnets", "PPconjEM.dne",
                                package="PNetica"),
                    session=sess)

comp <- PnetFindNode(EM1,"CompensatoryObs")
conj <- PnetFindNode(EM2,"ConjunctiveObs")

stopifnot(
  defaultAlphas(comp,"Compensatory") == c(0,0),
  defaultBetas(comp,"Compensatory") == 0,
  defaultAlphas(conj,"OffsetConjuctive") == 0,
  defaultBetas(conj,"OffsetConjunctive") == c(0,0)
)

DeleteNetwork(list(EM1,EM2))
stopSession(sess)
}
}
\keyword{ graph }
\keyword{ attrib }
