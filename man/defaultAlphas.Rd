\name{PnodeDefaultAlphas}
\alias{PnodeDefaultAlphas}
\alias{PnodeDefaultBetas}
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
PnodeDefaultAlphas(node, rule=PnodeRules(node), link=PnodeLink(node) )
PnodeDefaultBetas(node, rule=PnodeRules(node), link=PnodeLink(node))
}
\arguments{
  \item{node}{A \code{\link{Pnode}} object whose
    \code{\link{PnodeLnAlphas}} or \code{\link{PnodeBetas}} field is to
    be set.
  }
  \item{rule}{A rule (e.g., \code{\link{Compensatory}}, 
    \code{\link{OffsetConjunctive}} or the name of a rule.
  }
  \item{link}{A link function (e.g., \code{\link{partialCredit}})
    or the name of a link function.}
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
  PnodeDefaultAlphas(comp,"Compensatory") == c(0,0),
  PnodeDefaultBetas(comp,"Compensatory") == 0,
  PnodeDefaultAlphas(conj,"OffsetConjuctive") == 0,
  PnodeDefaultBetas(conj,"OffsetConjunctive") == c(0,0)
)

DeleteNetwork(list(EM1,EM2))
stopSession(sess)
}
}
\keyword{ graph }
\keyword{ attrib }
