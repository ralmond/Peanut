\name{PnodeMin}
\alias{PnodeMin}
\title{A minimal Pnode class for use in interface construction.}
\description{
  
  This is an experimental class which contains all of the meta-data of a Pnode, but isn't
  linked to backing Bayes net software.  Its intended use is for assistance in building 
  elicitation user interfaces.

}
\usage{
PnodeMin(name, states = c("Yes", "No"), parents = list(), title = title(), 
         description = character(), labels = character(), continuous = FALSE, 
         tvals = effectiveThetas(length(states)), rules = "Compensatory", 
         link = "partialCredit", alphas = 1, betas = 1, linkScale = NA, 
         QQ = 1, PriorWeight = NA, CPT = NULL)
}
\arguments{
  \item{name}{Name of the node.}
  \item{states}{Node state names.}
  \item{parents}{List of other nodes to use a parents.}
  \item{title}{Title of the node.}
  \item{description}{Node description.}
  \item{labels}{Node labels.}
  \item{continuous}{Logical flag for continuous nodes.}
  \item{tvals}{Values assigned to node levels.}
  \item{rules}{Combination rules.}
  \item{link}{Link function.}
  \item{alphas}{Slope parameters.}
  \item{betas}{Intercept parameters.}
  \item{linkScale}{Scale parameter}
  \item{QQ}{Inner Q-matrix}
  \item{PriorWeight}{Prior weights}
  \item{CPT}{Conditional probability table.}
}
\details{

  This class is bascially just a list containing the necessary fields.
  See the various accessor functions for the interpretation of the fields for more
  complete defintion.

}
\value{

  An \code{\linkS4class{Pnode}} object.

}
\author{Russell Almond}
\note{
  
  This is an experimental class.  It may be removed in future releases.
}
\seealso{
  
  \code{\linkS4class{Pnode}}.
  
}
\examples{

anode <- PnodeMin("anode")

}
\keyword{ classes }
