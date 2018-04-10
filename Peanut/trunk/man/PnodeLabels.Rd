\name{PnodeLabels}
\alias{PnodeLabels}
\alias{PnodeLabels<-}
\title{
  Lists or changes the labels associated with a parameterize node.
}
\description{

  A label is a character identifier associated with a node which provides
  information about its role in the models.  This function returns or
  sets the labels associated with a node.

}
\usage{
PnodeLabels(node)
PnodeLabels(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{Pnode}} object.
  }
  \item{value}{
    A character vector containing the names of the labels that
    \var{node} should be associated with.  These names should follow the
    variable naming rules.
  }
}
\details{

  Netica node sets are a collection of string labels that can be
  associated with various nodes in a network.  These have proved to be
  very useful on writing code as often it is useful to perform some
  operation on only a certain kind of nodes.  One 
  purpose of node sets is to label a set of nodes that play a similar
  role in the model.  For example, \code{"ReportingVariable"} or
  \code{"Observable"}.

  The \code{PnodeLabels} function is an attempt to generalize that
  mechanism.  The expression \code{PnodeLabels(\var{node})} returns the
  labels currently associated with \var{node}, thus provides a general
  mechanism for identifying the roles that a node might play.

  The expression \code{PnodeLabels(\var{node})<-\var{value}} removes any
  labels previously associated with \var{node} and adds the new labels
  named in \var{value}.  The elements of \var{value} need not 
  correspond to existing labels, new node sets will be created for
  new values.  (Warning:  this implies that if the name of the node set
  is spelled incorrectly in one of the calls, this will create a new
  node set.  For example, \code{"Observable"} and \code{"Observables"}
  would be two distinct labels.)
 
  Two labels have special meaning in the Peanut package.  The function
  \code{\link{BuildAllTables}(\var{net})} rebuilds the tables for nodes
  which are labeled \dQuote{pnode} (i.e., parameterized nodes).  The
  function \code{\link{GEMfit}} attempts to fit the parameters for nodes
  labeled \dQuote{pnodes}, and associates values in the cases argument
  with the nodes labeled \dQuote{onodes}.

 
}
\value{

  A character vector giving the names of the labels \var{node} is
  associated with.  The setter form returns \var{node}.

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnode}}, \code{\link{BuildAllTables}},
  \code{\link{GEMfit}}, \code{\link{PnetPnodes}}
  
}
\examples{
\dontrun{
library(PNetica)##Requires PNetica
sess <- NeticaSession()
startSession(sess)

nsnet <- CreateNetwork("NodeSetExample", session=sess)

Ability <- NewDiscreteNode(nsnet,"Ability",c("High","Med","Low"))

EssayScore <- NewDiscreteNode(nsnet,"EssayScore",paste("level",5:0,sep="_"))

stopifnot(
  length(PnodeLabels(Ability)) == 0, ## Nothing set yet
)

PnodeLabels(Ability) <- "ReportingVariable"
stopifnot(
  PnodeLabels(Ability) == "ReportingVariable"
)
PnodeLabels(EssayScore) <- "Observable"
stopifnot(
  PnodeLabels(EssayScore) == "Observable"
)
## Make EssayScore a reporting variable, too
PnodeLabels(EssayScore) <- c("ReportingVariable",PnodeLabels(EssayScore))
stopifnot(
  setequal(PnodeLabels(EssayScore),c("Observable","ReportingVariable"))
)

## Clear out the node set
PnodeLabels(Ability) <- character()
stopifnot(
  length(PnodeLabels(Ability)) == 0
)

DeleteNetwork(nsnet)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ attributes }
