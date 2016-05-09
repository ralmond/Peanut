\name{BNgenerics}
\alias{PnodeName}
\alias{PnodeStates}
\alias{PnodeNumStates}
\alias{PnodeParents}
\alias{PnodeNumParents}
\alias{PnodeParentNames}

\title{Aliases for generic Bayesian network functions.}
\description{
  These are all mostly self-explantory functions which almost any
  Bayesian network implementation will support.  These are alias of
  these functions so that generic functions can be written using Peanut
  which will support almost all Bayes net implementation.
}
\usage{
PnodeName(node)
PnodeStates(node)
PnodeNumStates(node)
PnodeParents(node)
PnodeNumParents(node)
PnodeParentNames(node)
}
\arguments{
  \item{node}{A object of a type which could be a Pnode, athough it does
    not necessarily need Pnode special properties.}
}
\details{

  The general idea is to find a minimal set of common Bayes net
  functions that any reasonable Bayes net package is likely to support
  so that basic code can be written which is generic across Bayes net
  packages.  For example, if \code{nd} is a
  \code{\link{RNetica}{NeticaNode}} object, then \code{PnodeName(nd)} is
  a synonym for \code{\link[RNetica]{NodeName(nd)}}.  However, using
  \code{PnodeName(nd)} is more portable as it could expand to another
  function if using a different Bayes net package.

  The goal is to be able to write simple loops based on things like
  number of parents and number of states which are common to most
  implementations.

  Note that setter methods are not supported, thus although
  \code{NodeName(nd) <- newname} is valid in
  \code{\link[RNetica]{RNetica.package}}, \code{PnodeName(nd) <-
  newname} will return an error.

}
\value{
  The expression \code{PnodeName(node)} returns a character scalar giving
  the name of \code{node}.

  The expression \code{PnodeStates(node)} returns a character vector giving
  the names of the states of \code{node}.

  The expression \code{PnodeNumStates(node)} returns an integer scalar giving
  the number of states of \code{node}.

  The expression \code{PnodeParents(node)} returns a list giving
  the parent objects.

  The expression \code{PnodeNumParents(node)} returns an integer scalar giving
  the number of parents of \code{node}.

  The expression \code{PnodeStates(node)} returns a character vector giving
  the names of the parents of \code{node}.

}
\author{Russell Almond}
\examples{
\dontrun{
PnodeName.NeticaNode <- function (node)
  NodeName(node)

PnodeStates.NeticaNode <- function (node)
  NodeStates(node)

PnodeNumStates.NeticaNode <- function (node)
  NodeNumStates(node)

PnodeParents.NeticaNode <- function (node)
  NodeParents(node)

PnodeParentNames.NeticaNode <- function (node)
  sapply(NodeParents(node),NodeName)

PnodeNumParents.NeticaNode <- function (node)
  length(NodeParents(node))

}
}
\keyword{ attribute }

