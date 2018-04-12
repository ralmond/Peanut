\name{PnodeStateTitles}
\alias{PnodeStateTitles}
\alias{PnodeStateTitles<-}
\alias{PnodeStateDescriptions}
\alias{PnodeStateDescriptions<-}
\title{
  Accessors for the titles and descriptions associated with states of
  a parameterized node.
}
\description{

  Each state of a \code{\link{Pnode}} has a short name (which could be
  restricted by the implementation) and a longer title (which generally
  can contain emedded spaces and other details to make it more
  readable).  Each state also can have a description associated with it.
  These functions get or set the state titles or descriptions.
}
\usage{
PnodeStateTitles(node)
PnodeStateTitles(node) <- value
PnodeStateDescriptions(node)
PnodeStateDescriptions(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{Pnode}} object whose state titles
    or descriptions will be accessed. 
  }
  \item{value}{
    A character vector of the same length as the number of states
  \code{length(\link{PnodeStates}(\var{node}))} which
    provides the new state titles or descriptions.
  }
}
\details{
  The titles are meant to be a more human readable version of the state
  names and are not subject the variable naming restrictions.
  The descriptions are meant to be a longer free form notes.

  Both titles and descriptions are returned as a named character vector with
  names corresponding to the state names.  Therefore one can change a
  single state title or description by accessing it either using the state
  number or the state name.
}
\value{
  Both \code{PnodeStateTitles()} and \code{PnodeStateDescriptions()} return a
  character vector of length
  \code{length(\link{PnodeStates}(\var{node}))} giving the 
  titles or descriptions respectively.  The names of this vector are
  \code{\link{PnodeStates}(\var{node})}.

  The setter methods return the modified \code{\link{Pnode}}
  object invisibly. 
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnode}}, \code{\link{PnodeStates}()},
  \code{\link{PnodeStateValues}()}
}
\examples{
\dontrun{
library(PNetica)##Requires PNetica
sess <- NeticaSession()
startSession(sess)
cnet <- CreateNetwork("CreativeNet", session=sess)

orig <- NewDiscreteNode(cnet,"Originality", c("H","M","L"))
PnodeStateTitles(orig) <- c("High","Medium","Low")
PnodeStateDescriptions(orig)[1] <- "Produces solutions unlike those typically seen."

stopifnot(
  PnodeStateTitles(orig) == c("High","Medium","Low"),
  grep("solutions unlike", PnodeStateDescriptions(orig))==1,
  PnodeStateDescriptions(orig)[3]==""
  )

sol <- NewDiscreteNode(cnet,"Solution",
       c("Typical","Unusual","VeryUnusual"))
stopifnot(
  all(PnodeStateTitles(sol) == ""),
  all(PnodeStateDescriptions(sol) == "")
  )

PnodeStateTitles(sol)["VeryUnusual"] <- "Very Unusual"
PnodeStateDescriptions(sol) <- paste("Distance from typical solution",
                      c("<1", "1--2", ">2"))
stopifnot(
  PnodeStateTitles(sol)[3]=="Very Unusual",
  PnodeStateDescriptions(sol)[1] == "Distance from typical solution <1"
  )

DeleteNetwork(cnet)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ attribute }
