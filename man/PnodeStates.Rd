\name{PnodeStates}
\alias{PnodeStates}
\alias{PnodeStates<-}
\title{ Accessor for states of a parameterized node.
}
\description{

  This function returns a list associated with a \code{\link{Pnode}}.
  The  function \code{PnodeStates} returns or manipulates them.
  Depending on the implementation, states may have restrictions on the
  names to it is best to stick with variable naming conventions (start
  with a letter, no embedded spaces or punctuation).

}
\usage{
PnodeStates(node)
PnodeStates(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{Pnode}} object whose states are to
    be accessed. 
  }
  \item{value}{
    A character vector giving the names of the new states.
    the names of the states.  State names may be restricted by the
    implementing package and should probably stick to variable naming
    conventions. 
  }
}
\details{

  The states are important when building conditional probability
  tables (CPTs).  In particular, the state names are used to label the
  columns of the CPT.  Thus, state names can be used to address arrays in the
  same way that \code{\link[base]{dimnames}} can.  In particular, the
  state names can be used to index the vectors returned by
  \code{\link{PnodeStates}()}, \code{\link{PnodeStateTitles}()},
  \code{\link{PnodeStateTitles}()}, and \code{\link{PnodeStateValues}()}.

}
\value{

  The function \code{PnodeStates()} returns a character vector whose
  values and names are both set to the state names.  The setter version
  of this function invisibly returns the 
  \var{node} object.

}
\note{

  Changing the number of states once a conditional probability table is
  set will change the dimensions of the table, and hence will likely
  remove it.
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnode}},
  \code{\link{PnodeName}()}, \code{\link{PnodeStateTitles}()},
  \code{\link{PnodeStateValues}()}, \code{\link{PnodeStateDescriptions}()},
}
\examples{
\dontrun{
library(PNetica)##Requires PNetica
sess <- NeticaSession()
startSession(sess)
anet <- CreateNetwork("Annette", session=sess)

## Discrete Nodes
nodel2 <- NewDiscreteNode(anet,"TwoLevelNode")
stopifnot(
  length(PnodeStates(nodel2))==2,
  PnodeStates(nodel2)==c("Yes","No")
)

PnodeStates(nodel2) <- c("True","False")
stopifnot(
  PnodeStates(nodel2)==c("True","False")
)


nodel3 <- NewDiscreteNode(anet,"ThreeLevelNode",c("High","Med","Low"))
stopifnot(
  PnodeStates(nodel3)==c("High","Med","Low"),
  PnodeStates(nodel3)[2]=="Med"
)

PnodeStates(nodel3)[2] <- "Median"
stopifnot(
  PnodeStates(nodel3)[2]=="Median"
)

PnodeStates(nodel3)["Median"] <- "Medium"
stopifnot(
  PnodeStates(nodel3)[2]=="Medium"
)

DeleteNetwork(anet)
stopSession(sess)
}
}
\keyword{ interface }
\keyword{ graphs }
