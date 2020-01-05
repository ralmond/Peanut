\name{Pnode-class}
\Rdversion{1.1}
\docType{class}
\alias{Pnode-class}

\title{Class \code{"Pnode"}}
\description{
  This is a virtual class.  Classes implementing the Pnet protocol
  should attach themselves using \code{\link[methods]{setIs}}.

  Note that \code{NULL} is always considered a member so that
  uninitialized in containers.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.

  Classes can register as belonging to this abstract class.  The trick
  for doing this is:
  \code{
    setIs("NodeClass","Pnode")
  }

  Currently \code{\link[RNetica]{NeticaNode}} is an example of an object
  of this class (but requires the \code{PNetica} package to provide all
  of the required functionality).

}
\section{Methods}{
  No methods defined with class "Pnode" in the signature;
  however, the following generic functions are available:

  \describe{
    \item{\link{PnodeName}}{\code{signature(node = "Pnode")}: Fetches node name.
    }
    \item{\link{PnodeName<-}}{\code{signature(node = "Pnode", value="character")}:
    Sets node name. 
    }
    \item{\link{PnodeTitle}}{\code{signature(node = "Pnode")}: Fetches node title.
    }
    \item{\link{PnodeTitle<-}}{\code{signature(node = "Pnode",
    value="character")}: Sets node title. 
    }
    \item{\link{PnodeDescription}}{\code{signature(node = "Pnode")}: Fetches
    documentation string for node.
    }
    \item{\link{PnodeDescription<-}}{\code{signature(node = "Pnode",
    value="character")}: Sets documentation string for node. 
    }
    \item{\link{PnodeLabels}}{\code{signature(node = "Pnode")}: Fetches a
    vector of lables assigned to this node.  
    }
    \item{\link{PnodeLabels<-}}{\code{signature(node = "Pnode", value =
    "character")}: Sets vector of labels assigned to this node.
    hub model.
    }
    \item{\link{PnodeNumStates}}{\code{signature(node = "Pnode")}: Fetches
    length of vector of states available for this node.
    }
    \item{\link{PnodeStates}}{\code{signature(node = "Pnode")}: Fetches vector
    of states available for this node.
    }
    \item{\link{PnodeStates<-}}{\code{signature(node = "Pnode", value)}: Sets
    vector of states for this node.
    }
    \item{\link{PnodeStateTitles}}{\code{signature(node = "Pnode")}: Fetches vector
    of states available for this node.
    }
    \item{\link{PnodeStateTitles<-}}{\code{signature(node = "Pnode", value)}: Sets
    vector of states for this node.
    }
    \item{\link{PnodeStateDescriptions}}{\code{signature(node = "Pnode")}: Fetches vector
    of states available for this node.
    }
    \item{\link{PnodeStateDescriptions<-}}{\code{signature(node = "Pnode",
    value)}: Sets vector of states for this node.
    }
    \item{\link{PnodeStateValues}}{\code{signature(node = "Pnode")}: Fetches vector
    of numeric values associated with states for this node.
    }
    \item{\link{PnodeStateValues<-}}{\code{signature(node = "Pnode", value)}: Sets
    vector of numeric values associated with states for this node.
    }
    \item{\link{PnodeStateBounds}}{\code{signature(node = "Pnode")}: Fetches
    matrix of upper and lower bounds for discritized states of a
    continuous node. 
    }
    \item{\link{PnodeStateBounds<-}}{\code{signature(node = "Pnode",
    value)}: Sets matrix of upper and lower bounds for discritized
    states of a continuous node. 
    }
    \item{\link{PnodeParents}}{\code{signature(node = "Pnode")}: Fetches a
      list of the nodes parents.
    }
    \item{\link{PnodeParents<-}}{\code{signature(node = "Pnode", value =
    "list")}: Sets a list of the nodes parents.
    }
    \item{\link{PnodeParentNames}}{\code{signature(node = "Pnode")}: Lists the
      names of the parents.
    }
    \item{\link{PnodeNumParents}}{\code{signature(node = "Pnode")}: The length
      of the parent vector.
    }
    \item{\link{isPnodeContinuous}}{\code{signature(node = "Pnode")}: Copies nodes from hub model into spoke model.
    }
    \item{\link{PnodeProbs}}{\code{signature(node = "Pnode")}: Fetchs the
        conditional probability table for the node.}
    \item{\link{PnodeProbs<-}}{\code{signature(node = "Pnode", value =
    "array")}: Sets the conditional probability table for the node.}
    \item{\link{PnodeEvidence}}{\code{signature(node = "Pnode")}: Fetches the
    current instantiated evidence for this node.
    }
    \item{\link{PnodeEvidence<-}}{\code{signature(node = "Pnode", value)}: Sets
      the instantiated evidence for this node.
    }
    \item{\link{PnodeMargin}}{\code{signature(node = "Pnode")}: Computes the vector
      of marginal beliefs associated with the state of this node given the
      evidence.
    }
    \item{\link{PnodeEAP}}{\code{signature(node = "Pnode")}: Computes the
      expected value of a node given the evidence.  This assumes node
      states are assigned numeric values.
    }
    \item{\link{PnodeSD}}{\code{signature(node = "Pnode")}: Computes the
      standard deviation of a node given the evidence.  This assumes node
      states are assigned numeric values.
    }
    \item{\link{PnodeMedian}}{\code{signature(node = "Pnode")}: Computes the
      median of a node given the evidence.  This assumes node
      states are ordered.
    }
    \item{\link{PnodeMedian}}{\code{signature(node = "Pnode")}: Computes the
      most likely state of a node given the evidence.  
    }
  }

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{Pnode}}.

  The class \code{\link[RNetica]{NeticaNode}} implements this protocol.

}
\examples{
showClass("Pnode")
\dontrun{
 setIs("NeticaNode","Pnode")
}
}
\keyword{classes}
