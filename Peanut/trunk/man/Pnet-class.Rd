\name{Pnet-class}
\Rdversion{1.1}
\docType{class}
\alias{Pnet-class}

\title{Class \code{"Pnet"}}
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
    setIs("NetClass","Pnet")
  }

  Currently \code{\link[RNetica]{NeticaBN}} is an example of an object
  of this class (but requires the \code{PNetica} package to provide all
  of the required functionality).
  
}
\section{Methods}{
  No methods defined with class "Pnet" in the signature;
  however, the following generic functions are available:

  \describe{
    \item{\link{PnetName}}{\code{signature(net = "Pnet")}: Fetches network name.
    }
    \item{\link{PnetName<-}}{\code{signature(net = "Pnet", value="character")}:
    Sets network name. 
    }
    \item{\link{PnetTitle}}{\code{signature(net = "Pnet")}: Fetches network title.
    }
    \item{\link{PnetTitle<-}}{\code{signature(net = "Pnet",
    value="character")}: Sets network title. 
    }
    \item{\link{PnetHub}}{\code{signature(net = "Pnet")}: Fetches name of hub
    (Proficiency model) if this is a spoke network (Evidence model).
    }
    \item{\link{PnetHub<-}}{\code{signature(net = "Pnet", value)}: Sets name of
    hub model.
    }
    \item{\link{PnetPathname}}{\code{signature(net = "Pnet")}: Fetches name of
    file in which network is saved.
    }
    \item{\link{PnetPathname<-}}{\code{signature(net = "Pnet", value)}: Sets name
    of file in which network is saved.
    }
    \item{\link{PnetDescription}}{\code{signature(net = "Pnet")}: Fetches
    documentation string for network.
    }
    \item{\link{PnetDescription<-}}{\code{signature(net = "Pnet",
    value="character")}: Sets documentation string for network. 
    }
    \item{\link{PnetFindNode}}{\code{signature(net = "Pnet",
    name="character")}: Finds a node by name.
    }
    \item{\link{PnetMakeStubNodes}}{\code{signature(net = "Pnet", nodes =
        "list")}: Copies nodes from hub model into spoke model.
      
    }
    \item{\link{PnetRemoveStubNodes}}{\code{signature(net = "Pnet", nodes =
        "list")}:  Removes copied nodes from hub model.
    }
    \item{\link{PnetAdjoin}}{\code{signature(hub = "Pnet", spoke = "Pnet")}:
      Attaches spoke to hub, matching stub nodes in spoke with their
      counterparts in the hub.
    }
    \item{\link{PnetDetach}}{\code{signature(motif = "Pnet", spoke = "Pnet")}:
      Removes the spoke from the motif (combined hub and spoke).
    }
    \item{\link{PnetCompile}}{\code{signature(net = "Pnet")}: Performs
      topological transformations on the net to make it ready for
      inference. 
    }
    \item{\link{PnetSerialize}}{\code{signature(net = "Pnet")}: Saves the net
      to a string which can be stored in a database.
    }
    \item{\link{PnetUnserialize}}{\code{signature(serial = "character")}:
      Reverses the above procedure.
    }
    \item{\link{unserializePnet}}{\code{signature(factory, data)}: this is an
      improved version of unserialize that assumes a store of networks.
    }
  }
  
}
\author{
  Russell Almond
}
\seealso{

  \code{\link{Pnet}}.

  The class \code{\link[RNetica]{NeticaBN}} implements this protocol.
}
\examples{
showClass("Pnet")
\dontrun{
 setIs("NeticaBN","Pnet")
}
}
\keyword{classes}
