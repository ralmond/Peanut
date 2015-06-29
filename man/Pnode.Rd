\name{Pnode}
\alias{Pnode}
\alias{is.Pnode}
\alias{as.Pnode}
\title{A Parameterized Bayesian network node}
\description{

  A node in a parameterized Bayesian network.  Note that this is a
  \dQuote{mixin} class, it is meant be added to another Bayesian network
  node class to indicate that it parameterized using the discrete
  partial credit framework.
}
\usage{
is.Pnode(x)
as.Pnode(x)
Pnode (node, lnAlphas, betas, rules="Compensatory",
       link="partialCredit",Q=TRUE,linkScale=NULL,
       priorWeight=NULL) 
}
\arguments{
  \item{x}{A object to test to see if it a parameterized node, or to
    coerce to a parameterized node.}
  \item{node}{An object that will become the base of the parameterized
    node.  This should already be a parameterized node, e.g., a
    \code{\link[RNetica]{NeticaNode}} object.}
  \item{lnAlphas}{A numeric vector of list of numeric vectors giving the
    log slope parameters.  See \code{\link{PnodeLnAlphas}} for a
    description of this parameter.  If missing, the constructor will try
    to create a pattern of zero values appropriate to the \code{rules}
    argument and the number of parent variables.}
  \item{betas}{A numeric vector of list of numeric vectors giving the
    intercept parameters.  See \code{\link{PnodeBetas}} for a
    description of this parameter.  If missing, the constructor will try
    to create a pattern of zero values appropriate to the \code{rules}
    argument and the number of parent variables.}
  \item{rules}{The combination rule or a list of combination rules.
    These should either be names of functions or function objects.  See
    \code{\link{PnodeRules}} for a description of this argument.}
  \item{link}{The name of the link function or the link function
    itself.  See \code{\link{PnodeLink}} for a description of the link
    function.}
  \item{Q}{A logical matrix or the constant \code{TRUE} (indicaticating
    that the Q-matrix should be a matrix of \code{TRUE}s).  See
    \code{\link{PnodeQ}} for a description of this parameter.}
  \item{linkScale}{A numeric vector of link scale parameters or
    \code{NULL} if scale parameters are not needed for the chosen link
    function.  See \code{\link{PnodeLinkScale}} for a description of
    this parameter.}
  \item{priorWeight}{A numeric vector of weights given to the prior
    parameter values for each row of the conditional probability table
    when learning from data (or a scalar if all rows have equal prior
    weight).  See \code{\link{PnodePriorWeight}} for a description of
    this parameter.}
}
\details{

  The \code{Pnode} class is basically a protocol which any Bayesian
  network node object can follow to work with the tools in the Peanut
  package.  This is really an abstract class (in java terms, an
  interface rather than a class) which exploits the rather
  loose S3 object system.  In particular, a \code{Pnode} is any object
  for which \code{is.Pnode} returns true.  The default method looks for
  the string \code{"Pnode"} in the class list.

  \bold{Fields.} A \code{Pnode} object has eight \dQuote{fields}
  (implemented through the accessor methods), which all \code{Pnode}
  objects are meant to support. These correspond to the arguments of the
  \code{\link[CPTtools]{calcDPCTable}} function.

  The function \code{\link{PnodeNet}} returns the \code{\link{Pnet}}
  object which contains the nodes.

  The function \code{\link{PnodeQ}} gets or sets a Q-matrix describing
  which paraent variables are relevant for which state transitions.  The
  default value is \code{TRUE} which indicates that all parent variables
  are relevant.
  
  The function \code{\link{PnodePriorWeight}} gets or sets the prior
  weights associated with the node.  This gives the relative weighting
  of the parameterized table as a prior and the observed data in the
  \code{\link{GEMfit}} algorithm.
  
  The function \code{\link{PnodeRules}} gets or sets the combination
  rules used to combine the influence of the parent variables.

  The functions \code{\link{PnodeLnAlphas}} and
  \code{\link{PnodeAlphas}} get or set the slope parameters associated
  with the combination rules.  Note that in many applications, the slope
  parameters are constrained to be positive and maximization is done
  over the log of the slope parameter.

  The function \code{\link{PnodeBetas}} gets or sets the difficulty
  (negative intercept) parameter asscoated with the combination rule.
  
  The function \code{\link{PnodeLink}} gets or sets the link function
  used to translate between the output of the combination rule and a row
  of the conditional probability table.

  The function \code{\link{PnodeLinkScale}} gets or sets a scale
  parameter associated with the link function.

  \bold{Generic Functions.} The importance of the \code{Pnode} object is
  that it supports the \code{\link{GEMfit}} method which adjust the
  parameters of the \code{Pnode} objects to fit a set of case data.  In
  order to be compatable with \code{GEMfit}, the \code{Pnode} object
  must support three methods:  \code{\link{PnodeParentTvals}},
  \code{\link{BuildTable}}, and  \code{\link{maxCPTParam}}.

  The generic function \code{\link{PnodeParentTvals}} returns a list of
  effective theta values (vectors of real numbers) associated with the
  states of the parent variables.  These are used to build the
  conditional probability tables.  

  The generic function \code{\link{BuildTable}} calls the function
  \code{\link[CPTtools]{calcDPCTable}} to generate a conditional
  probability table for the node using the current parameter values.  It
  also sets the node experience.

  The generic function \code{\link{maxCPTParam}} calls the function
  \code{\link[CPTtools]{mapDPC}} to calculate the optimal parameter
  values for the CPT for the node and the updates the parameter values.

}
\value{
  The function \code{is.Pnet} returns a logical scalar indicating
  whether or not the object claims to follow the \code{Pnet} protocol.  
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\seealso{

  Fields: \code{\link{PnodeNet}}, \code{\link{PnodeQ}},
       \code{\link{PnodePriorWeight}}, \code{\link{PnodeRules}}, 
       \code{\link{PnodeLink}}, \code{\link{PnodeLnAlphas}}, 
       \code{\link{PnodeAlphas}}, \code{\link{PnodeBetas}}, 
       \code{\link{PnodeLinkScale}}
    
  Generic Functions:  \code{\link{BuildTable}},
  \code{\link{PnodeParentTvals}}, \code{\link{maxCPTParam}}

  Functions: \code{\link{GetPriorWeight}},
  \code{\link[CPTtools]{calcDPCTable}}, \code{\link[CPTtools]{mapDPC}}

  Related Classes: \code{\link{Pnet}}
}
\examples{

 \dontrun{

## These are the implementations of the two key generic functions in
## PNetica

BuildTable.NeticaNode <- function (node) {
  node[] <- calcDPCFrame(ParentStates(node),NodeStates(node),
                          PnodeLnAlphas(node), PnodeBetas(node),
                          PnodeRules(node),PnodeLink(node),
                          PnodeLinkScale(node),PnetQ(node),
                          PnodeParentTvals(node))
  NodeExperience(node) <- GetPriorWeight(node)
  invisible(node)
}

maxCPTParam.NeticaNode <- function (node, Mstepit=3,
                                    tol=sqrt(.Machine$double.eps)) {
  ## Get the posterior pseudo-counts by multiplying each row of the 
  ## node's CPT by its experience.
  counts <- sweep(node[[]],1,NodeExperience(node),"*")
  est <- mapDPC(counts,ParentStates(node),NodeStates(node),
                PnodeLnAlphas(node), PnodeBeta(node),
                PnodeRules(node),PnodeLink(node),
                PnodeLinkScale(node),PnodeQ(node),
                control=list(reltol=tol,maxits=Mstepit)
                )
  PnodeLnAlphas(node) <- est$lnAlphas
  PnodeBetas(node) <- est$betas
  PnodeLinkScale(node) <- est$linkScale
  invisible(node)
}



}

}
\keyword{ classes }
\keyword{ graphs }
\keyword{ interface }
