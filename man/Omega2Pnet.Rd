\name{Omega2Pnet}
\alias{Omega2Pnet}
\title{Constructs a parameterized network from an Omega matrix.}
\description{

  An Omega matrix (represented as a data frame) is a structure which
  describes a Bayesian network as a series of regressions from the
  parent nodes to the child nodes.  It actually contains two matrixes,
  one giving the structure and the other the regression coefficients.
  A skeleton matrix can be constructed through the function
  \code{\link{Pnet2Omega}}. 

}
\usage{
Omega2Pnet(OmegaMat, pn, nodewarehouse, defaultRule = "Compensatory", defaultLink = "normalLink", defaultAlpha = 1, defaultBeta = 0, defaultLinkScale = 1, debug = FALSE, override = FALSE)
}
\arguments{
  \item{OmegaMat}{A data frame containing an Omega matrix (see values
    section of \code{\link{Pnet2Omega}}).
  }
  \item{pn}{A (possible empty) \code{\link{Pnet}} object.  This will be
    modified by the function.}
  \item{nodewarehouse}{A Node \link{Warehouse} which contains
    instructions for building nodes referenced in the Omega matrix but
    not in the network.}
  \item{defaultRule}{This should be a character scalar giving the name
    of a CPTtools combination rule (see
    \code{\link[CPTtools]{Compensatory}}).  With the regression model
    assumed in the algorithm, currently \dQuote{Compensatory} is the
    only value that makes sense.
  }
  \item{defaultLink}{This should be a character scalar giving the name
    of a CPTtools link function (see \code{\link{normalLink}}).  With
    the regression model assumed in the algorithm, currently
    \dQuote{normalLink} is the only value that makes sense.}
  \item{defaultAlpha}{A numeric scalar giving the default value for
    slope parameters.}
  \item{defaultBeta}{A numeric scalar giving the default value for
    difficulty (negative intercept)  parameters.}
  \item{defaultLinkScale}{A positive number which gives the default
    value for the link scale parameter.}
  \item{debug}{A logical value.  If true, extra information will be
    printed during process of building the Pnet.}
  \item{override}{A logical value.  If false, differences between any
    exsiting structure in the graph and the Omega matrix will raise an
    error.  If true, the graph will be modified to conform to the matrix.
  }
}
\details{

  Whittaker (1990) noted that a normal Bayesian network (one in which
  all nodes followed a standard normal distribution) could be described
  using the inverse of the covariance matrix, often denoted Omega.  In
  particular, zeros in the inverse covariance matrix represented
  variables which were conditionally independent, and therefore reducing
  the matrix to one with positive and zero values could provide the
  structure for a graphical model.  Almond (2010) proposed using this as
  the basis for specifying discrete Bayesian networks for the
  proficiency model in educational assessments (especially as
  correlation matrixes among latent variables are a possible output of a
  factor analysis).

  The Omega matrix is represented with a \code{\link[base]{data.frame}}
  object which contains two square submatrixes and a couple of auxiliary
  columns.  The first column should be named \dQuote{Node} and contains
  the names of the nodes.  This defines a collection of \var{nodes}
  which are defined in the Omega matrix.  Let \eqn{J} be the number of
  nodes (rows in the data frame). The next \eqn{J} columns should
  have the names the \var{nodes}.  Their values give the structural
  component of the matrix.  The following two columns are \dQuote{Link}
  and \dQuote{Rules} these give the name of the combination rule and
  link function to use for this row.  Next follows another series \eqn{J}
  \dQuote{A} columns, each should have a name of the form
  \dQuote{A.\var{node}}.  This defines a matrix \eqn{A} containing
  regression coefficients.  Finally, there should be two additional
  columns, \dQuote{Intercept} and \dQuote{PriorWeight}.

  Let \eqn{Q} be the logical matrix formed by the \eqn{J} columns after the
  first and let \eqn{A} be the matrix of coefficients.  The matrix
  \eqn{Q} gives the structure of the graph with \eqn{Q[i,j]} being true
  when Node \eqn{j} is a parent of node {i}.  By convention,
  \eqn{Q[j,j]=1}.  Note that unlike the inverse covariance matrix from
  which it gets its name, this matrix is not symmetric.  It instead
  reflects the (possibly arbitrary) directions assigned to the edges.
  Except for the main diagonal, \eqn{Q[i,j]} and \eqn{Q[j,i]} will not
  both be 1.  Note also, that \eqn{A[i,j]} should be positive only when
  \eqn{Q[i,j]=1}.  This provides an additional check that structures
  were correctly entered if the Omega matrix is being used for data
  entry.

  When the link function is set to \code{\link{normalLink}} and the
  rules is set of \code{\link{Compensatory}} the model is described as a
  series of regressions.  Consider Node \eqn{j} which has \eqn{K}
  parents.  Let \eqn{\theta_j} be a real value corresponding to that
  node and let \eqn{\theta_k} be a real (standard normal)
  value representing Parent Node \eqn{k} \eqn{a_k} represent the
  corresponding coefficient from the \eqn{A}-table.  Let \eqn{\sigma_j =
  a_{j,j}} that is the diagonal element of the \eqn{A}-table
  corresponding to the variable under consideration.  Let \eqn{b_j} be
  the value of the intercept column for Node \eqn{j}.  Then the model
  specifies that \eqn{theta_j} has a normal distribution with mean
  \deqn{\frac{1}{\sqrt{K}}\sum a_k\theta_k + b_j,} and standard
  deviation \eqn{\sigma_j}.  The regression is discretized to calculate
  the conditional probability table (see
  \code{\link[CPTtools]{normalLink}} for details).

  Note that the parameters are deliberately chosen to look like a
  regression model.  In particular, \eqn{b_j} is a normal intercept and
  not a difficulty parameter, so that in general
  \code{\link{PnodeBetas}} applied to the corresponding node will have
  the opposite sign.  The \eqn{1/\sqrt{K}} term is a variance
  stabilization parameter so that the variance of \eqn{\theta_j} will
  not be affected by number of parents of Node \eqn{j}.  The multiple
  R-squared for the regression model is
  \deqn{\frac{1/K \sum a_k^2}{ 1/K \sum a_k^2 + \sigma_j^2} .}
  This is often a more convenient parameter to elicit than
  \eqn{\sigma_j}.

  The function \code{Omega2Pnet} attempts to make adjustments to its
  \code{pnet} argument, which should be a \code{\link{Pnet}}, so that it
  conforms to the information given in the Omega matrix.  Nodes are
  created as necessary using information in the \code{nodewarehouse}
  argument, which should be a \code{\link{Warehouse}} object whose
  manifest includes instructions for building the nodes in the network.
  The warehouse supply function should either return an existing node in
  \code{pnet} or create a new node in \code{pnet}.  The structure of the
  graph is adjusted to correspond to the Q-matrix (structural part of
  the data frame).  If the value of the \code{override} argument is
  false, an error is raised if there is existing structure with a
  different topology.  If \code{override} is true, then the \code{pnet}
  is destructively altered to conform to the structural information in
  the Omega matrix.

  The \dQuote{Link} and \dQuote{Rules} columns are used to set the
  values of \code{\link{PnodeLink}(\var{node})} and
  \code{\link{PnodeRules}(\var{node})}. The off-diagonal elements of the
  A-matrix are used to set \code{\link{PnodeAlphas}(\var{node})} and the
  diagonal elements to set \code{\link{PnodeLinkScale}(\var{node})}.  The
  values in the \dQuote{Intercept} column are the negatives of the values
  \code{\link{PnodeBetas}(\var{node})}.  Finally, the values in the
  \dQuote{PriorWeight} column correspond to the values of
  \code{\link{PnodePriorWeight}(\var{node})}.  In any of these cases, if
  the value in the Omega matrix is missing, then the default value will
  be supplied instead.

  One challenge is setting up a matrix with the correct structure.  If
  the nodes have been defined, the the \code{\link{Pnet2Omega}} can be
  used to create a blank matrix with the proper format which can then be
  edited.

}
\section{Omega Matrix Structure}{

  An Omega Matrix should be an object of class \code{\link[base]{data.frame}})
  with number of rows equal to the number of nodes.  Throughout let
  \var{node} stand for the name of a node.
  \describe{
    \item{Node}{The name of the node described in this column.}
    \item{\var{node}}{One column for each node.  The value in this column
      should be 1 if the node in the column is regarded as a parent of the
      node referenced in the row.}
    \item{Link}{The name of a link function.  Currently,
      \dQuote{\link[CPTtools]{normalLink}} is the only value supported.}
    \item{Rules}{The name of the combination rule to use.  Currently,
      \dQuote{\link[CPTtools]{Compensatory}} is recommended.}
    \item{A.\var{node}}{One column for each node.  This should be a
      positive value if the corresponding \var{node} column has a 1.  This
      gives the regression coefficient.  If \var{node} corresponds to the
      current row, this is the residual standard deviation rather than a
      regression coefficient.  See details.}
    \item{Intercept}{A numeric value giving the change in prevalence for
      the two variables (see details).}
    \item{PriorWeight}{The amount of weight which should be given to the
      current values when learning conditional probability tables.  See
      \code{\link{PnodePriorWeight}}.}
  }
}

\value{

  The network \code{pnet} is returned.  Note that it is destructively
  modified by the commands to conform to the Omega matrix.

}
\section{Side Effects}{

  This function destructively modifies \code{pnet} and nodes referenced
  in the Qmat and supplied by the warehouses.

  Note that unlike typical R implementations, this is not necessarily
  safe.  In particular, if the Qmat references 10 node, and an error
  is raised when trying to modify the 5th node, the first 4 nodes will
  be modified, the last 5 will not be and the 5th node may be partially
  modified.  This is different from most R functions where changes are
  not committed unless the function returns successfully.

}


\references{

  Whittaker, J. (1990).  \emph{Graphical Models in Applied Multivariate
    Statistics}.  Wiley.

  Almond, R. G. (2010). \sQuote{I can name that Bayesian network in two
  matrixes.} \emph{International Journal of Approximate Reasoning.}
  \bold{51}, 167-178.

  Almond, R. G. (presented 2017, August). Tabular views of Bayesian
  networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
    Modeling Application Workshop 2017}. Symposium conducted at the
  meeting of Association for Uncertainty in Artificial Intelligence,
  Sydney, Australia. (International) Retrieved from \url{http://bmaw2017.azurewebsites.net/}

}
\author{Russell Almond}
\note{
While the Omega matrix allows the user to specify both link function
  and combination rule, the description of the Bayesian network as a
  series of regressions only really makes sense when the link function
  is \code{\link{normalLink}} and the combination rule is
  \code{\link{Compensatory}}.  These are included for future exapnsion.

  The representation, using a single row of the data frame for each node
  in the graph, only works well with the normal link function.  In
  particular, both the partial credit and graded response links require
  the ability to specify different intercepts for different states of
  the variable, something which is not supported in the Omega matrix.
  Furthermore, the \code{\link{OffsetConjunctive}} rule requires
  multiple intercepts.  Presumable the \code{\link{Conjunctive}} rule
  could be used, but the interpretation of the slope parameters is then
  unclear. If the variables need a model other than the compensatory
  normal model, it might be better to use a Q-matrix (see
  \code{\link{Pnet2Qmat}} to describe the variable.
}
\seealso{
  The inverse operation is \code{\link{Pnet2Omega}}.

  See \code{\link{Warehouse}} for description of the node warehouse
  argument.

  See \code{\link[CPTtools]{normalLink}} and
  \code{\link[CPTtools]{Compensatory}} for more 
  information about the mathematical model.
  
  The node attributes set from the Omega matrix include:
  \code{\link{PnodeParents}(\var{node})},
  \code{\link{PnodeLink}(\var{node})},
  \code{\link{PnodeLinkScale}(\var{node})},
  \code{\link{PnodeRules}(\var{node})},
  \code{\link{PnodeAlphas}(\var{node})},
  \code{\link{PnodeBetas}(\var{node})}, and
  \code{\link{PnodePriorWeight}(\var{node})}


}
\examples{

## Sample Omega matrix.
omegamat <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "miniPP-omega.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

\dontrun{
library(PNetica) ## Needs PNetica
sess <- NeticaSession()
startSession(sess)
curd <- getwd()

netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1,stringsAsFactors=FALSE)

nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

## Insures we are building nets from scratch
setwd(tempdir())
## Network and node warehouse, to create networks and nodes on demand.
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")

Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)
CM <- WarehouseSupply(Nethouse,"miniPP_CM")
CM1 <- Omega2Pnet(omegamat,CM,Nodehouse,override=TRUE,debug=TRUE)

Om2 <- Pnet2Omega(CM1,NetworkAllNodes(CM1))

stopSession(sess)
setwd(curd)

}

}
\keyword{ distribution}
\keyword{ graph }
