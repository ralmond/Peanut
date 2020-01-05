\name{Pnet2Omega}
\alias{Pnet2Omega}
\title{Constructs an Omega matrix from a parameterized network.}
\description{

  An Omega matrix (represented as a data frame) is a structure which
  describes a Bayesian network as a series of regressions from the
  parent nodes to the child nodes.  It actually contains two matrixes,
  one giving the structure and the other the regression coefficients.
  If the parameters have not yet been added to nodes, then the function
  will use the supplied default values allowing the parameters to later
  be defined through the use of the function \code{\link{Pnet2Omega}}.

}
\usage{
Pnet2Omega(net, prof, defaultRule = "Compensatory", defaultLink = "normalLink", defaultAlpha = 1, defaultBeta = 0, defaultLinkScale = 1, debug = FALSE)
}
\arguments{
  \item{net}{A \code{\link{Pnet}} object containing the network to be
    described.}
  \item{prof}{A list of \code{\link{Pnode}} objects which will become
    the rows and columns of the matrix.}
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
    printed during process of building the Omega matrix.}
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

  The function \code{Pnet2Omega} builds an Omega matrix from an existing
  \code{\link{Pnet}}.  Only the nodes specified in the \code{prof}
  argument are included in the matrix, each row corresponding to a node.
  The values in the \dQuote{Node} column are taken from
  \code{\link{PnodeName}(\var{node})}. The values in the structural
  part of the matrix are taken from the graphical structure,
  specifically \code{\link{PnodeParents}(\var{node})}.  The
  \dQuote{Link} and \dQuote{Rules} columns are taken from
  \code{\link{PnodeLink}(\var{node})} and
  \code{\link{PnodeRules}(\var{node})}. The off-diagonal elements of the
  A-matrix are taken from the values of
  \code{\link{PnodeAlphas}(\var{node})} and the diagonal elements from
  \code{\link{PnodeLinkScale}(\var{node})}.  The values in the
  \dQuote{Intercept} column are the negatives of the values
  \code{\link{PnodeBetas}(\var{node})}.  Finally, the values in the
  \dQuote{PriorWeight} column correspond to the values of
  \code{\link{PnodePriorWeight}(\var{node})}; note that a value of
  \code{NA} indicates that the prior weight should be taken from the
  \code{\link{Pnet}}.  

  If the nodes do not yet have the various parameters set, then this
  function will create a blank Omega matrix, with default values
  (set from various optional arguments) for entries where the parameters
  have not yet been set.  This matrix can then be edited and read back
  in with \code{\link{Omega2Pnet}} as a way of setting the parameters of
  the network.
  
}
\value{

  An object of class (\code{OmegaMat},\code{\link[base]{data.frame}})
  with number of rows equal to the number of nodes.  Throughout let
  \var{node} stand for the name of a node.

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
  Sydney, Australia. (International) Retrieved from
  \url{http://bmaw2017.azurewebsites.net/} 
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
  The inverse operation is \code{\link{Omega2Pnet}}.

  See \code{\link[CPTtools]{normalLink}} and
  \code{\link[CPTtools]{Compensatory}} for more 
  information about the mathematical model.

  The node functions from which the Omega matrix is populated includes:
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

class(omegamat) <- c("OmegMat","data.frame") # To match Pnet2Omega output.
omegamat$PriorWeight <- rep("10",nrow(omegamat))

stopifnot(all.equal(omegamat,Om2))


DeleteNetwork(CM)
stopSession(sess)
setwd(curd)

}
}
\keyword{ distribution }
\keyword{ graph }
