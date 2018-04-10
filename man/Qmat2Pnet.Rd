\name{Qmat2Pnet}
\alias{Qmat2Pnet}
\title{Makes or adjusts parameterized networks based on augmented Q-matrix}
\description{

  In augmented \eqn{Q}-matrix, there is a set of rows for each
  \code{\link{Pnode}} which describes the conditional probability table
  for that node in terms of the model parameters (see
  \code{\link{BuildTable}}).  As the Pnodes could potentially come from
  multiple nets, the key for the table is (\dQuote{Model},
  \dQuote{Node}).  As there are multiple rows per node, \dQuote{State}
  is the third part of the key.

  The function \code{Qmat2Pnet} adjusts the conditional probability
  tables of a node to conform to the supplied \eqn{Q}-matrix.


}
\usage{
Qmat2Pnet(Qmat, nethouse, nodehouse, defaultRule = "Compensatory", defaultLink = "partialCredit", defaultAlpha = 1, defaultBeta = NULL, defaultLinkScale = NULL, debug = FALSE, override = FALSE)
}
\arguments{
  \item{Qmat}{A \code{\link[base]{data.frame}} containing an augmented
    \eqn{Q}-matrix (See below).
  }
  \item{nethouse}{A \code{\link{Warehouse}} containing instructions for
    building the \code{\link{Pnet}} objects named in the \dQuote{Model}
    column of \code{Qmat}.
  }
  \item{nodehouse}{A \code{\link{Warehouse}} containing instructions for
    building the \code{\link{Pnode}} objects named in the
    (\dQuote{Model}, \dQuote{Name}) columns of \code{Qmat}.
  }
  \item{defaultRule}{This should be a character scalar giving the name
    of a CPTtools combination rule (see
    \code{\link[CPTtools]{Compensatory}}).  
  }
  \item{defaultLink}{This should be a character scalar giving the name
    of a CPTtools link function (see \code{\link{partialCredit}}).}
  \item{defaultAlpha}{A numeric scalar giving the default value for
    slope parameters.}
  \item{defaultBeta}{A numeric scalar giving the default value for
    difficulty (negative intercept) parameters.}
  \item{defaultLinkScale}{A positive number which gives the default
    value for the link scale parameter.}
  \item{debug}{A logical value.  If true, extra information will be
    printed during process of building the Pnet.}
  \item{override}{A logical value.  If false, differences between any
    exsiting structure in the graph and the \code{Qmat} will raise an
    error.  If true, the graph will be modified to conform to the matrix.
  }
}
\details{

  A \eqn{Q}-matrix is a 0-1 matrix which describes which proficiency
  (latent) variables are connected to which observable outcome
  variables; \eqn{q_{jk}=1} if and only if 
  proficiency variable \eqn{k} is a parent of observable variable
  \eqn{j}.  Almond (2010) suggested that augmenting the \eqn{Q}-matrix
  with additional columns representing the combination rules
  (\code{\link{PnodeRules}}), link function (\code{\link{PnodeLink}}),
  link scale parameter (if needed, \code{\link{PnodeLinkScale}}) and
  difficulty parameters (\code{\link{PnodeBetas}}).  The discrimination
  parameters (\code{\link{PnodeAlphas}}) could be overloaded with the
  \eqn{Q}-matrix, with non-zero parameters in places where there were
  1's in the \eqn{Q}-matrix. 

  This arrangement worked fine with combination rules (e.g.,
  \code{\link[CPTtools]{Compensatory}}) which contained multiple alpha
  (discrimination) parameters, one for each parent variable,  and a
  single beta (difficulty).  The introduction of a new type of offset
  rule (e.g., \code{\link[CPTtools]{OffsetDisjunctive}}) which uses a multiple
  difficulty parameters, one for each parent variable, and a single
  alpha.  Almond (2016) suggested a new augmentation which has three
  matrixes in a single table (a Qmat):  the \eqn{Q}-matrix, which
  contains structural information; the \eqn{A}-matrix, which contains
  discrimination parameters; and the \eqn{B}-matrix, which contains the
  difficulty parameters.  The names for the columns for these matrixes
  contain the names of the proficiency variables, prepended with
  \dQuote{A.} or \dQuote{B.} in the case of the \eqn{A}-matrix and
  \eqn{B}-matrix.   There are two additional columns marked \dQuote{A}
  and \dQuote{B} which are used for the discrimination and difficulty
  parameter in the multiple-beta and multiple-alpha cases.  There is
  some redundancy between the \eqn{Q}, \eqn{A} and \eqn{B} matrixes, but
  this provides an opportunity for checking the validity of the input.

  The introduction of the partial credit link function
  (\code{\link[CPTtools]{partialCredit}}) added a further
  complication.  With the partial credit model, there could be a
  separate set of discrimination or difficulty parameters for each
  transition for a polytomous item.  Even the
  \code{\link[CPTtools]{gradedResponse}} link function requires a
  separate difficulty parameter for each level of the varaible save the
  first.  The rows of the Qmat data structure are hence augmented to
  include one row for every state but the lowest-level state.  There
  should be of fewer rows of associated with the node than the value in
  the \dQuote{Nstates} column, and the names of the states (values in
  the \dQuote{State} column) should correspond to every state of the
  target variable except the first.  It is an error if the number of
  states does not match the existing node, or if the state names do not
  match what is already used for the node or is in the manifest for the
  node \code{\link{Warehouse}}.  

  Note that two nodes in different networks may share the same name, and
  two states in two different nodes may have the same name as well.
  Thus, the formal key for the Qmat data frame is (\dQuote{Model},
  \dQuote{Node}, \dQuote{State}), however, the rows which share the
  values for (\dQuote{Model}, \dQuote{Node}) form a subtable for that
  particular node.  In particular, the rows of the \eqn{Q}-matrix
  subtable for that node form the \emph{inner Q-matrix} for that node.
  The inner \eqn{Q}-matrix shows which variables are relevant for each
  state transition in a partial credit model.  The column-wise maximum
  of the inner \eqn{Q}-matrix forms the row of the outer \eqn{Q}-matrix
  for that node.  This shows which proficiency nodes are the parent of
  the observable node.  This corresponds to 
  \code{\link{PnodeQ}(\var{node})}. 

  The function \code{Qmat2Pnet} creates and sets the parameters of the
  observable \code{\link{Pnode}}s referenced in the \code{Qmat}
  argument.  As it needs to reference, and possibly create, a number of
  \code{\link{Pnet}}s and \code{Pnode}s, it requires both a network and
  a node \code{\link{Warehouse}}.  If the \code{override} parameter is
  true, the networks will be modified so that each node has the correct
  parents, otherwise \code{Qmat2Pnet} will signal an error if the
  existing network structure is inconsistent with the \eqn{Q}-matrix.

  As there is only one link function for each \var{node}, the values of
  \code{\link{PnodeLink}(\var{node})} and
  \code{\link{PnodeLinkScale}(\var{node})} 
  are set based on the values in the \dQuote{Link} and
  \dQuote{LinkScale} columns and the first row corresponding to
  \var{node}.  Note that the choice of link functions determines what is
  sensible for the other values but this is not checked by the code.

  The value of \code{\link{PnodeRules}(\var{node})} can either be a single
  value or a list of rule names.  The first value in the sub-Qmat must a
  character value, but if the other values are missing then a single
  value is used.  If not, all of the entries should be non-missing.  If
  this is a single value, then effectively the same combination rule is
  used for each transition.
  
  The interpretation of the \eqn{A}-matrix and the \eqn{B}-matrix
  depends on the value in the \dQuote{Rules} column.  There are two
  types of rules, multiple-A rules and multiple-B rules (offset rules).
  The CPTtools funciton \code{\link[CPTtools]{isOffsetRule}} checks to
  see what kind of a rule it is.  The multiple-A rules, of which
  \code{\link[CPTools]{Compensatory}} is the canonical example, have one
  discrimination (or slope) parameter for every parent variable (values
  of 1 in the \eqn{Q}-matrix) and have a single difficulty (negative
  intercept) parameter which is in the \dQuote{B} column of the Qmat.
  The multiple-B or offset rules, of which
  \code{\link[CPTtools]{OffsetConjunctive}} is the canonical example,
  have a difficulty (negative intercept) parameter for each parent
  variable and a single discrimination (slope) parameter which is in the
  \dQuote{A} column.  The function \code{Qmat2Pnet} uses the value of
  \code{isOffsetRule} to determine whether to use the multiple-B (true)
  or multiple-A (false) paradigm.

  A simple example is a binary observable variable which uses the
  \code{\link[CPTools]{Compensatory}} rule.  This is essentially a
  regression model (logistic regression with
  \code{\link[CPTtools]{partialCredit}} or
  \code{\link[CPTtools]{gradedResponse}} link funcitons, linear
  regression with \code{\link[CPTtools]{normalLink}} link function) on
  the parent variables.  The linear predictor is:
  \deqn{\frac{1}{\sqrt{K}} (a_1\theta_1 + \ldots + a_K\theta_K) - b .}
  The values \eqn{\theta_1, \ldots, \theta_K} are effective thetas, real
  values corresponding to the states of the parent variables.  The
  value \eqn{a_i} is stored in the column \dQuote{A.\var{namei}} where
  \var{namei} is the name of the \eqn{i}th proficiency variable; the
  value of \code{\link{PnodeAlphas}(\var{node})} is the vector \eqn{a_1,
  \ldots, a_k} with names corresponding to the parent variables.  The
  value of \eqn{b} is stored in the \dQuote{B} column; the value of
  \code{\link{PnodeBetas}(\var{node})} is \eqn{b}.

  The multiple-B pattern replaces the \eqn{A}-matrix with the
  \eqn{B}-matrix and the column \dQuote{A} with \dQuote{B}.
  Consider binary observable variable which uses the
  \code{\link[CPTools]{OffsetConjuctive}} rule.  The linear predictor is:
  \deqn{a \min (\theta_1 -b+1, \ldots , \theta_K- b_K) .}
  The value \eqn{b_i} is stored in the column \dQuote{B.\var{namei}} where
  \var{namei} is the name of the \eqn{i}th proficiency variable; the
  value of \code{\link{PnodeBetas}(\var{node})} is the vector \eqn{b_1,
  \ldots, b_k} with names corresponding to the parent variables.   The
  value of \eqn{a} is stored in the \dQuote{A} column; the value of
  \code{\link{PnodeBetas}(\var{node})} is \eqn{a}.

  When there are more than two states in the output varible,
  \code{\link{PnodeRules}}, \code{\link{PnodeAlphas}(\var{node})} and
  \code{\link{PnodeBetas}(\var{node})} become lists to indicate that a
  different value should be used for each transition between states.
  If there is a single value in the \dQuote{Rules} column, or
  equivalently the value of \code{\link{PnodeRules}} is a scalar, then
  the same rule is repeated for each state transition.  The same is true
  for \code{\link{PnodeAlphas}(\var{node})} and
  \code{\link{PnodeBetas}(\var{node})}.  If these values are a list,
  that indicates that a different value is to be used for each
  transition.  If they are a vector that means that different values (of
  discriminations for multiple-a rules or difficulties for multiple-b
  rules) are needed for the parent variables, but the same set of values
  is to be used for each state transition.  If different values are to
  be used then the values are a list of vectors.

  The necessary configuration of \eqn{a}'s and \eqn{b}'s depends on the
  type of link function.  Here are the rules for the currently existing
  link funcitons:
  \describe{
    \item{normal}{(\code{\link[CPTtools]{normalLink}}) This link function
    uses the same linear predictor for each transition, so there should be
    a single rule, and \code{\link{PnodeAlphas}(\var{node})} and
    \code{\link{PnodeBetas}(\var{node})} should both be vectors (with
    \eqn{b} of length 1 for a multiple-a rule).  This rule also requires a
    positive value for the \code{\link{PnodeLinkScale}(\var{node})} in the
    \dQuote{"LinkScale"} column.  The values in the \dQuote{A.\var{name}}
    and \dQuote{B.\var{name}} for rows after the first can be left as
    \code{NA}'s to indicate that the same values are reused.}

    \item{graded response}{(\code{\link[CPTtools]{gradedResponse}}) This
    link function models the probability of getting at or above each
    state and then calculates the differences between them to produce
    the conditional probability table.  In order to avoid negative
    probabilities, the probability of being in a higher state must
    always be nonincreasing.  The surest way to ensure this is to both
    use the same combination rules at each state and the same set of
    discrimination parameters for each state.  The difficulty parameters
    must be nondecreasing.  Again, values for rows after the first can
    be left as \code{NA}s to indicate that the same value should be
    resused.}
    
    \item{partial credit}{(\code{\link[CPTtools]{partialCredit}}) This
    link function models the conditional probability from moving from
    the previous state to the current state.  As such, there is no
    restriction on the rules or parameters.  In particular, it can
    alternate between multiple-a and multiple-b style rules from row to
    row. 

    Another restriction that the use of the partial credit rule lifts is
    the restriction that all parent variable must be used in each
    transition.  Note that there is one row of the \eqn{Q}-matrix (the
    inner \eqn{Q}-matrix) for each state transition.  Only the parent
    variables with 1's in the particular state row are considered when
    building the \code{\link{PnodeAlphas}(\var{node})} and
    \code{\link{PnodeBetas}(\var{node})} for this model.  Note that only
    the partial credit link function can take advantage of the multiple
    parents, the other two require all parents to be used for every
    state. }
  }

  The function \code{Qmat2Pnet} takes a data frame containing a Qmat
  sets the properties of the corresponding nodes to match the
  description in the Qmat.  It assumes that the proficiency variables
  have already been built, so it is almost always a good idea to first
  run \code{\link{Omega2Pnet}} to build the proficiency variables. 

  The function \code{Qmat2Pnet}  loops through the values in the
  \dQuote{Model} column, calling on the network \code{\link{Warehouse}}
  argument to supply (fetch or build) the requested network.  It then
  loops through the values in the \dQuote{Node} column, calling on the
  node \code{\link{Warehouse}} to supply them.  First, it attempts to
  adjust the parents of \var{node} to match the \eqn{Q}-matrix.  If the
  parent nodes are not in the current model, stub nodes are created by
  referencing the corresponding nodes in the proficiency model (the
  model corresponding to \code{\link{PnetHub}}).  If \code{override} is
  \code{TRUE}, the network will be modified so that \code{node} has the
  indicated parents; if it is \code{FALSE} an error will be signaled if
  the pattern in the \eqn{Q}-matrix does not match the network
  structure.  Then the values of various properties of a
  \code{\link{Pnode}}, in particular, the link function, the combination
  rules and the parameters, are set based on the values in Qmat (as
  described above).

}
\section{\eqn{Q}-Matrix (Qmat) Structure}{

  The output augmented \eqn{Q}-matrix is a data frame with the columns
  described below.  The number of columns is variable, with items marked
  \var{prof} actually corresponding to a number of columns with names
  taken from the proficiency variables (the \code{prof} argument).

  \describe{
  \item{Model}{The name of the \code{\link{Pnet}} in which the node
    in this row lives.}
  \item{Node}{The name of the \code{\link{Pnode}} described in this
    row.  Except for the multiple rows corresponding to the same node,
    the value of this column needs to be unique within \dQuote{Model}.}
  \item{Nstates}{The number of states for this node.  Generally, each
    node should have one fewer rows than this number.}
  \item{State}{The name of the state for this row.  This should be
    unique within the (\dQuote{Model},\dQuote{Node}) combination.}
  \item{Link}{The name of a link function.  This corresponds to
    \code{\link{PnodeLink}(\var{node})}.} 
  \item{LinkScale}{Either a positive number giving the link scale
    parameter or an \code{NA} if the link function does not need scale
    parameters.  This corresponds to
    \code{\link{PnodeLinkScale}(\var{node})}.}
  \item{\var{prof}}{There is one column for each proficiency variable.
    This corresponds to the structural part of the \eqn{Q}-matrix.
    There should be 1 in this column if the named proficiency is used in
    calculating the transition to this state for this particular node,
    and a 0 otherwise.}
  \item{Rules}{The name of the combination rule to use for this row.
    This corresponds to \code{\link{PnodeRules}(\var{node})}.}
  \item{A.\var{prof}}{There is one column for each proficiency with the
    proficiency name appended to \dQuote{A.}.  If a multiple-alpha style
    combination rule (e.g., \code{\link[CPTtools]{Compensatory}}) this
    column should contain the appropriate discriminations, otherwise,
    its value should be \code{NA}.}
  \item{A}{If a multiple-beta style
    combination rule (e.g., \code{\link[CPTtools]{OffsetConjuctive}}) this
    column should contain the single discrimination, otherwise,
    its value should be \code{NA}.}
  \item{B.\var{prof}}{There is one column for each proficiency with the
    proficiency name appended to \dQuote{B.}.  If a multiple-bet style
    combination rule (e.g., \code{\link[CPTtools]{OffsetConjuctive}}) this
    column should contain the appropriate difficulty (negative
    intercept), otherwise, its value should be \code{NA}.}
  \item{B}{If a multiple-beta style
    combination rule (e.g., \code{\link[CPTtools]{Compensatory}}) this
    column should contain the single difficulty (negative
    intercept), otherwise, its value should be \code{NA}.}
  \item{PriorWeight}{The amount of weight which should be given to the
    current values when learning conditional probability tables.  See
    \code{\link{PnodePriorWeight}}.} 
}
}
\value{

  Invisibly returns a list of models visited.

}
\section{Side Effects}{

  This function destructively modifies the networks and nodes referenced
  in the Qmat and supplied by the warehouses.

  Note that unlike typical R implementations, this is not necessarily
  safe.  In particular, if the Qmat references 10 models, and an error
  is raised when trying to modify the 5th model, the first 4 models will
  be modifid, the last 5 will not be and the 5th model may be partially
  modified.  This is different from most R functions where changes are
  not committed unless the function returns successfully.

}
\references{

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
\seealso{
  The inverse operation is \code{\link{Pnet2Qmat}}.

  See \code{\link{Warehouse}} for description of the network and node
  warehouse arguments.  The functions \code{\link{PnetMakeStubNodes}}
  and \code{\link{PnetRemoveStubNodes}} are used internally to create
  the stub nodes in evidence models.

  See \code{\link[CPTtools]{partialCredit}},
  \code{\link[CPTtools]{gradedResponse}}, and
  \code{\link[CPTtools]{normalLink}} for currently available link
  functions.  See \code{\link[CPTtools]{Conjunctive}} and
  \code{\link[CPTtools]{OffsetConjunctive}} for more information about
  available combination rules.
  
  The node attributes set from the Omega matrix include:
  \code{\link{PnodeParents}(\var{node})},
  \code{\link{PnodeLink}(\var{node})},
  \code{\link{PnodeLinkScale}(\var{node})},
  \code{\link{PnodeQ}(\var{node})},
  \code{\link{PnodeRules}(\var{node})},
  \code{\link{PnodeAlphas}(\var{node})},
  \code{\link{PnodeBetas}(\var{node})}, and
  \code{\link{PnodePriorWeight}(\var{node})}
}
\examples{

## Sample Q matrix
Q1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "miniPP-Q.csv", sep=.Platform$file.sep),
               stringsAsFactors=FALSE)

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

## Build the proficiency model first:
CM <- WarehouseSupply(Nethouse,"miniPP_CM")
CM1 <- Omega2Pnet(omegamat,CM,Nodehouse,override=TRUE,debug=TRUE)

## Build the nets from the Qmat

Qmat2Pnet(Q1, Nethouse,Nodehouse,debug=TRUE)

## Build the Qmat from the nets
## Generate a list of nodes
obs <-sapply(list(sess$nets$PPcompEM,sess$nets$PPconjEM,sess$nets$PPtwostepEM),
             NetworkAllNodes)

Q2 <- Pnet2Qmat(obs,NetworkAllNodes(CM),debug=TRUE)


stopSession(sess)
setwd(curd)
}
}
\keyword{ distribution }
\keyword{ graph }
