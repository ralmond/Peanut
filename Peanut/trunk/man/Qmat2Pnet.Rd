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
  \code{\link[CPTtools}{Compensatory}}) which contained multiple alpha
  (discrimination) parameters, one for each parent variable,  and a
  single beta (difficulty).  The introduction of a new type of offset
  rule (e.g., \code{\link{OffsetDisjunctive}}) which uses a multiple
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
  \code{\link{CPTtools]{gradedResponse}} link function requires a
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
  the observable node.

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
  

  \code{\link{PnodeAlphas}(\var{node})} and \code{\link{PnodeBetas}(\var{node})}

%%  ~~ If necessary, more details than the description above ~~
}
\section{\eqn{Q}-Matrix (Qmat) Structure}{

  The output augmented \eqn{Q}-matrix is a data frame with the columns
  described below.  The number of columns is variable, with items marked
  \var{prof} actually corresponding to a number of columns with names
  taken from the proficiency variables (the \code{prof} argument).


  \item{Model}{The name of the \code{\link{Pnet}} in which the node
    in this row lives.}
  \item{Node}{The name of the \code{\link{Pnode}} described in this
    row.  Except for the multiple rows corresponding to the same node,
    the value of this column needs to be unique within \dQuote{Model}.}
  \item{Nstates}{The number of states for this node.  Generally, each
    node should have one fewer rows than this number.}
  \item{State}{The name of the state for this row.  This should be
    unique within the (\dQuote{Model},\dQuote{Node}) combination.  
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Qmat, nethouse, nodehouse, defaultRule = "Compensatory", 
    defaultLink = "partialCredit", defaultAlpha = 1, defaultBeta = NULL, 
    defaultLinkScale = NULL, debug = FALSE, override = FALSE) 
{
    if (!is.PnodeWarehouse(nodehouse)) {
        stop("Node warehouse must be supplied.")
    }
    if (!is.PnetWarehouse(nethouse)) {
        stop("Net warehouse must be supplied.")
    }
    if (!all(Qmat.reqcol \%in\% names(Qmat))) {
        stop("Badly formed Q matrix.")
    }
    Anames <- names(Qmat)[grep("A\\..*", names(Qmat))]
    profnames <- sapply(strsplit(Anames, ".", fixed = TRUE), 
        function(x) x[2])
    if (!all(profnames \%in\% names(Qmat))) {
        stop("Expected columns for proficiency variables:", profnames)
    }
    if (!all(paste("B", profnames, sep = ".") \%in\% names(Qmat))) {
        stop("Expected B columns for proficiency variables:", 
            profnames)
    }
    netnames <- unique(Qmat$Model)
    for (netname in netnames) {
        if (debug) {
            cat("Processing net ", netname, "\n")
        }
        net <- WarehouseSupply(nethouse, netname)
        if (is.null(net)) {
            stop("Could not find/create network ", netname)
        }
        hubname <- PnetHub(net)
        if (nchar(hubname) > 0) {
            hub <- WarehouseSupply(nethouse, hubname)
            if (is.null(hub)) {
                stop("Could not find hub for network ", netname, 
                  ",", "hubname")
            }
        }
        nodenames <- unique(Qmat$Node[Qmat$Model == netname])
        for (nodename in nodenames) {
            if (debug) {
                cat("Processing node ", nodename, "\n")
            }
            node <- WarehouseSupply(nodehouse, c(netname, nodename))
            if (is.null(node)) {
                stop("Could not find/create node ", nodename, 
                  "in model", netname)
            }
            tryCatch({
                Qrows <- Qmat[Qmat$Model == netname & Qmat$Node == 
                  nodename, , drop = FALSE]
                nstates <- nrow(Qrows)
                if (nstates != Qrows[1, "NStates"] - 1L) {
                  stop("Expected ", nstates != Qrows[1, "NStates"] - 
                    1L, "got", nstates)
                }
                stubs <- list()
                parnames <- names(which(apply(Qrows[, profnames, 
                  drop = FALSE] == 1, 2, any)))
                exparnames <- PnodeParentNames(node)
                if (!setequal(parnames, exparnames)) {
                  if (debug) {
                    cat("Node has parents: ", exparnames, "\n")
                    cat("But Q matrix has parents: ", parnames, 
                      "\n")
                  }
                  if (length(exparnames) > 0L) {
                    cat("While processing links for node: ", 
                      nodename, " in network", netname, "\n")
                    cat("Node has parents: ", exparnames, "\n")
                    cat("But Q matrix has parents: ", parnames, 
                      "\n")
                    if (override) {
                      warning("Changing net to match Q matrix.")
                    }
                    else {
                      stop("Graphical structure does not match Q matrix.  See console.")
                    }
                  }
                  pars <- list()
                  for (pname in setdiff(parnames, exparnames)) {
                    pars[[pname]] <- WarehouseSupply(nodehouse, 
                      c(hubname, pname))
                  }
                  stubs <- PnetMakeStubNodes(net, pars)
                  PnodeParents(node) <- c(PnodeParents(node), 
                    stubs)
                }
                parnames <- PnodeParentNames(node)
                if (debug) {
                  cat("Final parents: ", paste(parnames, collapse = ", "), 
                    ".\n")
                }
                ll <- Qrows[1, "Link"]
                if (is.na(ll) || nchar(ll) == 0L) 
                  ll <- defaultLink
                PnodeLink(node) <- ll
                lsc <- Qrows[1, "LinkScale"]
                if (is.na(lsc) || nchar(lsc) == 0L) 
                  lsc <- defaultLinkScale
                if (!is.null(lsc)) {
                  PnodeLinkScale(node) <- lsc
                }
                if (debug) {
                  cat("Link: ", ll, "(", lsc, "). \n")
                }
                rules <- Qrows[, "Rules"]
                nrules <- sum(nchar(rules) > 0L)
                if (nrules == 0L) {
                  rules <- defaultRule
                }
                if (nrules != 1L && nrules != nstates) {
                  stop("Expected ", nstates, " (or 1) rules but got ", 
                    nrules)
                }
                if (debug) {
                  cat("Rules: ", paste(rules, collapse = ", "), 
                    ".\n")
                }
                if (nrules == 1L) {
                  PnodeRules(node) <- dgetFromString(rules[[1]])
                }
                else {
                  PnodeRules(node) <- lapply(rules, dgetFromString)
                }
                rules <- PnodeRules(node)
                QrowsQ <- Qrows[, parnames]
                QrowsA <- Qrows[, paste("A", parnames, sep = ".")]
                QrowsB <- Qrows[, paste("B", parnames, sep = ".")]
                if (nstates == 1L) {
                  PnodeQ(node) <- TRUE
                  if (is.list(rules)) 
                    rules <- rules[[1]]
                  if (rules \%in\% getOffsetRules()) {
                    alphas <- Qrows[1, "A"]
                    if (is.na(alphas)) 
                      alphas <- defaultAlpha
                    PnodeAlphas(node) <- alphas
                    betas <- as.numeric(QrowsB)
                    if (all(is.na(betas))) 
                      betas <- rep(defaultBeta, length(parnames))
                    names(betas) <- parnames
                    PnodeBetas(node) <- betas
                    if (debug) {
                      cat("Single Row offset: \n")
                      print(PnodeQ(node))
                      print(PnodeAlphas(node))
                      print(PnodeBetas(node))
                    }
                  }
                  else {
                    alphas <- as.numeric(QrowsA)
                    if (all(is.na(alphas))) 
                      alphas <- rep(defaultAlpha, length(parnames))
                    names(alphas) <- parnames
                    PnodeAlphas(node) <- alphas
                    betas <- Qrows[1, "B"]
                    if (is.na(betas)) 
                      betas <- defaultBeta
                    PnodeBetas(node) <- betas
                    if (debug) {
                      cat("Single Row weighted: \n")
                      print(PnodeQ(node))
                      print(PnodeAlphas(node))
                      print(PnodeBetas(node))
                    }
                  }
                }
                else {
                  QrowsQ <- as.matrix(QrowsQ)
                  QrowsA <- as.matrix(QrowsA)
                  QrowsB <- as.matrix(QrowsB)
                  if (length(rules) == 1L && !(rules \%in\% getOffsetRules()) && 
                    all(is.na(QrowsA[-1, ])) && all(QrowsQ)) {
                    PnodeQ(node) <- TRUE
                    alphas <- as.numeric(QrowsA[1, ])
                    if (all(is.na(alphas))) 
                      alphas <- rep(defaultAlpha, length(parnames))
                    names(alphas) <- parnames
                    PnodeAlphas(node) <- alphas
                    betas <- Qrows[, "B"]
                    if (is.na(betas)) 
                      betas <- rep(defaultBeta, nstates)
                    PnodeBetas(node) <- as.list(betas)
                    if (debug) {
                      cat("Multiple Row weighted: \n")
                      print(PnodeQ(node))
                      print(PnodeAlphas(node))
                      print(PnodeBetas(node))
                    }
                  }
                  else {
                    QrowsQ <- QrowsQ == 1
                    PnodeQ(node) <- QrowsQ
                    if (length(rules) < nstates) {
                      for (i in 2:nstates) {
                        rules[[i]] <- rules[[1]]
                      }
                    }
                    alphas <- list()
                    betas <- list()
                    for (istate in 1:nstates) {
                      npar <- sum(QrowsQ[istate, ])
                      if (rules[[istate]] \%in\% getOffsetRules()) {
                        bb <- as.numeric(QrowsB[istate, QrowsQ[istate, 
                          ]])
                        if (all(is.na(bb))) 
                          bb <- rep(defaultBeta, npar)
                        names(bb) <- parnames[QrowsQ[istate, 
                          ]]
                        betas[[istate]] <- bb
                        aa <- Qrows[istate, "A"]
                        if (is.na(aa)) 
                          aa <- defaultAlpha
                        alphas[[istate]] <- aa
                      }
                      else {
                        aa <- as.numeric(QrowsA[istate, QrowsQ[istate, 
                          ]])
                        if (all(is.na(aa))) 
                          aa <- rep(defaultAlpha, npar)
                        names(aa) <- parnames[QrowsQ[istate, 
                          ]]
                        alphas[[istate]] <- aa
                        bb <- Qrows[istate, "B"]
                        if (is.na(bb)) 
                          bb <- defaultBeta
                        betas[[istate]] <- bb
                      }
                    }
                    PnodeAlphas(node) <- alphas
                    PnodeBetas(node) <- betas
                    if (debug) {
                      cat("Multiple Row mixed: \n")
                      print(PnodeRules(node))
                      print(PnodeQ(node))
                      print(PnodeAlphas(node))
                      print(PnodeBetas(node))
                    }
                  }
                }
                wt <- Qrows[1, "PriorWeight"]
                if (!is.na(wt) && nchar(wt) >= 0L) {
                  PnodePriorWeight(node) <- dgetFromString(wt)
                  if (debug) {
                    cat("Prior Weight: ", wt, ".\n")
                  }
                }
                if (length(stubs) > 0L) 
                  PnetRemoveStubNodes(net, stubs)
            }, error = function(e) {
                ee <- simpleError(paste("While processing node ", 
                  nodename, "in model ", netname, ":", conditionMessage(e)), 
                  conditionCall(e))
                stop(ee)
            })
        }
    }
    invisible(netnames)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
