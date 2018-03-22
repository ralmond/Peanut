\name{Pnet2Qmat}
\alias{Pnet2Qmat}
\title{Makes an augmented Q-matrix from a collection of parameterized nets}
\description{
  In augmented \eqn{Q}-matrix, there is a set of rows for each
  \code{\link{Pnode}} which describes the conditional probability table
  for that node in terms of the model parameters (see
  \code{\link{BuildTable}}).  As the Pnodes could potentially come from
  multiple nets, the key for the table is (\dQuote{Model},
  \dQuote{Node}).  As there are multiple rows per node, \dQuote{State}
  is the third part of the key.

  The function \code{Pnet2} creates an augmented \eqn{Q}-matrix
  out of a collection of \code{Pnode}s, possibly spanning multiple
  \code{\link{Pnets}}.

}
\usage{
Pnet2Qmat(obs, prof, defaultRule = "Compensatory", defaultLink = "partialCredit", defaultAlpha = 1, defaultBeta = NULL, defaultLinkScale = NULL, debug = TRUE)
}
\arguments{
  \item{obs}{A list of \emph{observable} \code{\link{Pnode}} objects.
    These could span multiple \code{\link{Pnet}} objects.  Each element
    of this list will corresponded to one or more rows in the output
    \eqn{Q}-matrix. 
  }
  \item{prof}{A list of \emph{proficiency} \code{Pnode}s.  These are the
    parents of the \code{Pnode}s in the \code{obs} list.  Usually, these
    are all in a central proficiency or hub model.
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

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
function (obs, prof, defaultRule = "Compensatory", defaultLink = "partialCredit", 
    defaultAlpha = 1, defaultBeta = NULL, defaultLinkScale = NULL, 
    debug = TRUE) 
{
    statecounts <- sapply(obs, PnodeNumStates)
    obsnames <- sapply(obs, PnodeName)
    modnames <- sapply(obs, function(nd) PnetName(PnodeNet(nd)))
    profnames <- sapply(prof, PnodeName)
    rowcounts <- statecounts - 1
    Node <- rep(obsnames, rowcounts)
    Model <- rep(modnames, rowcounts)
    nrow <- length(Node)
    NStates <- rep(NA_integer_, nrow)
    State <- character(nrow)
    Link <- character(nrow)
    LinkScale <- rep(NA_real_, nrow)
    QQ <- matrix(NA_integer_, nrow, length(profnames))
    colnames(QQ) <- profnames
    Rules <- character(nrow)
    AA <- matrix(NA_real_, nrow, length(profnames))
    colnames(AA) <- profnames
    A <- rep(NA_real_, nrow)
    BB <- matrix(NA_real_, nrow, length(profnames))
    colnames(BB) <- profnames
    B <- rep(NA_real_, nrow)
    PriorWeight <- character(nrow)
    irow <- 1
    for (nd in obs) {
        tryCatch({
            if (debug) 
                cat("Processing node ", PnodeName(nd), ".\n")
            NStates[irow] <- nstate <- PnodeNumStates(nd)
            State[irow:(irow + nstate - 2)] <- PnodeStates(nd)[1:(nstate - 
                1)]
            if (is.null(PnodeLink(nd))) {
                Link[irow] <- defaultLink
            }
            else {
                Link[irow] <- as.character(PnodeLink(nd))
            }
            if (!is.null(PnodeLinkScale(nd))) {
                LinkScale[irow] <- as.character(PnodeLinkScale(nd))
            }
            else if (!is.null(defaultLinkScale)) {
                LinkScale[irow] <- as.character(defaultLinkScale)
            }
            QQ[irow:(irow + nstate - 2), ] <- 0
            QQ[irow:(irow + nstate - 2), profnames \%in\% PnodeParentNames(nd)] <- 1
            if (!is.null(PnodeQ(nd))) {
                QQ[irow:(irow + nstate - 2), match(PnodeParentNames(nd), 
                  profnames)] <- PnodeQ(nd)
            }
            QQQ <- QQ[irow:(irow + nstate - 2), match(ParentNames(nd), 
                profnames), drop = FALSE] == 1
            if (debug) {
                cat("Q matrix:\n")
                print(QQ[irow:(irow + nstate - 2), ])
                print(QQQ)
                cat("\n")
            }
            rules <- PnodeRules(nd)
            if (is.null(PnodeRules(nd))) {
                rules <- defaultRule
            }
            if (length(rules) == 1 || class(rules) == "function") {
                Rules[irow] <- dputToString(rules)
            }
            else {
                Rules[irow:(irow + nstate - 2)] <- sapply(rules, 
                  dputToString)
            }
            alpha <- PnodeAlphas(nd)
            if (is.null(alpha)) {
                alpha <- defaultAlpha
            }
            if (!is.list(alpha)) {
                alpha <- list(alpha)
                if (nstate > 2) {
                  for (i in 2:(nstate - 1)) {
                    alpha[[i]] <- alpha[[1]]
                  }
                }
            }
            beta <- PnodeBetas(nd)
            if (is.null(beta)) {
                beta <- defaultBeta
            }
            if (is.null(beta)) {
                beta <- as.list(effectiveThetas(nstate - 1))
            }
            if (!is.list(beta)) {
                beta <- list(beta)
                if (nstate > 2) {
                  for (i in 2:(nstate - 1)) {
                    beta[[i]] <- beta[[1]]
                  }
                }
            }
            if (length(rules) == 1 || class(rules) == "function") {
                rules <- list(rules)
                if (nstate > 2) {
                  for (i in 2:(nstate - 1)) {
                    rules[[i]] <- rules[[1]]
                  }
                }
            }
            for (i in 1:(nstate - 1)) {
                pnames <- PnodeParentNames(nd)[QQQ[i, ]]
                if (debug) {
                  cat("Rules[[", i, "]]:", toString(rules[[i]]), 
                    "\n")
                  cat("Parents[[", i, "]]:", pnames, "\n")
                }
                if (rules[[i]] \%in\% getOffsetRules()) {
                  a <- alpha[[i]]
                  bb <- beta[[i]]
                  if (is.null(names(bb))) {
                    if (length(bb) != length(pnames)) 
                      bb <- rep_len(bb, length(pnames))
                    names(bb) <- pnames
                  }
                  if (debug) {
                    cat("BB[[", i, "]] =", bb, " A[", i, "] = ", 
                      a, "\n")
                  }
                  BB[irow + i - 1, names(bb)] <- bb
                  A[irow + i - 1] <- a
                }
                else {
                  aa <- alpha[[i]]
                  b <- beta[[i]]
                  if (is.null(names(aa))) {
                    if (length(aa) != length(pnames)) 
                      aa <- rep_len(aa, length(pnames))
                    names(aa) <- pnames
                  }
                  if (debug) {
                    cat("AA[[", i, "]] =", aa, " B[", i, "] = ", 
                      b, "\n")
                  }
                  AA[irow + i - 1, names(aa)] <- aa
                  B[irow + i - 1] <- b
                }
            }
            if (debug) {
                cat("AA & A matrix:\n")
                print(cbind(AA[irow:(irow + nstate - 2), , drop = FALSE], 
                  A[irow:(irow + nstate - 2)]))
                cat("BB & B matrix:\n")
                print(cbind(BB[irow:(irow + nstate - 2), , drop = FALSE], 
                  B[irow:(irow + nstate - 2)]))
                cat("\n")
            }
            wt <- PnodePriorWeight(nd)
            if (!is.null(wt)) {
                PriorWeight[irow] <- dputToString(wt)
            }
        }, error = function(e) {
            ee <- simpleError(paste("While processing node ", 
                PnodeName(nd), ":", conditionMessage(e)), conditionCall(e))
            stop(ee)
        })
        irow <- irow + nstate - 1
    }
    colnames(AA) <- paste("A", colnames(AA), sep = ".")
    colnames(BB) <- paste("B", colnames(BB), sep = ".")
    result <- data.frame(Model, Node, NStates, State, Link, LinkScale, 
        QQ, Rules, AA, A, BB, B, PriorWeight, stringsAsFactors = FALSE)
    class(result) <- c("Qmat", class(result))
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
