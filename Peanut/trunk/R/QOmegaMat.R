


## This is clipboard inheritance from RNetica, but as Pnets is not
## supposed to depend on RNetica, this seems the easiest way to do this.
dputToString <- function (obj) {
  con <- textConnection(NULL,open="w")
  tryCatch({dput(obj,con);
           textConnectionValue(con)},
           finally=close(con))
}

dgetFromString <- function (str) {
  con <- textConnection(str,open="r")
  tryCatch(dget(con), finally=close(con))
}




### Function to build a blank Q-matrix from a Bayes net.
Pnet2Qmat <- function (pnet,obs,prof) {
  statecounts <- sapply(obs,PnodeNumStates)
  obsnames <- sapply(obs,PnodeName)
  profnames <- sapply(prof,PnodeName)

  ## Lay out node name row with proper repetition structure
  rowcounts <- statecounts-1
  Node <- rep(names(statecounts),rowcounts)
  nrow <- length(Node)

  ## Now lay out blank structure of rest of Qmat
  NStates <- rep(NA_integer_,nrow)
  States <- character(nrow)
  Link <- character(nrow)
  LinkScale <- numeric(nrow)
  Q <- matrix(NA_real_,nrow,length(profnames))
  colnames(Q) <- profnames
  Rules <- character(nrow)
  A <- matrix(NA_real_,nrow,length(profnames))
  colnames(A) <- profnames
  B <- rep(NA_real_,nrow)
  PriorWeight <- character(nrow)

  ## Now loop over vars, processing each one.
  irow <- 1
  for (nd in obs) {
    ## first row
    NStates[irow] <- nstate <- PnodeNumStates(nd)
    States[irow:(irow+nstate-2)] <- PnodeStates(nd)[1:(nstate-1)]
    Link[irow] <- as.character(PnodeLink(nd))
    if (!is.null(PnodeLinkScale(nd))) {
      LinkScale[irow] <- as.character(PnodeLinkScale(nd))
    }
    ## Q -- if not supplied, fill in first row according to parents.
    ## If supplied, reproduce matrix.
    if (is.null(PnodeQ(nd))) {
      Q[irow,] <- as.numeric(profnames %in% PnodeParentNames(nd))
    } else {
      Q[irow:(irow+nstate-2),] <- PnodeQ(nd)
    }
    if (length(PnodeRules(nd)) == 1 ||
        class(PnodeRules(nd))=="function") {
      Rules[irow] <- dputToString(PnodeRules(nd))
    } else {
      Rules[irow:(irow+nstate-2)] <- sapply(PnodeRules(nd),dputToString)
    }
    ### HERE
    ## A  -- LnAlpha or Beta if Rule is OffsetXXX
    ## B -- Beta or LnAlpha if Rule is OffsetXXX
    if (length(PnodeRules(nd)) ==1 ||
        class(PnodeRules(nd))== "function") {
      if (PnodeRules(nd) %in% getOffsetRules()) {
        a <- PnodeLnAlphas(nd)
        b <- PnodeBetas(nd)
      } else {
        a <- PnodeLnAlphas(nd)
        b <- PnodeBetas(nd)
      }
    } else {
      ## Different rule for each row of table, may need different A's
      ## and B's
      alpha <- PnodeLnAlphas(nd)
      beta <- PnodeBetas(nd)
      if (!is.list(alpha)) {
        alpha <- list(alpha)
        if (nstate>2) {
          for (i in 2:(nstate-1)) {
            alpha[[i]] <- alpha[[1]]
          }
        }
      }
      if (!is.list(beta)) {
        beta <- list(beta)
        if (nstate>2) {
          for (i in 2:(nstate-1)) {
            beta[[i]] <- beta[[1]]
          }
        }
      }
      a <- alpha; b <- beta
      i <- 1
      for (rule in PnodeRules(nd)) {
        if (rule %in% OffsetRules) {
          a[[i]] <- beta[[i]]
          b[[i]] <- alpha[[i]]
        }
        i <- i+1
      }
    }
    ## Now write out a
    ## Easier to just handle list case
    if (!is.list(a)) a <- list(a)
    for (i in 1:length(a)) {
      aa <- a[[i]]
      if (is.null(names(aa))) {
        names(aa) <- PnodeParentNames(nd)
      }
      A[irow+i-1,names(aa)] <- aa
    }
    ## For b, easier to coerce to vector.
    b <- as.numeric(b)
    if (length(b) > nstate-1) {
      stop("Too much b:", b)
    }
    B[irow:(irow+length(b)-1)] <- b

    ## Weights
    wt <- PnodePriorWeight(nd)
    if (!is.null(wt)) {
      PriorWeight[irow] <- dputToString(wt)
    }

    ## Next node
    irow <- irow + nstate-1
  }
  ## Finally, put this togehter into a data frame
  ## Fix colnames(A) so they are different from colnames(Q)
  colnames(A) <- paste("A",colnames(A),sep=".")
  result <- data.frame(Node,NStates,States,Link,LinkScale,Q,Rules,
                       A,B,PriorWeight)
  class(result) <- c("Qmat",class(result))
  result
}

Pnet2Omega <- function(net,prof) {
  p <- length(prof)
  statecounts <- sapply(prof,PnodeNumStates)
  profnames <- sapply(prof,PnodeName)

  ## Find a natural order so the Q matrix is lower triangular.
  Omega <- diag(p)
  rownames(Omega) <- profnames
  colnames(Omega) <- colnames

  for (nd in prof) {
    Omega[PnodeName(nd), PnodeParentNames(prof)] <- 1
  }
  ord <- topsort(Omega)
  Omega <- Omega[ord,ord]


}

## Takes an incidence matrix and produces a sorted ordering so that the parent
## value is always higher in the ordering than a child.
topsort <- function (Omega) {
  if (nrow(Omega) != ncol(Omega)) {
    stop("Matrix must be square.")
  }
  ord <- numeric()
  cols <- 1:ncol(Omega)
  if (!is.null(colnames(Omega))) {
    names(cols) <- colnames(Omega)
  }
  while (nrow(Omega) > 0) {
    rsum <- apply(Omega,1,sum)
    priors <- which(rsum==1)
    print("Omega so far:")
    print(Omega)
    if (length(priors) == 0) {
      stop("Graph is cyclic.")
    }
    ord <- c(ord,cols[priors])
    cols <- cols[-priors,drop=FALSE]
    Omega <- Omega[-priors,-priors,drop=FALSE]
  }
  ord
}
  
  
  
