


## This is clipboard inheritance from RNetica, but as Pnets is not
## supposed to depend on RNetica, this seems the easiest way to do this.
dputToString <- function (obj) {
  con <- textConnection(NULL,open="w")
  result <- tryCatch({dput(obj,con);
    textConnectionValue(con)},
    finally=close(con))
  ## R is helpfully adding newlines which we don't want.
  paste(result,collapse="")
}

dgetFromString <- function (str) {
  con <- textConnection(str,open="r")
  tryCatch(dget(con), finally=close(con))
}




### Function to build a blank Q-matrix from a Bayes net.
Pnet2Qmat <- function (pnet,obs,prof,defaultRule="Compensatory",
                       defaultLink="partialCredit",defaultLnAlpha=0,
                       defaultBeta=NULL,defaultLinkScale=NULL,
                       debug=FALSE) {
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
    tryCatch({
      ## first row
      NStates[irow] <- nstate <- PnodeNumStates(nd)
      States[irow:(irow+nstate-2)] <- PnodeStates(nd)[1:(nstate-1)]
      if (is.null(PnodeLink(nd))) {
        Link[irow] <- defaultLink
      } else {
        Link[irow] <- as.character(PnodeLink(nd))
      }
      if (!is.null(PnodeLinkScale(nd))) {
        LinkScale[irow] <- as.character(PnodeLinkScale(nd))
      } else if (!is.null(defaultLinkScale)) {
        LinkScale[irow] <- as.character(defaultLinkScale)
      }
      ## Q -- if not supplied, fill in first row according to parents.
      ## If supplied, reproduce matrix.
      if (is.null(PnodeQ(nd))) {
        Q[irow,] <- as.numeric(profnames %in% PnodeParentNames(nd))
      } else {
        Q[irow:(irow+nstate-2),] <- PnodeQ(nd)
      }
      rules <- PnodeRules(nd)
      if (is.null(PnodeRules(nd))) {
        rules <- defaultRule
      }
      if (length(rules) == 1 ||
          class(rules)=="function") {
        Rules[irow] <- dputToString(rules)
      } else {
        Rules[irow:(irow+nstate-2)] <- sapply(rules,dputToString)
      }
      ## Get ln(alpha) or default ln(alpha) value
      alpha <- PnodeLnAlphas(nd)
      if (is.null(alpha)) {
        alpha <- defaultLnAlpha
      }
      beta <- PnodeBetas(nd)
      if (is.null(beta)) {
        beta <- defaultBeta
      }
      if (is.null(beta)) {
        ## Make up based on number of states
        beta <- as.list(effectiveThetas(nstate-1))
      }
      ## A  -- LnAlpha or Beta if Rule is OffsetXXX
      ## B -- Beta or LnAlpha if Rule is OffsetXXX

      if (length(rules) ==1 ||
          class(rules)== "function") {
        if (rules %in% getOffsetRules()) {
          b <- alpha
          a <- beta
        } else {
          a <- alpha
          b <- beta
        }
      } else {
        ## Different rule for each row of table, may need different A's
        ## and B's
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
        for (rule in rules) {
          if (rule %in% OffsetRules) {
            a[[i]] <- beta[[i]]
            b[[i]] <- alpha[[i]]
          }
          i <- i+1
        }
      }
      if(debug) {
        print(nd)
        cat("Rules:",PnodeRules(nd),"\n")
        cat("a=",a,"\n")
        cat("b=",b,"\n")
      }
      ## Now write out a
      ## Easier to just handle list case
      if (!is.list(a)) a <- list(a)
      for (i in 1:length(a)) {
        aa <- a[[i]]
        if (is.null(names(aa))) {
          if (length(aa) != PnodeNumParents(nd)) {
            aa <- rep_len(aa,PnodeNumParents(nd))
          }
          names(aa) <- PnodeParentNames(nd)
        }
        if (debug) {
          print("a[[i]]")
          print(aa)
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
    },
    error = function (e) {
      ## Resignal error with context information about the node.
      ee <- simpleError(
          paste("While processing node ",PnodeName(nd),":",
                conditionMessage(e)),
          conditionCall(e))
      stop(ee)
    })
    ## Next node
    irow <- irow + nstate-1
  }
  ## Finally, put this togehter into a data frame
  ## Fix colnames(A) so they are different from colnames(Q)
  colnames(A) <- paste("A",colnames(A),sep=".")
  result <- data.frame(Node,NStates,States,Link,LinkScale,Q,Rules,
                       A,B,PriorWeight,
                       stringsAsFactors=FALSE)
  class(result) <- c("Qmat",class(result))
  result
}

Pnet2Omega <- function(net,prof, defaultRule="Compensatory",
                       defaultLink="normalLink",defaultAlpha=1,
                       defaultBeta=0,defaultLinkScale=1,
                       debug=FALSE) {
  p <- length(prof)
  statecounts <- sapply(prof,PnodeNumStates)
  profnames <- sapply(prof,PnodeName)

  ## Find a natural order so the Q matrix is lower triangular.
  Omega <- diag(p)
  rownames(Omega) <- profnames
  colnames(Omega) <- profnames

  for (nd in prof) {
    Omega[PnodeName(nd), PnodeParentNames(nd)] <- 1
  }
  ord <- topsort(Omega,noisy=debug)
  Omega <- Omega[ord,ord]
  profnames <- rownames(Omega)

  ## Now set up the Rows and columns.
  Nstates <- numeric(p)
  names(Nstates) <- profnames
  States <- character(p)
  names(States) <- profnames
  Rules <- rep(defaultRule,p)
  names(Rules) <- profnames
  Link <- rep(defaultLink,p)
  names(Link) <- profnames
  Intercept <- rep(defaultBeta,p)
  names(Intercept) <- profnames
  AOmega <- Omega
  diag(AOmega) <- defaultLinkScale
  PriorWeight <- character(p)
  names(PriorWeight) <- profnames

  ## Loop throught the nodes, filling in fields
  for (nd in prof) {
    pname <- PnodeName(nd)
    Nstates[pname] <- PnodeNumStates(nd)
    States[pname] <- dputToString(PnodeStates(nd))
    if (!is.null(PnodeRules(nd)))
      Rules[pname] <- as.character(PnodeRules(nd))
    if (!is.null(PnodeLink(nd)))
      Link[pname] <- as.character(PnodeLink(nd))
    if (!is.null(PnodeBetas(nd)))
      Intercept[pname] <- as.numeric(PnodeBetas(nd))
    if (!is.null(PnodeLinkScale(nd))) {
      AOmega[pname,pname] <-as.numeric(PnodeLinkScale(nd))
    }
    parnames <- PnodeParentNames(nd)
    if (is.null(PnodeAlphas(nd))) {
      AOmega[pname,parnames] <- defaultAlpha
    } else {
      AOmega[pname,parnames] <- as.numeric(PnodeAlphas(nd))
    }
    wt <- PnodePriorWeight(nd)
    if (!is.null(wt)) {
      PriorWeight[pname] <- dputToString(wt)
    }

  }

  colnames(AOmega) <- paste("A",colnames(AOmega),sep=".")
  result <- data.frame(Node=profnames,Nstates,States,Omega,
                       Link,Rules,AOmega,Intercept,PriorWeight,
                       stringsAsFactors=FALSE)
  class(result) <- c("OmegMat",class(result))
  result

}

## Takes an incidence matrix and produces a sorted ordering so that the parent
## value is always higher in the ordering than a child.
topsort <- function (Omega,noisy=FALSE) {
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
    if (noisy) {
      print("Omega so far:")
      print(Omega)
    }
    if (length(priors) == 0) {
      stop("Graph is cyclic.")
    }
    ord <- c(ord,cols[priors])
    cols <- cols[-priors,drop=FALSE]
    Omega <- Omega[-priors,-priors,drop=FALSE]
  }
  ord
}

### These columns should be in any Omega matrix.
Omega.reqcol <- c("Node","Nstates","States","Link",
          "Rules","Intercept","PriorWeight")

## Override controls behavior when OmegaMat and pn don't agree.
## Default  value of FALSE generates an error.
## A value of true issues a warning and changes the network to agree
## with the matrix.
Omega2Pnet <- function(OmegaMat,pn,defaultRule="Compensatory",
                       defaultLink="normalLink",defaultAlpha=1,
                       defaultBeta=0,defaultLinkScale=1,
                       debug=FALSE,override=FALSE) {
  if (!is.Pnet(pn)) {
    stop("Blank network must be provided.")
  }
  if (!all(Omega.reqcol %in% names(OmegaMat))) {
    stop("Badly formed Omega matrix.")
  }
  ## First parse the structural part of the matrix
  nodenames <- OmegaMat$Node
  Nstates <- OmegaMat$Nstates
  names(Nstates) <- nodenames
  States <- OmegaMat$States
  names(States) <- nodenames
  p <- nrow(OmegaMat)

  ## Container for list of proficiency variables
  profs <- list()
  for (ndn in nodenames) {
  }



}

Qmat2Pnet <- function (QQ, pnet,prof,defaultRule="Compensatory",
                       defaultLink="partialCredit",defaultLnAlpha=0,
                       defaultBeta=NULL,defaultLinkScale=NULL,
                       debug=FALSE,override=FALSE) {

}