
### Takes a character vector and puts it into a
### character scalar of quoted strings separated by commans.
statecat <- function (states)
  paste("\"",NodeStates(phys),"\"",collapse=",",sep="")

statesplit <- function (statestring)
  strsplit(sub("^\"","",sub("\"$","",statestring)),"\",\"")[[1]]


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


### Create table of Node meta-information



### Function to build a blank Q-matrix from a Bayes net.
Pnet2Qmat <- function (pnet,obs,prof,defaultRule="Compensatory",
                       defaultLink="partialCredit",defaultAlpha=1,
                       defaultBeta=NULL,defaultLinkScale=NULL,
                       debug=TRUE) {
  statecounts <- sapply(obs,PnodeNumStates)
  obsnames <- sapply(obs,PnodeName)
  profnames <- sapply(prof,PnodeName)

  ## Lay out node name row with proper repetition structure
  rowcounts <- statecounts-1
  Node <- rep(obsnames,rowcounts)
  nrow <- length(Node)

  ## Now lay out blank structure of rest of Qmat
  NStates <- rep(NA_integer_,nrow)
  States <- character(nrow)
  Link <- character(nrow)
  LinkScale <- numeric(nrow)
  QQ <- matrix(NA_real_,nrow,length(profnames))
  colnames(QQ) <- profnames
  Rules <- character(nrow)
  A <- matrix(NA_real_,nrow,length(profnames))
  colnames(A) <- profnames
  B <- rep(NA_real_,nrow)
  PriorWeight <- character(nrow)

  ## Now loop over vars, processing each one.
  irow <- 1
  for (nd in obs) {
    if (debug) cat("Processing node ",nd,".\n")
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
      ## QQ -- if not supplied, fill in first row according to parents.
      ## If supplied, reproduce matrix.
      ## First fill in non-parents with 0s
      QQ[irow:(irow+nstate-2),] <- 0
      QQ[irow:(irow+nstate-2),profnames %in% PnodeParentNames(nd)] <- 1
      if (!is.null(PnodeQ(nd))) {
        QQ[irow:(irow+nstate-2),match(ParentNames(nd),profnames)] <-
          PnodeQ(nd)
      }
      if (debug) {
        cat("Q matrix:\n")
        print(QQ[irow:(irow+nstate-2),])
        cat("\n")
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
      alpha <- PnodeAlphas(nd)
      if (is.null(alpha)) {
        alpha <- defaultAlpha
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
          if (rule %in% getOffsetRules()) {
            a[[i]] <- beta[[i]]
            b[[i]] <- alpha[[i]]
          }
          i <- i+1
        }
      }
      if(debug) {
        print(nd)
        cat("Rules:",toString(PnodeRules(nd)),"\n")
        cat("a=",toString(a),"\n")
        cat("b=",toString(b),"\n")
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
      if (debug) {
        cat("A matrix:\n")
        print(A[irow:(irow+nstate-2),])
        cat("B vector:\n")
        print(B[irow:(irow+nstate-2)])
        cat("\n")
      }

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
  ## Fix colnames(A) so they are different from colnames(QQ)
  colnames(A) <- paste("A",colnames(A),sep=".")
  result <- data.frame(Node,NStates,States,Link,LinkScale,QQ,Rules,
                       A,B,PriorWeight,
                       stringsAsFactors=FALSE)
  class(result) <- c("Qmat",class(result))
  result
}


Qmat.reqcol <- c("Node","Nstates","States","Link","LinkScale"
          "Rules","B","PriorWeight")


Qmat2Pnet <- function (QQ, nethouse,nodehouse,prof,defaultRule="Compensatory",
                       defaultLink="partialCredit",defaultAlpha=1,
                       defaultBeta=NULL,defaultLinkScale=NULL,
                       debug=FALSE,override=FALSE) {

  ### HERE ###
}




### Omega matrix for proficiencies.


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
    if (!all(PnodeParentNames(nd) %in% profnames)) {
      stop("Some of the parents of node ",nd," are not in the list.")
    }
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
    States[pname] <- statecat(PnodeStates(nd))
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
Omega2Pnet <- function(OmegaMat,pn,nodewarehouse,defaultRule="Compensatory",
                       defaultLink="normalLink",defaultAlpha=1,
                       defaultBeta=0,defaultLinkScale=1,
                       defaultWeight=NULL,
                       debug=FALSE,override=FALSE) {
  if (!is.Pnet(pn)) {
    stop("Blank network must be provided.")
  }
  if (!is(nodewarehouse,"PnodeWarehouse")) {
    stop("Node warehouse must be supplied.")
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
  rownames(OmegaMat) <- nodenames


  ## Container for list of proficiency variables
  if (debug) {
    cat("\nBuilding nodes.\n")
  }
  nodes <- vector("list",length(nodenames))
  names(nodes) <- nodenames
  for (ndn in nodenames) {
    if (debug) cat("Building node: ",ndn,"\n")
    node <- nodewarehouse$supply(c(PnetName(pn),ndn))
    ## Check state match
    statesEqual <- all.equal(PnodeStates(node),statesplit(States[ndn]))
    if (statesEqual != TRUE) {
      cat("States of node and Omega matrix not equal for node ",ndn,"\n")
      cat(statesEqual,"\n")
      if (override) {
        warning("Correcting net to match matrix.  This may damage meta-data.")
        PnodeStates(node) <- statesplit(States[ndn])
      } else {
        stop("Network and Omega matrix do not agree.  See console for details.")
      }
    }
    nodes[[ndn]] <- node
  }

  ### Next build structure.
  if (debug) {
    cat("\n Processing links.\n")
  }
  QQ <- OmegaMat[nodenames,nodenames]

  for (ndn in nodenames) {
    if (debug) cat("Building links for node: ",ndn,"\n")
    node <- nodes[[ndn]]
    parnames <- setdiff(nodesnames(QQ[ndn,]==1),ndn)
    exparnames <- PnodeParentNames(node)
    if (!setequal(parnames,exparnames)) {
      cat("While processing links for node: ",ndn,"\n")
      cat("Node has parents: ", exparnames,"\n")
      cat("But Omega matrix has parents: ", parnames,"\n")
      if (override) {
        warning("Changing net to match Omega matrix.")
      } else {
        stop("Graphical structure does not match Omega matrix.  See console.")
      }
    }
    # Change order to match matrix. Even if nominally a match.
    PnodeParents(node) <- nodes[parnames]
  }

  ### Next build structure.
  if (debug) {
    cat("\n Processing CPTs.\n")
  }
  AOmega <- OmegaMat[nodenames,paste("A",nodenames,sep=".")]
  colnames(AA) <- nodenames
  links <- OmegaMat$Link
  names(links) <- nodenames
  rules <- OmegaMat$Rules
  names(rules) <- nodenames
  intercepts <- OmegaMat$Intercept
  names(intercepts) <- nodenames
  weights <- OmegaMat$PriorWeight
  names(weights) <- nodenames

  for (ndn in nodenames) {
    if (debug) cat("Building links for node: ",ndn,"\n")
    node <- nodes[[ndn]]
    parnames <- setdiff(nodesnames(QQ[ndn,]==1),ndn)
    if (is.na(rules[ndn]) || nchar(rules[ndn]) == 0L) {
      PnodeRules(node) <- defaultRule
    } else {
      PnodeRules(node) <- rules[ndn]
    }
    if (is.na(rules[ndn]) || nchar(links[ndn]) == 0L) {
      PnodeLink(node) <- defaultLink
    } else {
      PnodeLink(node) <- links[ndn]
    }
    if (is.na(intercept[ndn])) {
      PnodeBetas(node) <- defaultBeta
    } else {
      PnodeBetas(node) <- intercept[ndn]
    }
    if (is.na(AOmega[ndn,ndn])) {
      PnodeLinkScale(node) <- defaultLinkScale
    } else {
      PnodeLinkScale(node) <- AOmega[ndn,ndn]
    }
    parnames <- PnodeParentNames(nd)
    alphas <- AOmega[pname,parnames]
    alphas[is.na(alphas)] <- defaultAlphas
    names(alphas) <- parnames
    PnodeAlphas(node) <- alphas

    if (!is.na(weights[ndn]) && nchar(weights[ndn]) > 0L) {
    PnodePriorWeight(node) <- dgetFromString(weights)
    }

  }
  pn

}

