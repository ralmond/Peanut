
### Takes a character vector and puts it into a
### character scalar of quoted strings separated by commans.
statecat <- function (states)
  paste("\"",states,"\"",collapse=",",sep="")

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
Pnet2Qmat <- function (obs,prof,defaultRule="Compensatory",
                       defaultLink="partialCredit",defaultAlpha=1,
                       defaultBeta=NULL,defaultLinkScale=NULL,
                       debug=TRUE) {
  statecounts <- sapply(obs,PnodeNumStates)
  obsnames <- sapply(obs,PnodeName)
  modnames <- sapply(obs,function(nd) PnetName(PnodeNet(nd)))
  profnames <- sapply(prof,PnodeName)

  ## Lay out node name row with proper repetition structure
  rowcounts <- statecounts-1
  Node <- rep(obsnames,rowcounts)
  Model <- rep(modnames,rowcounts)
  nrow <- length(Node)

  ## Now lay out blank structure of rest of Qmat
  NStates <- rep(NA_integer_,nrow)
  State <- character(nrow)
  Link <- character(nrow)
  LinkScale <- rep(NA_real_,nrow)
  QQ <- matrix(NA_integer_,nrow,length(profnames))
  colnames(QQ) <- profnames
  Rules <- character(nrow)
  AA <- matrix(NA_real_,nrow,length(profnames))
  colnames(AA) <- profnames
  A <- rep(NA_real_,nrow)
  BB <- matrix(NA_real_,nrow,length(profnames))
  colnames(BB) <- profnames
  B <- rep(NA_real_,nrow)
  PriorWeight <- character(nrow)

  ## Now loop over vars, processing each one.
  irow <- 1
  for (nd in obs) {
    tryCatch({
      if (debug) cat("Processing node ",PnodeName(nd),".\n")
      ## first row
      NStates[irow] <- nstate <- PnodeNumStates(nd)
      State[irow:(irow+nstate-2)] <- PnodeStates(nd)[1:(nstate-1)]
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
      QQQ <-QQ[irow:(irow+nstate-2),
               match(ParentNames(nd),profnames),
               drop=FALSE] == 1        # as.logical drops dims!
      if (debug) {
        cat("Q matrix:\n")
        print(QQ[irow:(irow+nstate-2),])
        print(QQQ)
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
      if (!is.list(alpha)) {            #Replicate to length nstate-1
        alpha <- list(alpha)
        if (nstate>2) {
          for (i in 2:(nstate-1)) {
            alpha[[i]] <- alpha[[1]]
          }
        }
      }
      beta <- PnodeBetas(nd)
      if (is.null(beta)) {
        beta <- defaultBeta
      }
      if (is.null(beta)) {
        ## Make up based on number of states
        beta <- as.list(effectiveThetas(nstate-1))
      }
      if (!is.list(beta)) {             #Replicate to length nstate-1
        beta <- list(beta)
        if (nstate>2) {
          for (i in 2:(nstate-1)) {
            beta[[i]] <- beta[[1]]
          }
        }
      }
      ## AA -- Alphas for non-offset rule
      ## A  -- Alpha if Rule is OffsetXXX
      ## B -- Beta if Rule is not OffsetXXX
      ## BB -- Betas if Rule is OffsetXXX
      if (length(rules) ==1 || class(rules)== "function") {
        ## Replicate out to length nstate-1
        rules <- list(rules)
        if (nstate>2) {
          for (i in 2:(nstate-1)) {
            rules[[i]] <- rules[[1]]
          }
        }
      }
      for (i in 1:(nstate-1)) {
        pnames <- PnodeParentNames(nd)[QQQ[i,]]
        if(debug) {
          cat("Rules[[",i,"]]:",toString(rules[[i]]),"\n")
          cat("Parents[[",i,"]]:",pnames,"\n")
        }
        if (rules[[i]] %in% getOffsetRules()) {
          ## Use BB and A
          a <- alpha[[i]]
          bb <- beta[[i]]
          if (is.null(names(bb))) {
            if (length(bb) != length(pnames))
              bb <- rep_len(bb,length(pnames))
            names(bb) <- pnames
          }
          if (debug) {
            cat("BB[[",i,"]] =",bb," A[",i,"] = ",a,"\n")
          }
          BB[irow+i-1,names(bb)] <- bb
          A[irow+i-1] <- a
        } else {
          ## Use AA and B
          aa <- alpha[[i]]
          b <- beta[[i]]
          if (is.null(names(aa))) {
            if (length(aa) != length(pnames))
              aa <- rep_len(aa,length(pnames))
            names(aa) <- pnames
          }
          if (debug) {
            cat("AA[[",i,"]] =",aa," B[",i,"] = ",b,"\n")
          }
          AA[irow+i-1,names(aa)] <- aa
          B[irow+i-1] <- b
        }
      }                                 #Next state
      if (debug) {
        cat("AA & A matrix:\n")
        print(cbind(AA[irow:(irow+nstate-2),,drop=FALSE],
                    A[irow:(irow+nstate-2)]))
        cat("BB & B matrix:\n")
        print(cbind(BB[irow:(irow+nstate-2),,drop=FALSE],
                    B[irow:(irow+nstate-2)]))
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
  colnames(AA) <- paste("A",colnames(AA),sep=".")
  colnames(BB) <- paste("B",colnames(BB),sep=".")
  result <- data.frame(Model,Node,NStates,State,Link,LinkScale,QQ,Rules,
                       AA,A,BB,B,PriorWeight,
                       stringsAsFactors=FALSE)
  class(result) <- c("Qmat",class(result))
  result
}


Qmat.reqcol <- c("Model","Node","NStates","State","Link","LinkScale",
          "Rules","A","B","PriorWeight")


Qmat2Pnet <- function (Qmat, nethouse,nodehouse,defaultRule="Compensatory",
                       defaultLink="partialCredit",defaultAlpha=1,
                       defaultBeta=NULL,defaultLinkScale=NULL,
                       debug=FALSE,override=FALSE) {

  if (!is.PnodeWarehouse(nodehouse)) {
    stop("Node warehouse must be supplied.")
  }
  if (!is.PnetWarehouse(nethouse)) {
    stop("Net warehouse must be supplied.")
  }

  if (!all(Qmat.reqcol %in% names(Qmat))) {
    stop("Badly formed Q matrix.")
  }

  ## Figure out names of proficiency variables.
  Anames <- names(Qmat)[grep("A\\..*",names(Qmat))]
  profnames <- sapply(strsplit(Anames,".",fixed=TRUE), function(x) x[2])

  if (!all(profnames %in% names(Qmat))) {
    stop("Expected columns for proficiency variables:",profnames)
  }
  if (!all(paste("B",profnames,sep=".") %in% names(Qmat))) {
    stop("Expected B columns for proficiency variables:",profnames)
  }

  netnames <- unique(Qmat$Model)
  for (netname in netnames) {
    if (debug) {
      cat("Processing net ",netname,"\n")
    }
    net <- WarehouseSupply(nethouse,netname)
    if (is.null(net)) {
      stop("Could not find/create network ",netname)
    }
    hubname <- PnetHub(net)
    if (nchar(hubname)>0) {
      hub <- WarehouseSupply(nethouse,hubname)
      if (is.null(hub)) {
        stop("Could not find hub for network ",netname,",","hubname")
      }
    }
    nodenames <- unique(Qmat$Node[Qmat$Model==netname])
    for (nodename in nodenames) {
      if (debug) {
        cat("Processing node ",nodename,"\n")
      }
      node <- WarehouseSupply(nodehouse,c(netname,nodename))
      if (is.null(node)) {
        stop("Could not find/create node ",nodename, "in model",netname)
      }
      tryCatch({
        Qrows <- Qmat[Qmat$Model==netname & Qmat$Node==nodename,,drop=FALSE]
        nstates <- nrow(Qrows)
        if (nstates != Qrows[1,"NStates"] - 1L) {
          stop("Expected ",nstates != Qrows[1,"NStates"] - 1L,"got",nstates)
        }
        ## Check/adjust structure
        stubs <- list()
        parnames <- names(which(apply(Qrows[,profnames,drop=FALSE]==1,2,any)))
        exparnames <- PnodeParentNames(node)
        if (!setequal(parnames,exparnames)) {
          if (debug) {
            cat("Node has parents: ", exparnames,"\n")
            cat("But Q matrix has parents: ", parnames,"\n")
          }
          if (length(exparnames) > 0L) {
            cat("While processing links for node: ",nodename," in network",
                netname,"\n")
            cat("Node has parents: ", exparnames,"\n")
            cat("But Q matrix has parents: ", parnames,"\n")
            if (override) {
              warning("Changing net to match Q matrix.")
            } else {
              stop("Graphical structure does not match Q matrix.  See console.")
            }
          }
          pars <- list()
          for (pname in setdiff(parnames,exparnames)) {
            pars[[pname]] <- WarehouseSupply(nodehouse,c(hubname,pname))
          }
          stubs <- PnetMakeStubNodes(net,pars)
          PnodeParents(node) <- c(PnodeParents(node),stubs)
        }
        ## Change order to match node. Even if nominally a match.
        parnames <- PnodeParentNames(node)
        if (debug) {
          cat("Final parents: ",paste(parnames,collapse=", "),".\n")
        }
        ## Extract Parameters
        ## "Link","LinkScale",
        ll <- Qrows[1,"Link"]
        if (is.na(ll) || nchar(ll)==0L) ll <- defaultLink
        PnodeLink(node) <- ll
        lsc <- Qrows[1,"LinkScale"]
        if (is.na(lsc) || nchar(lsc)==0L) lsc <- defaultLinkScale
        if (!is.null(lsc)) {
          PnodeLinkScale(node) <- lsc
        }
        if (debug) {
          cat ("Link: ",ll,"(",lsc,"). \n")
        }

        ## "Rules",
        rules <- Qrows[,"Rules"]
        nrules <- sum(nchar(rules)>0L)  #Number of non-missing rules
        if (nrules == 0L) {
          rules <- defaultRule
        }
        if (nrules!=1L && nrules !=nstates) {
          stop("Expected ",nstates," (or 1) rules but got ",nrules)
        }
        if (debug) {
          cat ("Rules: ",paste(rules,collapse=", "),".\n")
        }
        if (nrules==1L) {
          PnodeRules(node) <- dgetFromString(rules[[1]])
        }
        else {
          PnodeRules(node) <- lapply(rules,dgetFromString)
        }
        rules <- PnodeRules(node)

        ## "A","B",
        QrowsQ <- Qrows[,parnames]
        QrowsA <- Qrows[,paste("A",parnames,sep=".")]
        QrowsB <- Qrows[,paste("B",parnames,sep=".")]

        if (nstates==1L) {
          PnodeQ(node) <- TRUE
          if (is.list(rules)) rules <- rules[[1]]
          ## Single Row Cases
          if (rules %in% getOffsetRules()) {
            ## Offset Case
            alphas <- Qrows[1,"A"]
            if (is.na(alphas)) alphas <- defaultAlpha
            PnodeAlphas(node) <- alphas
            betas <- as.numeric(QrowsB)
            if (all(is.na(betas))) betas <- rep(defaultBeta,length(parnames))
            names(betas) <- parnames
            PnodeBetas(node) <- betas
            if (debug) {
              cat("Single Row offset: \n")
              print(PnodeQ(node))
              print(PnodeAlphas(node))
              print(PnodeBetas(node))
            }
          } else {
            ## Weighted Case (Compensatory)
            alphas <- as.numeric(QrowsA)
            if (all(is.na(alphas))) alphas <- rep(defaultAlpha,length(parnames))
            names(alphas) <- parnames
            PnodeAlphas(node) <- alphas
            betas <- Qrows[1,"B"]
            if (is.na(betas)) betas <- defaultBeta
            PnodeBetas(node) <- betas
            if (debug) {
              cat("Single Row weighted: \n")
              print(PnodeQ(node))
              print(PnodeAlphas(node))
              print(PnodeBetas(node))
            }
          }
        } else {
          QrowsQ <- as.matrix(QrowsQ)
          QrowsA <- as.matrix(QrowsA)
          QrowsB <- as.matrix(QrowsB)
          ## Multiple Row Cases
          if (length(rules) == 1L && !(rules %in% getOffSetRules())
              && all(is.na(QrowsA[-1,])) && all(QrowsQ)) {
            ## Weighted -- single row of AAs.
            ## This pattern is only allowed when there is a single
            ## non-offset rule, all elements of the Q-matrix are true,
            ## and all rows after the first are blank (NA)
            PnodeQ(node) <- TRUE
            alphas <- as.numeric(QrowsA[1,])
            if (all(is.na(alphas))) alphas <- rep(defaultAlpha,length(parnames))
            names(alphas) <- parnames
            PnodeAlphas(node) <- alphas
            betas <- Qrows[,"B"]
            if (is.na(betas)) betas <- rep(defaultBeta,nstates)
            PnodeBetas(node) <- as.list(betas)

            if (debug) {
              cat("Multiple Row weighted: \n")
              print(PnodeQ(node))
              print(PnodeAlphas(node))
              print(PnodeBetas(node))
            }
          } else {
            ## Offset or Mixed
            QrowsQ <- QrowsQ==1
            PnodeQ(node) <- QrowsQ
            if (length(rules) <nstates) {
              ##If single rules, replicate out as that will be easier
              for (i in 2:nstates) {
                rules[[i]] <- rules[[1]]
              }
            }
            alphas <- list()
            betas <- list()
            for (istate in 1:nstates) {
              npar <- sum(QrowsQ[istate,])
              if (rules[[istate]] %in% getOffsetRules()) {
                bb <- as.numeric(QrowsB[istate,QrowsQ[istate,]])
                if (all(is.na(bb))) bb <- rep(defaultBeta,npar)
                names(bb) <- parnames[QrowsQ[istate,]]
                betas[[istate]] <- bb
                aa <- Qrows[istate,"A"]
                if (is.na(aa)) aa <- defaultAlpha
                alphas[[istate]] <- aa
              } else { ## Weighted Rule
                aa <- as.numeric(QrowsA[istate,QrowsQ[istate,]])
                if (all(is.na(aa))) aa <- rep(defaultAlpha,npar)
                names(aa) <- parnames[QrowsQ[istate,]]
                alphas[[istate]] <- aa
                bb <- Qrows[istate,"B"]
                if (is.na(bb)) bb <- defaultBeta
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
          } ## End Mixed
        } ## End Multiple

        ## "PriorWeight"
        wt <- Qrows[1,"PriorWeight"]
        if (!is.na(wt) && nchar(wt)>=0L) {
          PnodePriorWeight(node) <- dgetFromString(wt)
          if (debug) {
            cat ("Prior Weight: ",wt,".\n")
          }
        }

        ## Build Table
        ## This won't work until we figure out how to set the default
        ## prior weight.
        ##BuildTable(node)

        ## Clean out stub nodes
        if (length(stubs)>0L)
          PnetRemoveStubNodes(net,stubs)

      }, ## End tryCatch
      error = function(e) {
        ## Resignal error with context information about the node.
        ee <- simpleError(
            paste("While processing node ",nodename, "in model ",netname,":",
                  conditionMessage(e)),
            conditionCall(e))
      stop(ee)
      })
    }  ## Next Node
  } ## Next Net
  invisible(netnames)
}




### Omega matrix for proficiencies.


Pnet2Omega <- function(net,prof, defaultRule="Compensatory",
                       defaultLink="normalLink",defaultAlpha=1,
                       defaultBeta=0,defaultLinkScale=1,
                       debug=FALSE) {
  p <- length(prof)
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
  result <- data.frame(Node=profnames,Omega,
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
Omega.reqcol <- c("Node","Link","Rules","Intercept","PriorWeight")

## Override controls behavior when OmegaMat and pn don't agree.
## Default  value of FALSE generates an error.
## A value of true issues a warning and changes the network to agree
## with the matrix.
Omega2Pnet <- function(OmegaMat,pn,nodewarehouse,
                       defaultRule="Compensatory",
                       defaultLink="normalLink",defaultAlpha=1,
                       defaultBeta=0,defaultLinkScale=1,
                       defaultWeight=NULL,
                       debug=FALSE,override=FALSE) {
  if (!is.Pnet(pn)) {
    stop("Blank network must be provided.")
  }
  if (!is.PnodeWarehouse(nodewarehouse)) {
    stop("Node warehouse must be supplied.")
  }
  if (!all(Omega.reqcol %in% names(OmegaMat))) {
    stop("Badly formed Omega matrix.")
  }
  ## First parse the Matrix
  nodenames <- OmegaMat$Node
  QQ <- OmegaMat[,nodenames]
  if (ncol(QQ) != length(nodenames)) {
    stop("There are not columns corresponding to every variable.")
  }
  AA <- OmegaMat[,paste("A",nodenames,sep=".")]
  if (ncol(AA) != length(nodenames)) {
    stop("There are not A columns corresponding to every variable.")
  }
  colnames(AA) <- nodenames
  intercepts <- OmegaMat$Intercept
  names(intercepts) <- nodenames
  weights <- OmegaMat$PriorWeight
  names(weights) <- nodenames
  links <- OmegaMat$Link
  names(links) <- nodenames
  rules <- OmegaMat$Rules
  names(rules) <- nodenames

  ## Buidling List of Nodes
  if (debug) {
    cat("\n Building list of nodes.\n")
  }
  nodes <- vector("list",length(nodenames))
  names(nodes) <- nodenames
  netname <- PnetName(pn)
  for (ndn in nodenames) {
    nodes[[ndn]] <- WarehouseSupply(nodewarehouse,c(netname,ndn))
  }

  ## Next build structure.
  if (debug) {
    cat("\n Processing links.\n")
  }

  for (ndn in nodenames) {
    if (debug) cat("Building links for node: ",ndn,"\n")
    node <- nodes[[ndn]]
    parnames <- setdiff(nodenames[QQ[ndn,]==1],ndn)
    exparnames <- PnodeParentNames(node)
    if (length(exparnames) > 0L) {
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
    }
    # Change order to match matrix. Even if nominally a match.
    PnodeParents(node) <- nodes[parnames]
  }

  ### Next build structure.
  if (debug) {
    cat("\n Processing CPTs.\n")
  }

  for (ndn in nodenames) {
    if (debug) cat("Building links for node: ",ndn,"\n")
    node <- nodes[[ndn]]
    parnames <- setdiff(nodenames[QQ[ndn,]==1],ndn)
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
    if (is.na(intercepts[ndn])) {
      PnodeBetas(node) <- defaultBeta
    } else {
      PnodeBetas(node) <- intercepts[ndn]
    }
    if (is.na(AA[ndn,ndn])) {
      PnodeLinkScale(node) <- defaultLinkScale
    } else {
      PnodeLinkScale(node) <- AA[ndn,ndn]
    }
    parnames <- PnodeParentNames(node)
    alphas <- AA[ndn,parnames]
    alphas[is.na(alphas)] <- defaultAlpha
    names(alphas) <- parnames
    PnodeAlphas(node) <- alphas

    if (!is.na(weights[ndn]) && nchar(weights[ndn]) > 0L) {
    PnodePriorWeight(node) <- dgetFromString(weights)
    }

  }
  pn

}

