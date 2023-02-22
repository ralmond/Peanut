
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
  flog.info("Proficiency variables are",profnames,capture=TRUE)

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

  ## Initialize error accumulator
  Errs <- list()

  ## Now loop over vars, processing each one.

  irow <- 1
  for (nd in obs) {
    ndnm <- PnodeName(nd)
    netnm <- PnetName(PnodeNet(nd))
    flog.debug("Processing node %s in net %s",ndnm,netnm)
    out <- tryCatch({
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
        QQ[irow:(irow+nstate-2),match(PnodeParentNames(nd),profnames)] <-
          PnodeQ(nd)
      }
      QQQ <-QQ[irow:(irow+nstate-2),
               match(PnodeParentNames(nd),profnames),
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
    }, context=sprintf("Processing node %s in net %s",ndnm,netnm))
    if (is(out,'try-error')) {
      Errs <- c(Errs,out)
      if (debug) recover()
    }

    ## Next node
    irow <- irow + nstate-1
  }
  if (length(Errs) >0L)
    stop("Errors encountered while building Q-matrix.")
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
                       defaultPriorWeight=10,
                       debug=FALSE,override=FALSE) {

  if (!is.PnodeWarehouse(nodehouse)) {
    stop("Node warehouse must be supplied.")
  }
  if (!is.PnetWarehouse(nethouse)) {
    stop("Net warehouse must be supplied.")
  }
  foundCols <- (Qmat.reqcol %in% names(Qmat))
  if (!all(foundCols)) {
    flog.error("Q-matrix missing columns:", Qmat.reqcol[!foundCols],
               capture=TRUE)
    stop("Badly formed Q matrix, missing columns: ",
         paste(Qmat.reqcol[!foundCols],collapse=", "))
  }

  ## Figure out names of proficiency variables.
  Anames <- names(Qmat)[grep("A\\..*",names(Qmat))]
  profnames <- sapply(strsplit(Anames,".",fixed=TRUE), function(x) x[2])
  flog.info("Proficiency variables:",profnames,capture=TRUE)
  if (!all(profnames %in% names(Qmat))) {
    flog.error("Missing proficiency variables:",
              setdiff(profnames,names(Qmat)), capture=TRUE)
    stop("Expected columns for proficiency variables:",profnames)
  }
  Bnames <- paste("B",profnames,sep=".")
  if (!all(Bnames %in% names(Qmat))) {
    flog.error("Missing B columns:",
               setdiff(Bnames,names(Qmat)), capture=TRUE)
    stop("Expected B columns for proficiency variables:",profnames)
  }
  ## Initialize error list.
  Errs <- list()

  Qmat$Model <- trimws(Qmat$Model)
  Qmat$Node <- trimws(Qmat$Node)
  netnames <- unique(Qmat$Model)
  for (netname in netnames) {
    if (nchar(netname) ==0L) {
      flog.warn("Skipping Blank Row(s)")
      next
    }
    flog.info("Processing net %s",netname)
    net <- WarehouseSupply(nethouse,netname)
    if (is.null(net)) {
      flog.error("Could not find/create network %s",netname)
      Errs <- c(Errs,
                simpleError(sprintf("Could not find/create network %s",
                                    netname)))
      next
    }
    hubname <- PnetHub(net)
    if (nchar(hubname)>0) {
      flog.debug("Fetching hub network %s",hubname)
      hub <- WarehouseSupply(nethouse,hubname)
      if (is.null(hub)) {
        flog.error("Could not find hub (%s) for network %s", hubname,netname)
        Errs <- c(Errs,
                  simpleError(sprintf("Could not find hub (%s) for network %s",
                                      hubname, netname)))
        next
      }
    }
    nodenames <- unique(Qmat$Node[Qmat$Model==netname])
    for (nodename in nodenames) {
      flog.info("Processing node %s in net %s",nodename, netname)
      node <- WarehouseSupply(nodehouse,c(netname,nodename))
      if (is.null(node)) {
        flog.error("Could not find/create node %s in model %s",
                   nodename, netname)
        Errs <- c(Errs,
                  simpleError(sprintf("Could not find/create node %s in model %s",
                                      nodename, netname)))
      }
      context <- sprintf("processing node %s in net %s", nodename, netname)
      out <- flog.try({
        Qrows <- Qmat[Qmat$Model==netname & Qmat$Node==nodename,,drop=FALSE]
        nstates <- nrow(Qrows)
        if (nstates != Qrows[1,"NStates"] - 1L) {
          stop("Expected ", Qrows[1,"NStates"] - 1L," rows got ",nstates)
        }
        ## Check/adjust structure
        stubs <- list()
        parnames <- profnames[which(apply(Qrows[,profnames,drop=FALSE]==1,2,any))]
        exparnames <- PnodeParentNames(node)
        flog.trace("While %s:",context)
        flog.trace("Node has parents: ", exparnames,capture=TRUE)
        flog.trace("And Q matrix has parents: ", parnames,capture=TRUE)
        if (!setequal(parnames,exparnames)) {
          if (length(exparnames) > 0L) {
            flog.warn("While %s:",context)
            flog.warn("Node has parents: ", exparnames,capture=TRUE)
            flog.warn("But Q matrix has parents: ", parnames,capture=TRUE)
            if (override) {
              flog.warn("Changing node %s to match Q matrix.", nodename)
            } else {
              stop("Graphical structure does not match Q matrix.  See console.")
            }
          }
          parents <- list()
          isStub <- rep(FALSE,length(parnames))
          names(isStub) <- parnames
          for (pname in parnames) {
            flog.trace("Processing Parent %s",pname)
            ## Already existing node?
            parents[[pname]] <- PnetFindNode(net,pname)
            if (is.null(parents[[pname]])) {
              flog.trace("Parent %s not found, making stub node",pname)
              ## Try to make stub node from hub.
              isStub[pname] <- TRUE
              parents[[pname]] <-
                WarehouseSupply(nodehouse,c(hubname,pname))
            }
            if (is.null(parents[[pname]])) {
              flog.trace("Parent %s not found in hub, making local node",pname)
              ## Try to make local node.
              isStub[pname] <- FALSE
              parents[[pname]] <-
                WarehouseSupply(nodehouse,c(netname,pname))
            }
            if (is.null(parents[[pname]])) {
              flog.error("While %s:",context)
              flog.error("Could not find parent %s of %s in %s or %s",
                         pname, nodename, hubname,netname)
              stop("Could not find parent ",pname, "in net", hubname,
                   "or ",netname)
            }
          }
          if (any(isStub)) {
            flog.trace("Making stub nodes for ",pname[isStub],
                       capture=TRUE)
            stubs <- PnetMakeStubNodes(net,parents[isStub])
            parents[isStub] <- stubs
          }
          PnodeParents(node) <- parents
        }
        ## Change order to match node. Even if nominally a match.
        parnames <- PnodeParentNames(node)
        flog.debug("Final parents for node %s: %s ",nodename,
                   paste(parnames,collapse=", "))
        ## Extract Parameters
        ## "Link","LinkScale",
        ll <- trimws(Qrows[1,"Link"])
        if (is.na(ll) || nchar(ll)==0L) {
          flog.trace("No link for %s, using default link.", nodename)
          ll <- defaultLink
        }
        PnodeLink(node) <- ll
        lsc <- Qrows[1,"LinkScale"]
        if (is.na(lsc) || nchar(lsc)==0L) {
          flog.trace("No link scale for %s, using default.", nodename)
          lsc <- defaultLinkScale
        }
        if (!is.null(lsc)) {
          PnodeLinkScale(node) <- lsc
        }
        flog.debug("Link: %s (%f)",ll,as.numeric(lsc))


        ## "Rules",
        rules <- trimws(Qrows$Rules)    #Need to use $ here to force
                                        #in case Qmat is tibble not df
        ## Fix fancy quotes added by some spreadsheets
        rules <- gsub(intToUtf8(c(91,0x201C,0x201D,93)),"\"",rules)
        ## Blanks are read as NAs, so fix
        if (any(is.na(rules)))
          rules[is.na(rules)] <- ""
        nrules <- sum(nchar(rules)>0L)  #Number of non-missing rules
        ## Add back in quotes if missing.
        rules <- ifelse(grepl('^".*"$',rules),rules,sprintf('"%s"',rules))
        if (nrules == 0L) {
          flog.trace("No rule for %s, using default rule.", nodename)
          rules <- defaultRule
        }
        flog.debug("Rules for node %s: %s",nodename,
                   paste(rules,collapse=", "))
        if (nrules!=1L && nrules !=nstates) {
          flog.error("Context",context)
          flog.error("Expected ",nstates," (or 1) rules but got ",nrules)
          stop("Expected ",nstates," (or 1) rules but got ",nrules)
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
            flog.debug("Single Row offset: ")
            flog.debug("PnodeQ:",PnodeQ(node), capture=TRUE)
            flog.debug("PnodeAlphas:", PnodeAlphas(node), capture=TRUE)
            flog.debug("PnodeBetas:", PnodeBetas(node), capture=TRUE)
          } else {
            ## Weighted Case (Compensatory)
            alphas <- as.numeric(QrowsA)
            if (all(is.na(alphas))) alphas <- rep(defaultAlpha,length(parnames))
            names(alphas) <- parnames
            PnodeAlphas(node) <- alphas
            betas <- Qrows[1,"B"]
            if (is.na(betas)) betas <- defaultBeta
            PnodeBetas(node) <- betas
            flog.debug("Single Row weighted:")
            flog.debug("PnodeQ:", PnodeQ(node), capture=TRUE)
            flog.debug("PnodeAlphas", PnodeAlphas(node), capture=TRUE)
            flog.debug("PnodeBetas", PnodeBetas(node), capture=TRUE)
          }
        } else {
          QrowsQ <- as.matrix(QrowsQ)
          QrowsA <- as.matrix(QrowsA)
          QrowsB <- as.matrix(QrowsB)
          ## Multiple Row Cases
          if (length(rules) == 1L && !(rules %in% getOffsetRules())
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
            betas <- Qrows$B            # Need $ her in case we have
                                        # tibble instead of df.
            if (is.na(betas)) betas <- rep(defaultBeta,nstates)
            PnodeBetas(node) <- as.list(betas)

            flog.debug("Multiple Row weighted:")
            flog.debug("PnodeQ", PnodeQ(node), capture=TRUE)
            flog.debug("PnodeAlphas", PnodeAlphas(node), capture=TRUE)
            flog.debug("PnodeBetas", PnodeBetas(node), capture=TRUE)
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
            flog.debug("Multiple Row mixed: \n")
            flog.debug("PnodeRules", PnodeRules(node), capture=TRUE)
            flog.debug("PnodeQ", PnodeQ(node), capture=TRUE)
            flog.debug("PnodeAlphas", PnodeAlphas(node), capture=TRUE)
            flog.debug("PnodeBetas", PnodeBetas(node), capture=TRUE)
          } ## End Mixed
        } ## End Multiple

        ## "PriorWeight" from table
        wt <- Qrows[1,"PriorWeight"]
        pwt <- NULL
        if (!is.na(wt) && !is.null(wt)) {
          if (isTRUE(is.numeric(wt)) && isTRUE(wt > 0)) pwt <- wt
          if (isTRUE(is.character(wt)) && isTRUE(nchar(wt)>=0L)) {
            pwt <- dgetFromString(wt)
          }
        }
        if (is.null(pwt)) pwt <- PnodePriorWeight(node)
        if (is.null(pwt)) pwt <- defaultPriorWeight
        flog.debug("Prior Weight: ",pwt, capture=TRUE)
        PnodePriorWeight(node) <- pwt
        ## Build Table
        BuildTable(node)

        ## Clean out stub nodes
        if (length(stubs)>0L)
          PnetRemoveStubNodes(net,stubs)

      }, ## End tryCatch
      context=context)
      if (is(out,'try-error')) {
        Errs <- c(Errs,out)
        if (debug) recover()
      }
    }  ## Next Node
  } ## Next Net
  if (length(Errs) >0L)
    stop("Errors encountered while building networks.")
  invisible(netnames)
}




### Omega matrix for proficiencies.


Pnet2Omega <- function(net,prof, defaultRule="Compensatory",
                       defaultLink="normalLink",defaultAlpha=1,
                       defaultBeta=0,defaultLinkScale=1,
                       debug=FALSE) {
  p <- length(prof)
  profnames <- sapply(prof,PnodeName)
  flog.info("Proficiency variables are",profnames,capture=TRUE)

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
  ord <- topsort(Omega)
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

  ## Initialize Error list
  Errs <- list()

  ## Loop throught the nodes, filling in fields
  for (nd in prof) {
    pname <- PnodeName(nd)
    context <- sprintf("processing node %s.",pname)
    flog.debug(context)
    out <- flog.try({
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
    }, context=context)
    if (is(out,'try-error')) {
      Errs <- c(Errs,out)
      if (debug) recover()
    }
  }

  if (length(Errs) >0L)
    stop("Errors encountered while building omega matrix.")
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
  if (noisy==TRUE)
    warning("Noisy argument depricated for function topsort.
             Use 'flog.threshold(TRACE)' instead.")

  if (nrow(Omega) != ncol(Omega)) {
    stop("Matrix must be square.")
  }
  ord <- numeric()
  cols <- 1:ncol(Omega)
  if (!is.null(colnames(Omega))) {
    names(cols) <- colnames(Omega)
  }
  while (nrow(Omega) > 0L) {
    rsum <- apply(Omega,1,sum)
    priors <- which(rsum==1)
    flog.trace("Omega so far:", Omega, capture=TRUE)
    if (length(priors) == 0L) {
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
                       defaultPriorWeight=10,
                       debug=FALSE,override=FALSE,
                       addTvals=TRUE) {
  if (!is.Pnet(pn)) {
    stop("Blank network must be provided.")
  }
  if (!is.PnodeWarehouse(nodewarehouse)) {
    stop("Node warehouse must be supplied.")
  }
  if (!all(Omega.reqcol %in% names(OmegaMat))) {
      flog.error("Omega matrix missing columns %s.",
                 paste(setdiff(Omega.reqcol,names(OmegaMat)),collapse=", "))
    stop("Badly formed Omega matrix.")
  }
  ## First parse the Matrix
  nodenames <- trimws(OmegaMat$Node)
  flog.info("Proficiency variables are",nodenames,capture=TRUE)
  Qcol <- pmatch(nodenames, names(OmegaMat))
  if (any(is.na(Qcol))) {
    stop ("Could not find Q-matrix columns:",nodenames[is.na(Qcol)])
  }
  QQ <- OmegaMat[,nodenames]
  if (ncol(QQ) != length(nodenames)) {
    stop("There are not columns corresponding to every variable.")
  }
  rownames(QQ) <- colnames(QQ)
  flog.trace("Included Q-matrix:",QQ,capture=TRUE)
  Anames <- paste("A",nodenames,sep=".")
  Acol <- pmatch(Anames,names(OmegaMat))
  if (any(is.na(Acol))) {
    stop ("Could not find A-matrix columns:",nodenames[is.na(Acol)])
  }
  AA <- OmegaMat[,Anames]
  if (ncol(AA) != length(nodenames)) {
    stop("There are not A columns corresponding to every variable.")
  }
  colnames(AA) <- nodenames
  rownames(AA) <- nodenames
  flog.trace("Included A-matrix:",AA,capture=TRUE)
  intercepts <- OmegaMat$Intercept
  names(intercepts) <- nodenames
  weights <- OmegaMat$PriorWeight
  names(weights) <- nodenames
  links <- OmegaMat$Link
  names(links) <- nodenames
  rules <- OmegaMat$Rules
  names(rules) <- nodenames

  Errs <- list()
  ## Buidling List of Nodes
  flog.info("Building list of nodes.")
  nodes <- vector("list",length(nodenames))
  names(nodes) <- nodenames
  netname <- PnetName(pn)
  for (ndn in nodenames) {
    flog.debug("Fetching node %s",ndn)
    nodes[[ndn]] <- WarehouseSupply(nodewarehouse,c(netname,ndn))
    if (is.null(nodes[[ndn]])) {
      msg <- sprintf("Could not find node %s in net %s.", ndn, netname)
      flog.error(msg)
      Errs <- c(Errs,simpleError(msg))
    }
  }
  if (length(Errs) >0L)
    stop("Not all nodes could be created:  stopping.")
  ## Next build structure.
  flog.info("Processing links.")

  for (ndn in nodenames) {
    context <- sprintf("Building links for node: %s.",ndn)
    flog.debug(context)
    out <- flog.try({
      node <- nodes[[ndn]]
      if (any(is.na(PnodeStateValues(node)))) {
        flog.warn("Node %s does not have State Values set.",ndn)
        if (addTvals) {
          flog.info("Using normal rule to create State Values for %s.",ndn)
          PnodeStateValues(node) <- effectiveThetas(PnodeNumStates(node))
        }
      }
      parnames <- nodenames[sapply(QQ[ndn,]==1,isTRUE)]
      parnames <- setdiff(parnames,ndn)
      exparnames <- PnodeParentNames(node)
      flog.trace("Processing links for node: %s",ndn)
      flog.trace("Node has parents: ", exparnames, capture=TRUE)
      flog.trace("Omega matrix has parents: ", parnames, capture=TRUE)
      if (length(exparnames) > 0L) {
        if (!setequal(parnames,exparnames)) {
          flog.warn("While processing links for node: %s",ndn)
          flog.warn("Node has parents: ", exparnames, capture=TRUE)
          flog.warn("But Omega matrix has parents: ", parnames, capture=TRUE)
          if (override) {
            flog.warn("Changing node %s to match Omega matrix.",ndn)
          } else {
            stop("Graphical structure does not match Omega matrix.  See console.")
          }
        }
      }
      ## Change order to match matrix. Even if nominally a match.
      PnodeParents(node) <- nodes[parnames]
    }, context=context)
    if (is(out,'try-error')) {
      Errs <- c(Errs,out)
      if (debug) recover()
    }
  }
  if (length(Errs) >0L)
    stop("Graph structure could be created:  stopping.")

  ### Next build structure.
  flog.info("Processing CPTs.")

  for (ndn in nodenames) {
    context <- sprintf("Building CPTs for node: %s",ndn)
    flog.debug(context)
    flog.try({
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
      PnodeQ(node) <- TRUE               #This works as long as always normalLink
      parnames <- PnodeParentNames(node)
      alphas <- as.numeric(AA[ndn,parnames])
      alphas[is.na(alphas)] <- defaultAlpha
      names(alphas) <- parnames
      PnodeAlphas(node) <- alphas

      pwt <- PnodePriorWeight(node)
      if (!is.na(weights[ndn])) {
        if (is.character(weights[ndn]) && nchar(weights[ndn]) > 0L) {
          pwt <- dgetFromString(weights[ndn])
        } else if (is.numeric(weights[ndn])) {
          pwt <- weights[ndn]
        }
      }
      if (is.null(pwt)) pwt <- defaultPriorWeight
      flog.debug("Prior Weight: ",pwt, capture=TRUE)
      PnodePriorWeight(node) <- pwt

      ## Build Table
      napar <- sapply(PnodeParentTvals(node),function(x) any(is.na(x)))
      if (any(napar)) {
        flog.error("Parents %s of node %s don't have levels set.",
                   paste(parnames[napar],collapse=", "),ndn)
        stop("Parent",paste(parnames[napar],collapse=", "),
             "of node", ndn, "don't have levels set.")
      }
      BuildTable(node)

    },context=context)
    if (is(out,'try-error')) {
      Errs <- c(Errs,out)
      if (debug) recover()
    }
  }
  if (length(Errs) >0L)
    stop("Errors encountered while building network.")
  pn
}

