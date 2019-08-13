
GEMfit <- function (net, cases, tol=sqrt(.Machine$double.eps), maxit=100,
                   Estepit=1, Mstepit=30,trace=FALSE,debugNo=maxit+1) {

  oldThreshold <- flog.threshold()
  ## Base case
  converged <- FALSE
  llike <- rep(NA,maxit+1)
  iter <- 1
  if (iter >= debugNo) flog.threshold(DEBUG)
  BuildAllTables(net)
  llike[iter] <- calcPnetLLike(net,cases)
  if (trace)
    flog.info("Iteration %d; Log Likelihood %e.",iter,llike[iter])

  while(!converged && iter <= maxit) {
    ## E-step
    calcExpTables(net, cases, Estepit=Estepit, tol=tol)
    #browser()

    ## M-step
    maxAllTableParams(net, Mstepit=Mstepit, tol=tol)
    #browser()
    ## Update parameters & convergence test
    iter <- iter + 1
    if (iter >= debugNo) flog.threshold(DEBUG)
    BuildAllTables(net)

    #browser()

    llike[iter] <- calcPnetLLike(net,cases)
    if (trace) flog.info("Iteration %d; Log likelihood %e.",iter,llike[iter])
    converged <- (abs(llike[iter]-llike[iter-1]) < tol)
  }
  flog.info("GEMfit %s after %d iterations.",
            ifelse(converged,"converged","did not converge"),iter)
  flog.threshold(oldThreshold)
  list(converged=converged,iter=iter,
       llikes=llike[1:iter])
}


### Build CPTs from parameters

PnodeBuildTable <- function (node) {
  UseMethod("PnodeBuildTable")
}
setGeneric("PnodeBuildTable")
  ## node[] <- calcDPCTable(PnodeParentStates(node),PnodeStates(node),
  ##                        PnodeLnAlphas(node), PnodeBetas(node),
  ##                        PnodeRules(node),PnodeLink(node),
  ##                        PnodeLinkScale(node),PnodeParentTvals(node))
  ## NodeExperience(node) <- GetPriorWeight(node)
  ## invisible(node)



calcPnetLLike <- function (net,cases){
  UseMethod("calcPnetLLike")
}
setGeneric("calcPnetLLike")

calcExpTables <- function (net, cases, Estepit=1, tol=sqrt(.Machine$double.eps)) {
  UseMethod("calcExpTables")
}
setGeneric("calcExpTables")


maxAllTableParams <- function (net, Mstepit=5,
                                       tol=sqrt(.Machine$double.eps),
                                       debug=FALSE) {
  Errs <- list()
  netnm <- PnetName(net)
  lapply(PnetPnodes(net),
         function (nd) {
           ndnm <- PnodeName(nd)
           flog.debug("Updating params for node %s in net %s.",ndnm,netnm)
           out <- flog.try(maxCPTParam(nd,Mstepit,tol),
                           context=sprintf("Updating params for node %s in net %s.",
                                           ndnm, netnm))
           if (is(out,'try-error')) {
             Errs <- c(Errs,out)
             if (debug) recover()
           }
         })
  if (length(Errs) >0L)
    stop("Errors encountered while updating parameters for ",netnm)
  invisible(net)
}
setGeneric("maxAllTableParams")

maxCPTParam <- function (node, Mstepit=5, tol=sqrt(.Machine$double.eps)) {
  UseMethod("maxCPTParam")
}
setGeneric("maxCPTParam")
