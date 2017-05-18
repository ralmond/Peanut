
GEMfit <- function (net, cases, tol=sqrt(.Machine$double.eps), maxit=100,
                   Estepit=1, Mstepit=3,trace=FALSE,debugNo=maxit+1) {

  ## Base case
  converged <- FALSE
  llike <- rep(NA,maxit+1)
  iter <- 1
  BuildAllTables(net,debug=iter>=debugNo)
  llike[iter] <- calcPnetLLike(net,cases)
  if (trace) cat("Iteration ",iter,"; Log likelihood",llike[iter],"\n")

  while(!converged && iter <= maxit) {
    ## E-step
    calcExpTables(net, cases, Estepit=Estepit, tol=tol)

    ## M-step
    maxAllTableParams(net, Mstepit=Mstepit, tol=tol,
                      debug=iter>=debugNo)

    ## Update parameters & convergence test
    iter <- iter + 1
    BuildAllTables(net,debug=iter>=debugNo)
    llike[iter] <- calcPnetLLike(net,cases)
    if (trace) cat("Iteration ",iter,"; Log likelihood",llike[iter],"\n")
    converged <- (abs(llike[iter]-llike[iter-1]) < tol)
  }

  list(converged=converged,iter=iter,
       llikes=llike[1:iter])
}


### Build CPTs from parameters

PnodeBuildTable <- function (node) {
  UseMethod("PnodeBuildTable")
  ## node[] <- calcDPCTable(ParentStates(node),NodeStates(node),
  ##                        PnodeLnAlphas(node), PnodeBetas(node),
  ##                        PnodeRules(node),PnodeLink(node),
  ##                        PnodeLinkScale(node),PnodeParentTvals(node))
  ## NodeExperience(node) <- GetPriorWeight(node)
  ## invisible(node)
}


calcPnetLLike <- function (net,cases){
  UseMethod("calcPnetLLike")
}

calcExpTables <- function (net, cases, Estepit=1, tol=sqrt(.Machine$double.eps)) {
  UseMethod("calcExpTables")
}

maxAllTableParams <- function (net, Mstepit=5, tol=sqrt(.Machine$double.eps),
                               debug=FALSE) {
  UseMethod("maxAllTableParams")
}

maxAllTableParams.default <- function (net, Mstepit=5,
                                       tol=sqrt(.Machine$double.eps),
                                       debug=FALSE) {
  lapply(PnetPnodes(net),
         function (nd) {
           if (debug) cat("Updating params for",nd,"\n")
           maxCPTParam(nd,Mstepit,tol)
         })
  invisible(net)
}

maxCPTParam <- function (node, Mstepit=5, tol=sqrt(.Machine$double.eps)) {
  UseMethod("maxCPTParam")
}