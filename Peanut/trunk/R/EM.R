
GEMfit <- function (net, cases, tol=sqrt(.Machine$double.eps), maxit=100,
                   Estepit=1, Mstepit=3) {

  ## Base case
  converged <- FALSE
  llike <- rep(NA,maxit+1)
  iter <- 1
  PnetBuildTables(net)
  llike[iter] <- calcPnetLLike(net,cases)

  while(!converged && iter <= maxit) {
    ## E-step
    calcExpTables(net, cases, Estepit=Estepit, tol=tol)

    ## M-step
    maxAllTableParams(net, Mstepit=Mstepit, tol=tol)

    ## Update parameters & convergence test
    iter <- iter + 1
    PnetBuildTables(net)
    llike[iter] <- calcPnetLLike(net,cases)
    converged <- (abs(llike[iter]-llike[iter-1]) < tol)
  }

  list(converged=converged,iter=iter,
       llikes=llikes[1:iter])
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

maxAllTableParams <- function (net, Mstepit=3, tol=sqrt(.Machine$double.eps)) {
  UseMethod("maxTableParams")
}

maxAllTableParams.default <- function (net, Mstepit=3,
                                       tol=sqrt(.Machine$double.eps)) {
  lapply(PnetPnodes(net),
         function (nd) {maxCPTParam(nd,Mstepit,tol)})
}

maxCPTParam <- function (node, Mstepit=3, tol=sqrt(.Machine$double.eps)) {
  UseMethod("maxCPTParam")
}
