
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
    maxTableParams(net, Mstepit=Mstepit, tol=tol)

    ## Update parameters & convergence test
    iter <- iter + 1
    PnetBuildTables(net)
    llike[iter] <- calcPnetLLike(net,cases)
    converged <- (abs(llike[iter]-llike[iter-1]) < tol)
  }

  list(converged=converged,iter=iter,
       llikes=llikes[1:iter])
}
