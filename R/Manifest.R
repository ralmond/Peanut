
### Create table of Model meta-information
BuildNetManifest <- function (Pnetlist) {
  ## Node Level fields
  Name <- character()
  Hub <- character()
  Title <- character()
  Pathname <- character()
  Description <- character()
  Errs <- list()

  ## Main Loop
  for (net in Pnetlist) {
    if (!is.Pnet(net)) {
      flog.error("Expected a list of Pnets, got ",net)
      Errs <- c(Errs,simpleError(paste("Expected a list of Pnets, got ",net)))
      next
    }
    netnm <-PnetName(net)
    flog.debug("Processing network %s.",netnm)
    Name <-c(Name,PnetName(net))
    out <- flog.try({
      Title <- c(Title,PnetTitle(net))
      Hub <- c(Hub,PnetHub(net))
      Pathname <-c(Pathname,ifelse(is.null(PnetPathname(net)),
                                   "",PnetPathname(net)))
      Description <- c(Description,PnetDescription(net))
    }, context=sprintf("Net: %s.",netnm))
    if (is(out,'try-error')) {
      Errs <- c(Errs,out)
    }
  }
  if (length(Errs) >0L)
    stop("Errors encountered while building manifest.")
  result <- data.frame(Name,Title,Hub,Pathname,Description,
                       stringsAsFactors=FALSE)
  rownames(result) <- Name
  ## Seems there is a bug in the class checking mechanism,
  ## Easiest to just leave it as a data.frame
  ##class(result) <- c("NetManifest",class(result))
  result
}

##############################################################
## Pnode Functions

BuildNodeManifest <- function (Pnodelist) {
  ## Node Level fields
  Model <- character()
  ModelHub <- character()
  NodeName <- character()
  NodeTitle <- character()
  Nstates <- integer()
  NodeDescription <- character()
  NodeLabels <- character()
  Continuous <- logical()
  LowerBound <- numeric()
  UpperBound <- numeric()

  ## State Level Fields
  StateName <- character()
  StateTitle <- character()
  StateDescription <- character()
  StateValue <- numeric()
  Errs <- list()

  ## Main Loop
  for (nd in Pnodelist) {
    if (!is.Pnode(nd)) {
      flog.error("Expected a list of Pnodes, got ",nd)
      Errs <- c(Errs,simpleError(paste("Expected a list of Pnodes, got ",nd)))
      next
    }
    ndnm <- PnodeName(nd)
    netnm <- PnetName(PnodeNet(nd))
    flog.debug("Processing node %s in network %s.",ndnm,netnm)
    out <- flog.try({
      k <- PnodeNumStates(nd)
      ## Key elements, these must be repeated for the states.
      Model <- c(Model,rep(PnetName(PnodeNet(nd)),k))
      NodeName <-c(NodeName,rep(PnodeName(nd),k))
      ## Non-key elements -- these can be blank.
      ModelHub <- c(ModelHub,PnetHub(PnodeNet(nd)))
      NodeTitle <- c(NodeTitle,PnodeTitle(nd))
      Nstates <- c(Nstates,k)
      NodeDescription <- c(NodeDescription,PnodeDescription(nd))
      NodeLabels <- c(NodeLabels,paste(PnodeLabels(nd),collapse=","))
      Continuous <- c(Continuous,isPnodeContinuous(nd))
      ## Pad the node level fields with blank lines
      if (k>1) {
        ModelHub <- c(ModelHub,rep("",k-1))
        NodeTitle <- c(NodeTitle,rep("",k-1))
        Nstates <- c(Nstates,rep(NA_integer_,k-1))
        NodeDescription <- c(NodeDescription,rep("",k-1))
        NodeLabels <- c(NodeLabels,rep("",k-1))
        Continuous <- c(Continuous,rep(as.logical(NA),k-1))
      }
      StateName <- c(StateName,PnodeStates(nd))
      StateTitle <- c(StateTitle,PnodeStateTitles(nd))
      StateDescription <- c(StateDescription,PnodeStateDescriptions(nd))
      if (isPnodeContinuous(nd)) {
        StateValue <- c(StateValue,rep(NA_real_,k))
        bnds <- PnodeStateBounds(nd)
        LowerBound <- c(LowerBound,bnds[,1L])
        UpperBound <- c(UpperBound,bnds[,2L])
      } else {
        if (!is.null(PnodeStateValues(nd))) {
          StateValue <- c(StateValue,PnodeStateValues(nd))
        } else {
          StateValue <- c(StateValue,rep(NA,k))
        }
        LowerBound <- c(LowerBound,rep(NA_real_,k))
        UpperBound <- c(UpperBound,rep(NA_real_,k))
      }
    }, context=sprintf("Net: %s, Node: %s.",netnm,ndnm))
    if (is(out,'try-error')) {
      Errs <- c(Errs,out)
    }
  }
  if (length(Errs) >0L)
    stop("Errors encountered while building manifest.")
  result <- data.frame(Model,NodeName,ModelHub,NodeTitle,NodeDescription,
                       NodeLabels,Continuous,Nstates,StateName,StateTitle,
                       StateDescription, StateValue,LowerBound,UpperBound,
                       stringsAsFactors=FALSE)
  ## Seems there is a bug in the class checking mechanism,
  ## Easiest to just leave it as a data.frame
  ##class(result) <- c("NodeManifest",class(result))
  result

}


