
### Create table of Model meta-information
BuildNetManifest <- function (Pnetlist) {
  ## Node Level fields
  Name <- character()
  Hub <- character()
  Title <- character()
  Pathname <- character()
  Description <- character()
  ## Main Loop
  for (net in Pnetlist) {
    if (!is.Pnet(net)) {
      stop("Expected a list of Pnets, got ",net)
    }
    Name <-c(Name,PnetName(net))
    Title <- c(Title,PnetTitle(net))
    Hub <- c(Hub,PnetHub(net))
    Pathname <-c(Pathname,PnetPathname(net))
    Description <- c(Description,PnetDescription(net))
  }
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

  ## State Level Fields
  StateName <- character()
  StateTitle <- character()
  StateDescription <- character()
  StateValue <- numeric()

  ## Main Loop
  for (nd in Pnodelist) {
    if (!is.Pnode(nd)) {
      stop("Expected a list of Pnodes, got ",nd)
    }
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
    ## Pad the node level fields with blank lines
    if (k>1) {
      ModelHub <- c(ModelHub,rep("",k-1))
      NodeTitle <- c(NodeTitle,rep("",k-1))
      Nstates <- c(Nstates,rep(NA_integer_,k-1))
      NodeDescription <- c(NodeDescription,rep("",k-1))
      NodeLabels <- c(NodeLabels,rep("",k-1))
    }
    StateName <- c(StateName,PnodeStates(nd))
    StateTitle <- c(StateTitle,PnodeStateTitles(nd))
    StateDescription <- c(StateDescription,PnodeStateDescriptions(nd))
    if (!is.null(PnodeStateValues(nd))) {
      StateValue <- c(StateValue,PnodeStateValues(nd))
    } else {
      StateValue <- c(StateValue,rep(NA,k))
    }
  }
  result <- data.frame(Model,NodeName,ModelHub,NodeTitle,NodeDescription,
                       NodeLabels,Nstates,StateName,StateTitle,
                       StateDescription, StateValue,
                       stringsAsFactors=FALSE)
  ## Seems there is a bug in the class checking mechanism,
  ## Easiest to just leave it as a data.frame
  ##class(result) <- c("NodeManifest",class(result))
  result

}


