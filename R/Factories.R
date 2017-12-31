#### This implements a factory paradigm for Models and Nodes.
## Warehouses are initialized with a default class and a manifest:
## a list of possible objects along with their metadata.  The factories
## maintain a cache of objects.  If the object is not found,
## metadata from the manifest is used to create it.

#########################################################
## Abstract Warehouse

Warehouse <- setRefClass("Warehouse",
                       fields=c(type="character",
                                manifest="data.frame",
                                inventory="list",
                                key="character",
                                packsep="character"),
                       prototype=list(type="UNDEFINED",
                                      manifest=data.frame(),
                                      inventory=list(),
                                      key="Name",
                                      packsep="/")
                       )
Warehouse$methods(
              ## Prototype doesn't work with reference classes, check for defaults.
              initialize = function(...,type="UNDEFINED",key="Name",
                                    packsep="/") {
                callSuper(...,type=type,key=key,packsep=packsep)
                })



Warehouse$methods(
            show = function () {
              cat("<Warehouse for ",type," with ",length(inventory),
                  " items in inventory>\n")
            },
            clear = function () {
              for (name in names(inventory)) {
                .self$free(name)
              }
              invisible(inventory)
            },
            manifestData = function (name) {
              name <- as.character(name)
              if (ispacked(name)) name <- unpackname(name)
              if (length(name) != length(key))
                stop("Expected name to contain elements",key)
              whch = rep(TRUE,nrow(manifest))
              for (i in 1:length(key)) {
                whch <- whch & manifest[[key[i]]] == name[i]
              }
              manifest[whch,,drop=FALSE]
            },
            supply = function (name) {
              val <- fetch(name)
              if (is.null(val))
                val <- make(name)
              val
            },
            packname = function (name) {
              ## This makes sure that the name is a single string.
              paste(name,collapse=packsep)
            },
            unpackname = function (name) {
              ## This undoes the previous operation
              strsplit(name,packsep,fixed=TRUE)[[1]]
            },
            ispacked = function (name) {
              length(name)==1 && grepl(packsep,name,fixed=TRUE)
            },
            isAvailable = function(name) {
              nrow(manifestData(name)) > 0L
            },
            fetch = function(name) {
              inventory[[packname(name)]]
            },
            make = function(name) {
              name <- as.character(name)
              pname <- packname(name)
              if (!is.null(inventory[[pname]]))
                free(name)
              dat = manifestData(name)
              val <- do.call(paste("Make",type,sep="."),
                             list(name,dat))
              inventory[[pname]] <<- val
              val
            },
            free = function(name) {
              pname <- packname(name)
              if (!is.null(inventory[[pname]]))
                Free(inventory[[pname]])
              inventory[[pname]] <<- NULL
              invisible(NULL)
            }
)

Make <- function(name,data) {
  stop("Use Warehouse$make instead.")
}

## Set this up as an S3 generic.
Free <- function (obj) {
  UseMethod("Free")
}
#setGeneric("Free") #Register it as an S4 generic, too.

## Default method is no special action required.
Free.default <- function (obj) {invisible(NULL)}



###########################################################
## Pnet Factory

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

PnetWarehouse <- setRefClass("PnetWarehouse",contains="Warehouse")


PnetWarehouse$methods(
            show = function () {
              cat("<Pnet Warehouse for ",type," with ",length(inventory),
                  " items in inventory\n>")
            },
            save = function (name,pathname) {
              if (missing(pathname)) {
                pathname <- manifestData(name)$Pathname
              }
              Save(inventory[[packname(name)]],pathname)
            },
            delete = function (name) {
              pname <- packname(name)
              if (!is.null(inventory[[pname]])) {
                Delete(inventory[[packname(name)]])
              }
              inventory[[pname]] <<- NULL
            },
            reload = function (name,pahtname,fromScratch=FALSE) {
              if (fromScratch) {
                new <- make(name)
              } else {
                if (missing(pathname)) {
                  pathname <- manifestData(name)$Pathname
                }
                free(name)
                new <- Reload(inventory[[packname(name)]],pathname)
                inventory[[packname(name)]] <<- new
              }
              invisible(new)
            })

Save<-function(net,pathname) {
  UseMethod("Save")
}
#setGeneric("Save")


Reload<-function(net,pathname)
    UseMethod("Reload")
#setGeneric("Reload")

Delete<-function(obj)
  UseMethod("Delete")
#setGeneric("Delete")


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


PnodeWarehouse <- setRefClass("PnodeWarehouse",contains="Warehouse",
                              fields=c(pnetwarehouse="PnetWarehouse"))

PnodeWarehouse$methods(
            initialize = function(...,type="UNDEFINED",
                                  key=c("Model","NodeName"),
                                  packsep="/") {
              callSuper(...,type=type,key=key,packsep=packsep)
            },
            show = function () {
              cat("<Pnode Warehouse for ",type," with ",length(inventory),
                  " items in inventory>\n")
              cat("Linked to Pnet Warehouse:\n")
              pnetwarehouse$show()
            },
            make = function(name) {
              name <- as.character(name)
              pname <- packname(name)
              if (!is.null(inventory[[pname]]))
                free(name)
              dat <- manifestData(name)
              net <- pnetwarehouse$supply(name[1])
              val <- do.call(paste("MakePnode",type,sep="."),
                             list(net,name[2],dat))
              inventory[[pname]] <<- val
              val
            },
            delete = function (name) {
              pname <- packname(name)
              node <- inventory[[pname]]
              if (!is.null(node)) {
                Delete(node)
              }
              inventory[[pname]] <<- NULL
            }
)

MakePnode <- function(net,name,data) {
  stop("Use PnodeWarehouse$make instead.")
}

########
## Ensure Parents

### The idea behind this funciton is to ensure that parents exist for an EM (spoke).
### Procedure.  First check parent names.  If this matches what is
### expected, assume that the net has proper parents or pseudo
### parents.
### If there are some parents missing, then use the node warehouse to
### generate the parents.
### Now check the newly generated nodes.  If any of them are from a
### different net, assume that they are references and create stubs to
### hold the references.
### The list of stubs is returned, so that it can be removed at a
### later date.

PnodeEnsureParents <- function (node, parentnames, nodewarehouse) {
  net <- PnodeNet(node)
  netname <- PnetName(net)
  hubname <- PnodeHub(net)
  pars <- PnodeParents(node)
  parnames <- PnodeParentNames(node)
  names(par) <- parnames
  stubs <- list()

  ## Check for missing parent nodes.
  misspars <- setdiff(parentnames,parnames)
  for (mpn in misspars) {
    newpar <- nodewarehouse$supply(mpn)
    if (nodewarehouse$isAvailable(c(netname,mpn))) {
      ## Missing node is in this net, just not built yet.  Build it.
      newnode <- nodewarehouse$supply(c(netname,mpn))
      pars[[mpn]] <- newnode
    } else if (nodewarehouse$isAvailable(c(hubname,mpn))) {
      ## Missing node is in hub.  We need to create a stub.
      orig <- nodewarehouse$supply(c(hubname,mpn))
      stub <- PnetMakeStubNode(net,orig)
      pars[[mpn]] <- stub
      stubs[[mpn]] <- stub
    } else {
      stop("Need parent ",mpn," for node ",PnodeName(node),
           "but can't find it.")
    }
  }
  ## Ensure parents are in the right order.
  PnodeParents(node) <- pars[parentnames]
  stubs
}
