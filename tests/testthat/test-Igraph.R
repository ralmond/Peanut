test_that("pnet2Igraph", {
  
  ## Temp directory for nets
  curd <- getwd()
  setwd(tempdir())
  on.exit(setwd(curd))
  
  sess <- RNetica::NeticaSession()
  RNetica::startSession(sess)
  
  netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv", 
                                  package="Peanut"),
                      row.names=1,stringsAsFactors=FALSE)
  
  nodeman1 <- read.csv(system.file("auxdata", "Mini-PP-Nodes.csv", 
                                   package="Peanut"),
                       row.names=1,stringsAsFactors=FALSE)
  
  omegamat <- read.csv(system.file("auxdata", "miniPP-omega.csv",
                                   package="Peanut"),
                       row.names=1,stringsAsFactors=FALSE)
  

  ## Network and node warehouse, to create networks and nodes on demand.
  Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name")
  
  Nodehouse <- PNetica::NNWarehouse(manifest=nodeman1,
                           key=c("Model","NodeName"),
                           session=sess)
  
  ## Build the proficiency model first:
  CM <- WarehouseSupply(Nethouse,"miniPP_CM")
  CM1 <- Omega2Pnet(omegamat,CM,Nodehouse,override=TRUE,debug=TRUE)
  
  ig <- pnet2Igraph(CM1)
  
  expect_equal(length(igraph::V(ig)),length(PnetPnodes(CM1)))
  
  for (anode in PnetPnodes(CM1)) {
    pnames <- PnodeParentNames(anode)
    ipnames <- igraph::vertex_attr(ig,"name",ig[[to=PnodeName(anode)]][[1]])
    testthat::expect_setequal(PnodeParentNames(!!anode),ipnames)
  }
})
