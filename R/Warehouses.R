

ClearWarehouse <- function (warehouse)
  UseMethod("ClearWarehouse")
setGeneric("ClearWarehouse")

WarehouseManifest <- function (warehouse)
  UseMethod("WarehouseManifest")
setGeneric("WarehouseManifest")

"WarehouseManifest<-" <- function (warehouse,value)
  UseMethod("WarehouseManifest<-")
setGeneric("WarehouseManifest<-")

WarehouseData <- function (warehouse,name)
    UseMethod("WarehouseData")
setGeneric("WarehouseData")

WarehouseSupply <- function(warehouse,name)
  UseMethod("WarehouseSupply")
setGeneric("WarehouseSupply")


setMethod("WarehouseSupply", c("ANY"), function(warehouse,name) {
  val <- WarehouseFetch(warehouse,name)
  if (is.null(val))
    val <- WarehouseMake(warehouse,name)
  val
})

WarehouseFetch <- function (warehouse,name)
    UseMethod("WarehouseFetch")
setGeneric("WarehouseFetch")

WarehouseMake <- function (warehouse,name)
    UseMethod("WarehouseMake")
setGeneric("WarehouseMake")

## Serial should have components name and data.
WarehouseUnpack <- function(warehouse,serial)
  UseMethod("WarehouseUnpack")
setGeneric("WarehouseUnpack")


WarehouseFree <- function (warehouse,name)
    UseMethod("WarehouseFree")
setGeneric("WarehouseFree")

is.PnodeWarehouse <- function (obj) {
  is(obj,"PnodeWarehouse")
}
setGeneric("is.PnodeWarehouse")

is.PnetWarehouse <- function (obj) {
  is(obj,"PnetWarehouse")
}
setGeneric("is.PnetWarehouse")
