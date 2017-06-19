\name{Warehouse-class}
\Rdversion{1.1}
\docType{class}
\alias{Warehouse-class}
\alias{Warehouse}
\title{Class \code{"Warehouse"}}
\description{

  A warehouse combines an inventory with a factory for making new
  objects of the specified type.  The idea is that when an object is
  requested (using the \code{\$supply()} method), the warehouse will
  first look in its inventory and if there it will return the cached
  object.  If not, it will call and appropriate \code{Make} function
  using data from the \code{manifest}.

  Each warehouse object must be given a value for its \code{type} slot.
  This should be a character slot naming a class (S3 or S4 classes are
  both fine).  It expects that there exists a method called
  \code{Make.\emph{type}} which constructs objects of the appropriate
  class.  This takes two arguments:  \code{name} (a character) and
  \code{data} (a data frame).

  The \code{manifest} argument, which must be supplied to the
  constructor, contains data to use for object construction.  This is
  represented as a data frame, and its fields must be compatible with
  the \code{Make.\emph{type}} function.  The method
  \code{\$manifestData(name)} returns the appropriate rows of the data
  frame.  The appropriate rows of the data frame are the ones for which
  the \code{Name} column of the manifest match the \code{name} argument
  to the method.  The name of the column searched can be changed by
  changing the \code{key} slot of the object.  Note that
  \code{\$manifestData(name)} always returns a data frame (even with one
  row).  It could return more than one row as more complex objects might
  require more than one row in the database to describe them.  (For example,
  \code{PnodeWarehouse}).

  For most cases, the \code{name} argument to most methods will be a
  character giving a unique identifier from the object.  This is used as
  a key for both the \code{inventory} and \code{manifest}.  In some
  cases, that might not be suffficient.  For example, in a Bayesian
  network, node object only have unique names within a given network.
  Thus, the key for a node is a character vector with two elements, one
  for the the net and one for the node.  This can be accomplished by
  making the \code{key} slot contian a character vector.  The
  \code{name} argument to the various methods of the factory should then
  be character vectors of the same length.  However, the inventory still
  needs a unique key which is a single character vector.  The
  \code{$packname()}, \code{$unpackname()} and \code{$ispacked()}
  methods take care of that.  The packing is done by pasting the
  elements of name together using the value of the \code{packsep} slot
  as a separator.  This should be a character string which is not used
  in names.

  Finally, the \code{$free(name)} method releases and item from the
  inventory and the the \code{$clear()} method clears the inventory.
  These functions each call the \code{\link{Free}} function on the
  inventory object so that any necessary cleanup can be done.  This is a
  generic function which should operate as both a S3 and S4 generic.  It
  has a default method which does nothing.

  
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
  None yet.
}
\author{
  Russell Almond
}
\note{

  To be used with the warehouse protocol, the \code{Make.\emph{type}}
  method must be defined for the class.  The \code{\link{Free}} method
  should be defined for \emph{type} as well if necessary.

}

\seealso{

  This is an abstract class, meant to be specialized for various types of
  object. See \code{\link{PnetWarehouse-class}} and
  \code{\link{PnodeWarehouse-class}} for examples.

  \code{\link{Free}}

}
\examples{
showClass("Warehouse")

### Supplying an S3 class

S3Product <- function (name,data) {
  result <- list(Name=name,Data=data)
  class(result) <- "S3Product"
  result
}
Make.S3Product <- function (name,data) {
  result <- S3Product(name,data)
  cat("Just made: \n")
  show(result)
  result
}

show.S3Product <- function(obj) {
  cat("An S3Product with name",obj$name,"\n")
  cat("and data\n")
  print(data)
  invisible(obj)
}

Free.S3Product <- function(obj) {
  cat("Freeing S3Product with name",obj$Name,"\n")
}

grades <-
data.frame(Name=c("Abalard","Balthasar","Casper"),Grade=c(4,3,2),
  stringsAsFactors=FALSE)

S3Warehouse <- new("Warehouse",type="S3Product",manifest=grades)
stopifnot(is.data.frame(S3Warehouse$manifestData("Abalard")))

a <- S3Warehouse$supply("Abalard")
aa <- S3Warehouse$supply("Abalard")
stopifnot(all.equal(a,aa))



b <- S3Warehouse$fetch("Balthasar")
stopifnot(is.null(b))
b <- S3Warehouse$make("Balthasar")
bb <- S3Warehouse$fetch("Balthasar")
stopifnot(all.equal(b,bb))

c <- S3Warehouse$make("Casper")
cc <- S3Warehouse$free("Casper")
ccc <- S3Warehouse$fetch("Casper")
stopifnot(!is.null(c),is.null(cc),is.null(ccc))

S3Warehouse$clear()
aaa <- S3Warehouse$fetch("Abalard")
bbb <- S3Warehouse$fetch("Balthasar")
stopifnot(is.null(aaa),is.null(bbb),length(S3Warehouse$inventory)==0L)



### Supplying an S4 class

S4Product <- setClass("S4Product",
    slots=c(Name="character",Data="data.frame"))

Make.S4Product <- function (name,data) {
   cat("Making a new S4Product: ",name,"\n")
   new("S4Product",Name=name,Data=data)
}

setMethod("Free","S4Product",
   function(obj) {
    cat("Freeing S4Product with name",obj@Name,"\n")
   })

S4Warehouse <- new("Warehouse",type="S4Product",manifest=grades)
stopifnot(is.data.frame(S4Warehouse$manifestData("Abalard")))

stopifnot(S4Warehouse$isAvailable("Casper"),!S4Warehouse$isAvailable("Francine"))


a <- S4Warehouse$supply("Abalard")
aa <- S4Warehouse$supply("Abalard")
stopifnot(all.equal(a,aa))

b <- S4Warehouse$fetch("Balthasar")
stopifnot(is.null(b))
b <- S4Warehouse$make("Balthasar")
bb <- S4Warehouse$fetch("Balthasar")
stopifnot(all.equal(b,bb))

c <- S4Warehouse$make("Casper")
cc <- S4Warehouse$free("Casper")
ccc <- S4Warehouse$fetch("Casper")
stopifnot(!is.null(c),is.null(cc),is.null(ccc))

S4Warehouse$clear()
aaa <- S4Warehouse$fetch("Abalard")
bbb <- S4Warehouse$fetch("Balthasar")
stopifnot(is.null(aaa),is.null(bbb),length(S4Warehouse$inventory)==0L)


#####
## Test multiple data rows
grades2 <-
data.frame(Name=rep(c("Abalard","Balthasar","Casper"),2),
           Class=rep(c("Grammar","Rhetoric"),each=3),
           Grade=c(4,3,2,3.5,2.5,1.5),
           stringsAsFactors=FALSE)

## Unit is person, data has multiple rows
PersonWarehouse <- new("Warehouse",type="S3Product",manifest=grades2)
ad <- PersonWarehouse$manifestData("Abalard")
a <- PersonWarehouse$supply("Abalard")
stopifnot(nrow(ad)==2L,nrow(a$Data)==2L)

## Unit is a class/person.  Key is multiples.
ClassPersonWarehouse <- new("Warehouse",type="S4Product",
                            manifest=grades2,key=c("Class","Name"))
gcp <- ClassPersonWarehouse$packname(c("Grammar","Casper"))
gcu <- ClassPersonWarehouse$unpackname(c("Grammar/Casper"))
stopifnot(gcp=="Grammar/Casper",all(gcu==c("Grammar","Casper")),
   !ClassPersonWarehouse$ispacked(c("Grammar","Casper")),
    ClassPersonWarehouse$ispacked(c("Grammar/Casper")),
   !ClassPersonWarehouse$ispacked(c("Grammar.Casper")))

c <- ClassPersonWarehouse$supply(c("Grammar","Casper"))
stopifnot(c@Data$Grade==2)
stopifnot(grepl("Grammar/Casper",names(ClassPersonWarehouse$inventory)))
ClassPersonWarehouse$free(c("Grammar","Casper"))
stopifnot(!grepl("Grammar/Casper",names(ClassPersonWarehouse$inventory)))

}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{type}:}{A \code{character} scalar giving the class name
      for objects warehoused by this object.  The default value is
      \dQuote{UNDEFINED}, which should produce intelligible error messages.}
    \item{\code{manifest}:}{Object of class \code{data.frame} giving the
      data necessary to create objects which are not yet in the
      inventory.  It is assumed that it has columns corresponding to the
      values of the \code{key} slot.}
    \item{\code{inventory}:}{Object of class \code{list} containing
      cached values with keys equal to the packed name.  Normally, this
      should not be modified by the user. }
    \item{\code{key}:}{Object of class \code{character} giving the names
      of the columns in the \code{manifest} to use as keys for the data.}
    \item{\code{packsep}:}{A \code{character} scalar, default value
      \sQuote{/}.  This is the separator used when packing vector valued
      names. } 
  }
}
\section{Methods}{

  For most of these methods the \code{name} argument should be a
  character vector whose length corresponds to the \code{key} slot of
  the object identifying the object to be manipulated.
  
  \describe{
    \item{\code{supply(name)}:}{This is the principle method.  If a
      cached object corresponding to \code{name} exists, it is returned.
      If not a new object is made using the data from the manifest.
      This method calls the \code{fetch} and \code{make} methods.}
    \item{\code{fetch(name)}:}{This checks to see if a method
      corresponding to \code{name} is in the inventory.  If it is, it is
      returned.  If not, \code{NULL} is returned.}
    \item{\code{make(name)}:}{This makes a new object corresponding to
      \code{name}, using the \code{Make.\emph{type}} function.  The
      returned object is cached in the inventory.  If an object
      corresponding to \code{name} is already in the inventory, it is
      freed (with a call to the \code{free} method) first.  The newly
      created object is returned. }
    \item{\code{free(name)}:}{This removes an object from the inventory.
      If the object exists, the \code{\link{Free}} generic function is
      called to handle any necessary cleanup.  It is harmless to call
      this function if the object does not exist.}
    \item{\code{clear()}:}{This calls \code{free} on all objects in the
      inventory.}
    \item{\code{manifestData(name)}:}{ This finds the rows of the
      \code{manifest} associated with name.  Return value is a data frame. }
    \item{\code{isAvailable(name)}:}{ Returns true if a manifest entry
      is available for \code{name}, false otherwise.}
    \item{\code{ispacked(name)}:}{Checks to see if the name is packed
      (TRUE; single string with components separated by \code{packsep}) or
      unpacked (FALSE; character vector with compnents corresponding to
      the elements).} 
    \item{\code{packname(name)}:}{This converts a name as a character
      vector into a single character string.  For example,
      \code{c("school", "teacher")} would be converted to
      \code{"school/teacher"}, where \sQuote{/} is the value of the
      \code{packsep} slot.}
    \item{\code{unpackname(name)}:}{This coverts the name as a character
      scalar back to a character vector.  For example,
      \code{"school/teacher"} would be converted to \code{c("school","teacher")}.}
    \item{\code{show()}:}{Prints the type and number of items in the inventory.}
  }
}
