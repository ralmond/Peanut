\name{BuildNetManifest}
\alias{BuildNetManifest}
\title{Builds a network manifest from a list of Pnets
}
\description{

  A network manifest is a table of meta data about a colleciton of
  networks.  Each line corresponds to the specific network.  This
  manifest can be used by a network warehouse (\code{\link{Warehouse}})
  to recreate the network on demand.
}
\usage{
BuildNetManifest(Pnetlist)
}
\arguments{
  \item{Pnetlist}{
    A list of \code{\link{Pnet}} objects which will appear in the
    network manifest.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~

}
\value{

  An object of type \code{\link[base]{data.frame}} where the columns
  have the following values.
  \item{Name}{A character value giving the name of the network.  This
  should be unique for each row and normally must conform to variable
  naming conventions.}
  \item{Title }{An optional character value giving a longer human readable name
  for the netowrk.}
  \item{Hub}{If this model is incomplete without being joined to another
  network, then the name of the hub network.  Otherwise an empty
  character vector.}
  \item{Pathname}{The location of the file from which the network should
  be read or to which it should be written.}
  \item{Description}{An optional character value documenting the purpose
  of the network.}

  Note that the name column is regarded as a primary key to the table.
}
\references{

Almond, R. G. (presented 2017, August). Tabular views of Bayesian
networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
  Modeling Application Workshop 2017}. Symposium conducted at the
meeting of Association for Uncertainty in Artificial Intelligence,
Sydney, Australia. (International) Retrieved from \url{http://bmaw2017.azurewebsites.net/}
}
\author{Russell Almond}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Pnetlist) 
{
    Name <- character()
    Hub <- character()
    Title <- character()
    Pathname <- character()
    Description <- character()
    for (net in Pnetlist) {
        if (!is.Pnet(net)) {
            stop("Expected a list of Pnets, got ", net)
        }
        Name <- c(Name, PnetName(net))
        Title <- c(Title, PnetTitle(net))
        Hub <- c(Hub, PnetHub(net))
        Pathname <- c(Pathname, PnetPathname(net))
        Description <- c(Description, PnetDescription(net))
    }
    result <- data.frame(Name, Title, Hub, Pathname, Description, 
        stringsAsFactors = FALSE)
    rownames(result) <- Name
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
