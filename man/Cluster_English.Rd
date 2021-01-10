\name{Cluster}

\alias{Cluster}

\title{Cluster Analysis.}

\description{Performs hierarchical and non-hierarchical cluster analysis in a data set.}

\usage{Cluster(data, titles = NA, hierarquico = TRUE, analise = "Obs",  
        corabs = FALSE, normaliza = FALSE, distance = "euclidean",  
        method = "complete", horizontal = FALSE, numgrupos = 0,
        lambda = 2, savptc = FALSE, width = 3236, height = 2000, 
        res = 300, casc = TRUE) }

\arguments{
  \item{data}{Data to be analyzed.}
  \item{titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{hierarquico}{Hierarchical groupings (default = TRUE), for non-hierarchical groupings (method K-Means), only for case Analysis = "Obs".}
  \item{analise}{"Obs" for analysis on observations (default), "Var" for analysis on variables.}
  \item{corabs}{Matrix of absolute correlation case Analyze = "Var" (default = FALSE).}
  \item{normaliza}{Normalizes the data only for case Analyze = "Obs" (default = TRUE).}
  \item{distance}{Metric of the distances in case of hierarchical groupings: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" or "minkowski". Case Analysis = "Var" the metric will be the correlation matrix, according to corabs.}
  \item{method}{Method for analyzing hierarchical groupings: "complete" (default), "ward.D", "ward.D2", "single", "average", "mcquitty", "median" or "centroid".}
  \item{horizontal}{Horizontal dendrogram (default = FALSE).}
  \item{numgrupos}{Number of groups to be formed.}
  \item{lambda}{Value used in the minkowski distance.}
  \item{savptc}{Saves graphics images to files (default = FALSE).} 
  \item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
  \item{height}{Graphics images height when savptc = TRUE (default = 2000).}
  \item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
  \item{casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{{Several graphics.}
  \item{tabres}{Table with similarities and distances of the groups formed.}
  \item{groups}{Original data with groups formed.}
  \item{resgroups}{Results of the groups formed.}
  \item{sqt}{Total sum of squares.}
  \item{mtxD}{Matrix of the distances.}
}

\references{
MINGOTI, S. A. \emph{analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # set of quantitative data

data <- DataQuan[,2:8]

rownames(data) <- DataQuan[1:nrow(DataQuan),1]

res <- Cluster(data, titles = NA, hierarquico = TRUE, analise = "Obs",
               corabs = FALSE, normaliza = FALSE, distance = "euclidean", 
               method = "ward.D", horizontal = FALSE, numgrupos = 2,
               savptc = FALSE, width = 3236, height = 2000, res = 300, 
               casc = FALSE)

print("Table with similarities and distances:"); res$tabres
print("groups formed:"); res$groups
print("Table with the results of the groups:"); res$resgroups
print("Total sum of squares:"); res$sqt
print("distance Matrix:"); res$mtxD 
 
write.table(file=file.path(tempdir(),"SimilarityTable.csv"), res$tabres, sep=";",
            dec=",",row.names = FALSE) 
write.table(file=file.path(tempdir(),"GroupData.csv"), res$groups, sep=";",
            dec=",",row.names = TRUE) 
write.table(file=file.path(tempdir(),"GroupResults.csv"), res$resgroups, sep=";",
            dec=",",row.names = TRUE)  
}

\keyword{Cluster Analysis}
\keyword{Cluster}
