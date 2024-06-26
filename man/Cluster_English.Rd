\name{Cluster}

\alias{Cluster}

\title{Cluster Analysis.}

\description{Performs hierarchical and non-hierarchical cluster analysis in a data set.}

\usage{Cluster(data, titles = NA, hierarquic = TRUE, analysis = "Obs",  
        cor.abs = FALSE, normalize = FALSE, distance = "euclidean",  
        method = "complete", horizontal = FALSE, num.groups = 0,
        lambda = 2, savptc = FALSE, width = 3236, height = 2000, 
        res = 300, casc = TRUE)}

\arguments{
  \item{data}{Data to be analyzed.}
  \item{titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{hierarquic}{Hierarchical groupings (default = TRUE), for non-hierarchical groupings (method K-Means), only for case 'analysis' = "Obs".}
  \item{analysis}{"Obs" for analysis on observations (default), "Var" for analysis on variables.}
  \item{cor.abs}{Matrix of absolute correlation case 'analysis' = "Var" (default = FALSE).}
  \item{normalize}{Normalize the data only for case 'analysis' = "Obs" (default = FALSE).}
  \item{distance}{Metric of the distances in case of hierarchical groupings: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" or "minkowski". Case Analysis = "Var" the metric will be the correlation matrix, according to cor.abs.}
  \item{method}{Method for analyzing hierarchical groupings: "complete" (default), "ward.D", "ward.D2", "single", "average", "mcquitty", "median" or "centroid".}
  \item{horizontal}{Horizontal dendrogram (default = FALSE).}
  \item{num.groups}{Number of groups to be formed.}
  \item{lambda}{Value used in the minkowski distance.}
  \item{savptc}{Saves graphics images to files (default = FALSE).} 
  \item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
  \item{height}{Graphics images height when savptc = TRUE (default = 2000).}
  \item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
  \item{casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{{Several graphics.}
  \item{tab.res}{Table with similarities and distances of the groups formed.}
  \item{groups}{Original data with groups formed.}
  \item{res.groups}{Results of the groups formed.}
  \item{R.sqt}{Result of the R squared.}
  \item{sum.sqt}{Total sum of squares.}
  \item{mtx.dist}{Matrix of the distances.}
}

\references{
Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

Mingoti, S. A. \emph{analysis de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Ferreira, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani
}

\examples{
data(DataQuan) # set of quantitative data

data <- DataQuan[,2:8]

rownames(data) <- DataQuan[1:nrow(DataQuan),1]

res <- Cluster(data, titles = NA, hierarquic = TRUE, analysis = "Obs",
               cor.abs = FALSE, normalize = FALSE, distance = "euclidean", 
               method = "ward.D", horizontal = FALSE, num.groups = 2,
               savptc = FALSE, width = 3236, height = 2000, res = 300, 
               casc = FALSE)

print("R squared:"); res$R.sqt
# print("Total sum of squares:"); res$sum.sqt
print("Groups formed:"); res$groups
# print("Table with similarities and distances:"); res$tab.res
# print("Table with the results of the groups:"); res$res.groups
# print("Distance Matrix:"); res$mtx.dist 
 
write.table(file=file.path(tempdir(),"SimilarityTable.csv"), res$tab.res, sep=";",
            dec=",",row.names = FALSE) 
write.table(file=file.path(tempdir(),"GroupData.csv"), res$groups, sep=";",
            dec=",",row.names = TRUE) 
write.table(file=file.path(tempdir(),"GroupResults.csv"), res$res.groups, sep=";",
            dec=",",row.names = TRUE)  
}

\keyword{Cluster Analysis}
\keyword{Cluster}
