\name{Cluster}

\alias{Cluster}

\title{Cluster Analysis.}

\description{Performs hierarchical and non-hierarchical cluster analysis in a data set.}

\usage{Cluster(Data, Titles = NA, Hierarquico = TRUE, Analise = "Obs",  
        CorAbs = FALSE, Normaliza = FALSE, Distance = "euclidean",  
        Method = "complete", Horizontal = FALSE, NumGrupos = 0,
        Casc = TRUE)}

\arguments{
  \item{Data}{Data to be analyzed.}
  \item{Titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{Hierarquico}{Hierarchical groupings (default = TRUE), for non-hierarchical groupings (Method K-Means), only for case Analysis = "Obs".}
  \item{Analise}{"Obs" for analysis on observations (default), "Var" for analysis on variables.}
  \item{CorAbs}{Matrix of absolute correlation case Analyze = "Var" (default = FALSE).}
  \item{Normaliza}{Normalizes the data only for case Analyze = "Obs" (default = TRUE).}
  \item{Distance}{Metric of the distances in case of hierarchical groupings: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" or "minkowski". Case Analysis = "Var" the metric will be the correlation matrix, according to CorAbs.}
  \item{Method}{Method for analyzing hierarchical groupings: "complete" (default), "ward.D", "ward.D2", "single", "average", "mcquitty", "median" or "centroid".}
  \item{Horizontal}{Horizontal dendrogram (default = FALSE).}
  \item{NumGrupos}{Number of groups to be formed.}
  \item{Casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{{Several graphics.}
  \item{TabRes}{Table with similarities and distances of the groups formed.}
  \item{Groups}{Original data with groups formed.}
  \item{ResGroups}{Results of the groups formed.}
  \item{SQT}{Total sum of squares.}
  \item{MatrixD}{Matrix of the distances.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de Methods de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # set of quantitative data

Data <- DataQuan[,2:8]

rownames(Data) <- DataQuan[1:nrow(DataQuan),1]

Res <- Cluster(Data, Hierarquico = TRUE, Analise = "Obs", CorAbs = FALSE, 
               Normaliza = FALSE, Distance = "euclidean", Method = "ward.D", 
               Horizontal = FALSE, NumGrupos = 2)

print("Table with similarities and distances:"); Res$TabRes
print("Groups formed:"); Res$Groups
print("Table with the results of the groups:"); Res$ResGroups
print("Total sum of squares:"); Res$SQT
print("Distance Matrix:"); Res$MatrixD 
 
write.table(file=file.path(tempdir(),"SimilarityTable.csv"), Res$TabRes, sep=";",
            dec=",",row.names = FALSE) 
write.table(file=file.path(tempdir(),"GroupedData.csv"), Res$Groups, sep=";",
            dec=",",row.names = TRUE) 
write.table(file=file.path(tempdir(),"GroupResults.csv"), Res$ResGroups, sep=";",
            dec=",",row.names = TRUE) 
}

\keyword{Cluster Analysis}
\keyword{Cluster}
