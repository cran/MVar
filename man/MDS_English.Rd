\name{MDS}

\alias{MDS}

\title{Multidimensional Scaling (MDS).}

\description{Performs Multidimensional Scaling (MDS) on a data set.}

\usage{MDS(data, distance = "euclidean", title = NA, xlabel = NA,  
    ylabel = NA, posleg = 2, boxleg = TRUE, axes = TRUE, 
    size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
    class = NA, classcolor = NA, savptc = FALSE, width = 3236, 
    height = 2000, res = 300)}

\arguments{
  \item{data}{Data to be analyzed.}
  \item{distance}{Metric of the distance: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" or "minkowski".}
  \item{title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}  
  \item{posleg}{0 with no caption,\cr
                1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner (default),\cr
                3 for caption in the right lower corner,\cr
                4 for caption in the left lower corner.}
  \item{boxleg}{Puts the frame in the caption (default = TRUE).} 
  \item{axes}{Plot the X and Y axes (default = TRUE).}  
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{linlab}{Vector with the labels for the observations.}
  \item{class}{Vector with names of data classes.}
  \item{classcolor}{Vector with the colors of the classes.}
  \item{savptc}{Saves graphics images to files (default = FALSE).} 
  \item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
  \item{height}{Graphics images height when savptc = TRUE (default = 2000).}
  \item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
}

\value{{Multidimensional Scaling.}
\item{mtxD}{Matrix of the distances.}
}

\references{
Mingoti, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # data set

data <- iris[,1:4]

cls <- iris[,5] # data class

md <- MDS(data = data, distance = "euclidean", title = NA, xlabel = NA,  
          ylabel = NA, posleg = 2, boxleg = TRUE, axes = TRUE, color = TRUE,
          linlab = NA, class = cls, classcolor = c("goldenrod3","gray53","red"),
          savptc = FALSE, width = 3236, height = 2000, res = 300)
          
print("Matrix of the distances:"); md$mtxD
}

\keyword{Multidimensional Scaling}
\keyword{MDS}
