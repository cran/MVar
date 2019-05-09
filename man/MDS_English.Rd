\name{MDS}

\alias{MDS}

\title{Multidimensional Scaling (MDS).}

\description{Performs Multidimensional Scaling (MDS) on a data set.}

\usage{MDS(Data, Distance = "euclidean", Title = NA, xlabel = NA,  
    ylabel = NA, PosLeg = 2, BoxLeg = TRUE, Axis = TRUE, 
    Color = TRUE, LinLab = NA, Class = NA)}

\arguments{
  \item{Data}{Data to be analyzed.}
  \item{Distance}{Metric of the distance: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" or "minkowski".}
  \item{Title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}  
  \item{PosLeg}{0 with no caption,\cr
                1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner (default),\cr
                3 for caption in the right lower corner,\cr
                4 for caption in the left lower corner.}
  \item{BoxLeg}{Puts the frame in the caption (default = TRUE).} 
  \item{Axis}{Plot the X and Y axes (default = TRUE).}  
  \item{Color}{Colored graphics (default = TRUE).}
  \item{LinLab}{Vector with the labels for the observations.}
  \item{Class}{Vector with names of data classes.}
}

\value{{Multidimensional Scaling.}
\item{MatrixD}{Matrix of the distances.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # data set

Data <- iris[,1:4]

cls <- iris[,5] # data class

MD <- MDS(Data, Distance = "euclidean", Title = NA, xlabel = NA,  
          ylabel = NA, PosLeg = 2, BoxLeg = TRUE, Axis = TRUE, 
          Color = TRUE, LinLab = NA, Class = cls)
          
print("Matrix of the distances:"); MD$MatrixD
}

\keyword{Multidimensional Scaling}
\keyword{MDS}
