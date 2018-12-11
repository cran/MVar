\name{MDS}

\alias{MDS}

\title{Multidimensional Scaling (MDS).}

\description{Performs Multidimensional Scaling (MDS) on a data set.}

\usage{MDS(Data, Distance = "euclidean", Axis = TRUE, Title = NA,
    xlabel = NA, ylabel = NA, Color = TRUE, LinLab = NA)}

\arguments{
  \item{Data}{Data to be analyzed.}
  \item{Distance}{Metric of the distance: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" or "minkowski".}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{Axis}{Plot the X and Y axes (default = TRUE).}
  \item{Title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{LinLab}{Vector with the labels for the observations, if not set, assumes the default text.}
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
data(DataQuan) # set of quantitative data

Data <- DataQuan[,2:8]

rownames(Data) <- DataQuan[1:nrow(DataQuan),1]

MD <- MDS(Data, Distance = "euclidean", Axis = TRUE, Title = NA,
          xlabel = NA, ylabel = NA, Color = TRUE, LinLab = NA)
          
print("Matrix of the distances:"); MD$MatrixD
}

\keyword{Multidimensional Scaling}
\keyword{MDS}
