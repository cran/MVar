\name{Biplot}

\alias{Biplot}

\title{Biplot graph.}

\description{Plots the Biplot graph.}

\usage{Biplot(Data, alpha = 0.5, Title = NA, xlabel = NA, ylabel = NA,
       Color = TRUE, Obs = TRUE, LinLab = NA)}

\arguments{
  \item{Data}{Data for plotting.}
  \item{alpha}{Representativeness of the individuals (alpha), representativeness of the variables (1 - alpha), being 0.5 the default.}
  \item{Title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{Obs}{Adds the observations to the graph (default = TRUE).}
  \item{LinLab}{Vector with the labels for the observations, if not set, assumes the default text.}
}

\value{
  \item{Biplot}{Biplot graph.}
  \item{Md}{Matrix eigenvalues.}
  \item{Mu}{Matrix U (eigenvectors).}
  \item{Mv}{Matrix V (eigenvectors).}
  \item{Coor_I}{Coordinates of the individuals.}
  \item{Coor_V}{Coordinates of the variables.}
  \item{PVar}{Proportion of the principal components.}
}

\references{
RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # set of quantitative data

Data <- DataQuan[,2:ncol(DataQuan)]
rownames(Data) <- DataQuan[,1]

Biplot(Data)

LinNames <- paste("C",1:nrow(Data), sep="")
Biplot(Data, alpha = 0.6, Title = "Biplot of data valuing individuals",
       xlabel = "X Axis", ylabel = "Y Axis", Color = TRUE, Obs = TRUE, 
       LinLab = LinNames)
       
Biplot(Data, alpha = 0.4, Title = "Graph valuing the variables",
       xlabel = "", ylabel = "", Color = FALSE, Obs = FALSE)
}

\keyword{Biplot}
