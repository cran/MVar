\name{Biplot}

\alias{Biplot}

\title{Biplot graph.}

\description{Plots the Biplot graph.}

\usage{Biplot(data, alpha = 0.5, title = NA, xlabel = NA, ylabel = NA,
       size = 1.1, grid = TRUE, color = TRUE, obs = TRUE, linlab = NA)}

\arguments{
  \item{data}{Data for plotting.}
  \item{alpha}{Representativeness of the individuals (alpha), representativeness of the variables (1 - alpha), being 0.5 the default.}
  \item{title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{obs}{Adds the observations to the graph (default = TRUE).}
  \item{linlab}{Vector with the labels for the observations, if not set, assumes the default text.}
}

\value{
  \item{Biplot}{Biplot graph.}
  \item{Md}{Matrix eigenvalues.}
  \item{Mu}{Matrix U (eigenvectors).}
  \item{Mv}{Matrix V (eigenvectors).}
  \item{coorI}{Coordinates of the individuals.}
  \item{coorV}{Coordinates of the variables.}
  \item{pvar}{Proportion of the principal components.}
}

\references{
RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # set of quantitative data

data <- DataQuan[,2:ncol(DataQuan)]
rownames(data) <- DataQuan[,1]

Biplot(data)

LinNames <- paste("C",1:nrow(data), sep="")
Biplot(data, alpha = 0.6, title = "Biplot of data valuing individuals",
       xlabel = "X Axis", ylabel = "Y Axis", color = TRUE, obs = TRUE, 
       linlab = LinNames)
       
Biplot(data, alpha = 0.4, title = "Graph valuing the variables",
       xlabel = "", ylabel = "", color = FALSE, obs = FALSE)
}

\keyword{Biplot}
