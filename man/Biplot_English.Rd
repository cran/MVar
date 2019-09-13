\name{Biplot}

\alias{Biplot}

\title{Biplot graph.}

\description{Plots the Biplot graph.}

\usage{Biplot(data, alpha = 0.5, title = NA, xlabel = NA, ylabel = NA,
       size = 1.1, grid = TRUE, color = TRUE, var = TRUE,
       obs = TRUE, linlab = NA, class = NA, posleg = 2,
       boxleg = TRUE, axes = TRUE)}

\arguments{
  \item{data}{Data for plotting.}
  \item{alpha}{Representativeness of the individuals (alpha), representativeness of the variables (1 - alpha), being 0.5 the default.}
  \item{title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{var}{Adds the variable projections to graph (default = TRUE).}
  \item{obs}{Adds the observations to graph (default = TRUE).}
  \item{linlab}{Vector with the labels for the observations.}
  \item{class}{Vector with names of data classes.}
  \item{posleg}{0 with no caption,\cr
                1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner (default),\cr
                3 for caption in the right lower corner,\cr
                4 for caption in the left lower corner.}
  \item{boxleg}{Puts the frame in the caption (default = TRUE).}
  \item{axes}{Plots the X and Y axes (default = TRUE).}
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
data(iris) # dataset

data <- iris[,1:4]

Biplot(data)


cls <- iris[,5]
res <- Biplot(data, alpha = 0.6, title = "Biplot of data valuing individuals",
              class = cls, posleg = 2, boxleg = FALSE, axes = TRUE)
print(res$pvar)


res <- Biplot(data, alpha = 0.4, title = "Graph valuing the variables",
              xlabel = "", ylabel = "", color = FALSE, obs = FALSE) 
print(res$pvar)

}

\keyword{Biplot}
