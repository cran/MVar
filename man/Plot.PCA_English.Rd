\name{Plot.PCA}

\alias{Plot.PCA}

\title{Graphs of the Principal Components Analysis (PCA).}

\description{Graphs of the Principal Components Analysis (PCA).}

\usage{Plot.PCA(PC, Titles = NA, xlabel = NA, ylabel = NA,
         Color = TRUE, LinLab = NA, Casc = TRUE)}

\arguments{
  \item{PC}{Data of the PCA function.}
  \item{Titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{LinLab}{Vector with the labels for the observations, if not set, assumes the default text.}
  \item{Casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{Returns several graphs.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PCA}}}

\examples{
data(DataQuan) # set of quantitative data

Dat <- DataQuan[,2:8]

rownames(Dat) <- DataQuan[1:nrow(DataQuan),1]

PC <- PCA(Dat, 2) # performs the PCA

Tit = c("Scree-plot","Graph of the Observations","Circle of Correlation")

Plot.PCA(PC, Titles = Tit, xlabel = NA, ylabel = NA,
         Color = TRUE, LinLab = NA, Casc = TRUE)
}

\keyword{Principal Components Analysis}
\keyword{PCA}
