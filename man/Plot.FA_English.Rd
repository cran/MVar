\name{Plot.FA}

\alias{Plot.FA}

\title{Graphs of the Factorial Analysis (FA).}

\description{Graphs of the Factorial Analysis (FA).}

\usage{Plot.FA(FA, Titles = NA, xlabel = NA, ylabel = NA,
        Color = TRUE, LinLab = NA, Casc = TRUE)}

\arguments{
  \item{FA}{Data of the FA function.}
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

\seealso{\code{\link{FA}}}

\examples{
data(DataQuan) # database

Data <- DataQuan[,2:ncol(DataQuan)]

rownames(Data) <- DataQuan[,1]

Resp <- FA(Data, Method = "PC", Type = 2, NFactor = 3)

Tit = c("Scree-plot","Scores of the Observations","Factorial Loadings","Biplot")

Plot.FA(Resp, Titles = Tit, xlabel = NA, ylabel = NA,
        Color = TRUE, LinLab = rep("", nrow(Data)),
        Casc = TRUE)
}

\keyword{Factor Analysis}
\keyword{FA}
