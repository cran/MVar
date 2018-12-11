\name{Plot.CCA}

\alias{Plot.CCA}

\title{Graphs of the Canonical Correlation Analysis (CCA).}

\description{Graphs of the Canonical Correlation Analysis (CCA).}

\usage{Plot.CCA(CCA, Titles = NA, xlabel = NA, ylabel = NA,
         Color = TRUE, Casc = TRUE)}

\arguments{
  \item{CCA}{Data of the CCA function.}
  \item{Titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{Casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{Returns several graphs.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{CCA}}}

\examples{
data(DataMix) # database

Data <- DataMix[,2:ncol(DataMix)]

rownames(Data) <- DataMix[,1]

X <- as.data.frame(NormData(Data[,1:2],2))

Y <- as.data.frame(NormData(Data[,5:6],2))

Resp <- CCA(X, Y, Type = 1, Test = "Bartlett", Sign = 0.05) # performs CCA

Tit = c("Scree-plot","Correlations","Scores of the group X","Scores of the group Y")

Plot.CCA(Resp, Titles = Tit, xlabel = NA, ylabel = NA,
         Color = TRUE, Casc = TRUE)
}

\keyword{Analysis of canonical correlation}
\keyword{CCA}
