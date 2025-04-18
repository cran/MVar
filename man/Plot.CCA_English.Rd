\name{Plot.CCA}

\alias{Plot.CCA}

\title{Graphs of the Canonical Correlation Analysis (CCA).}

\description{Graphs of the Canonical Correlation Analysis (CCA).}

\usage{Plot.CCA(CCA, titles = NA, xlabel = NA, ylabel = NA, size = 1.1, 
         grid = TRUE, color = TRUE, axes = TRUE, savptc = FALSE, 
         width = 3236, height = 2000, res = 300, casc = TRUE)}

\arguments{
  \item{CCA}{Data of the CCA function.}
  \item{titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{axes}{Plots the X and Y axes (default = TRUE).}
  \item{savptc}{Saves graphics images to files (default = FALSE).} 
  \item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
  \item{height}{Graphics images height when savptc = TRUE (default = 2000).}
  \item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
  \item{casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{Returns several graphs.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{CCA}}}

\examples{
data(DataMix) # database

data <- DataMix[,2:ncol(DataMix)]

rownames(data) <- DataMix[,1]

X <- data[,1:2]

Y <- data[,5:6]

res <- CCA(X, Y, type = 2, test = "Bartlett", sign = 0.05) # performs CCA

tit <- c("Scree-plot","Correlations","Scores of the group X","Scores of the group Y")

Plot.CCA(res, titles = tit, xlabel = NA, ylabel = NA,
         color = TRUE, savptc = FALSE, width = 3236, 
         height = 2000, res = 300, axes = TRUE,
         casc = FALSE)
}

\keyword{Analysis of canonical correlation}
\keyword{CCA}
