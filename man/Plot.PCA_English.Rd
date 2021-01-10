\name{Plot.PCA}

\alias{Plot.PCA}

\title{Graphs of the Principal Components Analysis (PCA).}

\description{Graphs of the Principal Components Analysis (PCA).}

\usage{Plot.PCA(PC, titles = NA, xlabel = NA, ylabel = NA,
         size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
         savptc = FALSE, width = 3236, height = 2000, res = 300,
         casc = TRUE)}

\arguments{
  \item{PC}{Data of the PCA function.}
  \item{titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{linlab}{Vector with the labels for the observations.}
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

\seealso{\code{\link{PCA}}}

\examples{
data(DataQuan) # set of quantitative data

data <- DataQuan[,2:8]

rownames(data) <- DataQuan[1:nrow(DataQuan),1]

pc <- PCA(data, 2) # performs the PCA

tit <- c("Scree-plot","Graph of the Observations","Circle of Correlation")

Plot.PCA(PC = pc, titles = tit, xlabel = NA, ylabel = NA,
         color = TRUE, linlab = NA, savptc = FALSE, 
         width = 3236, height = 2000, res = 300,
         casc = TRUE)
}

\keyword{Principal Components Analysis}
\keyword{PCA}
