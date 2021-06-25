\name{Plot.PCA}

\alias{Plot.PCA}

\title{Graphs of the Principal Components Analysis (PCA).}

\description{Graphs of the Principal Components Analysis (PCA).}

\usage{Plot.PCA(PC, titles = NA, xlabel = NA, ylabel = NA, size = 1.1, 
         grid = TRUE, color = TRUE, linlab = NA, axes = TRUE, class = NA, 
         classcolor = NA, posleg = 2, boxleg = TRUE, savptc = FALSE,
         width = 3236, height = 2000, res = 300, casc = TRUE)}

\arguments{
\item{PC}{Data of the PCA function.}
\item{titles}{Titles of the graphics, if not set, assumes the default text.}
\item{xlabel}{Names the X axis, if not set, assumes the default text.}
\item{ylabel}{Names the Y axis, if not set, assumes the default text.}
\item{size}{Size of the points in the graphs.}
\item{grid}{Put grid on graphs (default = TRUE).}
\item{color}{Colored graphics (default = TRUE).}
\item{linlab}{Vector with the labels for the observations.}
\item{axes}{Plots the X and Y axes (default = TRUE).}
\item{class}{Vector with names of data classes.}
\item{classcolor}{Vector with the colors of the classes.}
\item{posleg}{0 with no caption,\cr
              1 for caption in the left upper corner,\cr
              2 for caption in the right upper corner (default),\cr
              3 for caption in the right lower corner,\cr
              4 for caption in the left lower corner.}
\item{boxleg}{Puts the frame in the caption (default = TRUE).} 
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
data(iris) # data set

data <- iris[,1:4]

cls <- iris[,5] # data class

pc <- PCA(data, 2)

tit <- c("Scree-plot","Observations","Correlations")

cls <- as.character(iris[,5])

Plot.PCA(PC = pc, titles = tit, xlabel = NA, ylabel = NA,
         color = TRUE, linlab = NA, savptc = FALSE, size = 1.1,
         posleg = 2, boxleg = FALSE, class = cls, axes = TRUE,
         classcolor = c("blue3","red","goldenrod3"),
         width = 3236, height = 2000, res = 300, casc = FALSE)
         
}

\keyword{Principal Components Analysis}
\keyword{PCA}
