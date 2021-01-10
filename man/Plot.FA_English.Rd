\name{Plot.FA}

\alias{Plot.FA}

\title{Graphs of the Factorial Analysis (FA).}

\description{Graphs of the Factorial Analysis (FA).}

\usage{Plot.FA(FA, titles = NA, xlabel = NA, ylabel = NA,
        size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
        savptc = FALSE, width = 3236, height = 2000, res = 300, 
        casc = TRUE)}

\arguments{
  \item{FA}{Data of the FA function.}
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

\seealso{\code{\link{FA}}}

\examples{
data(DataQuan) # database

data <- DataQuan[,2:ncol(DataQuan)]

rownames(data) <- DataQuan[,1]

res <- FA(data, method = "PC", type = 2, nfactor = 3)

tit <- c("Scree-plot","Scores of the Observations","Factorial Loadings","Biplot")

Plot.FA(res, titles = tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = rep("", nrow(data)),
        savptc = FALSE, width = 3236, height = 2000, 
        res = 300, casc = TRUE)
}

\keyword{Factor Analysis}
\keyword{FA}
