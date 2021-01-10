\name{Plot.CA}

\alias{Plot.CA}

\title{Graphs of the simple (CA) and multiple correspondence analysis (MCA).}

\description{Graphs of the simple (CA) and multiple correspondence analysis (MCA).}

\usage{Plot.CA(CA, titles = NA, xlabel = NA, ylabel = NA,
        size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
        savptc = FALSE, width = 3236, height = 2000, 
        res = 300, casc = TRUE)}

\arguments{
\item{CA}{Data of the CA function.}
  \item{titles}{Titles of the graphics, if not set, assumes the default text..}
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

\seealso{\code{\link{CA}}}

\examples{
data(DataFreq) # frequency data set

data <- DataFreq[,2:ncol(DataFreq)]

rownames(data) <- DataFreq[1:nrow(DataFreq),1]

res <- CA(data, "f") # performs CA

tit <- c("Scree-plot","Observations", "Variables", "Observations / Variables")

Plot.CA(res, titles = tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = rownames(data), savptc = FALSE,
        width = 3236, height = 2000, res = 300, casc = FALSE)


data(DataQuali) # qualitative data set

data <- DataQuali[,2:ncol(DataQuali)]

res <- CA(data, "c", "b") # performs CA

tit <- c("","","Graph of the variables")

Plot.CA(res, titles = tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = NA, savptc = FALSE, 
        width = 3236, height = 2000, res = 300,
        casc = FALSE)

}

\keyword{Correspondence Analysis}
\keyword{CA}
