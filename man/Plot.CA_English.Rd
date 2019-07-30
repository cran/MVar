\name{Plot.CA}

\alias{Plot.CA}

\title{Graphs of the simple (CA) and multiple correspondence analysis (MCA).}

\description{Graphs of the simple (CA) and multiple correspondence analysis (MCA).}

\usage{Plot.CA(CA, titles = NA, xlabel = NA, ylabel = NA,
        size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
        casc = TRUE)}

\arguments{
\item{CA}{Data of the CA function.}
  \item{titles}{Titles of the graphics, if not set, assumes the default text..}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{linlab}{Vector with the labels for the observations.}
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

Data <- DataFreq[,2:ncol(DataFreq)]

rownames(Data) <- DataFreq[1:nrow(DataFreq),1]

Resp <- CA(Data, "f") # performs CA

Tit = c("Scree-plot","Observations", "Variables", "Observations / Variables")

Plot.CA(Resp, titles = Tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = rownames(Data), casc = FALSE)


data(DataQuali) # qualitative data set

Data <- DataQuali[,2:ncol(DataQuali)]

Resp <- CA(Data, "c", "b") # performs CA

Tit = c("","","Graph of the variables")

Plot.CA(Resp, titles = Tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = NA, casc = FALSE)

}

\keyword{Correspondence Analysis}
\keyword{CA}
