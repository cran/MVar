\name{Plot.CA}

\alias{Plot.CA}

\title{Graphs of the simple (CA) and multiple correspondence analysis (MCA).}

\description{Graphs of the simple (CA) and multiple correspondence analysis (MCA).}

\usage{Plot.CA(CA, Titles = NA, xlabel = NA, ylabel = NA,
        Color = TRUE, LinLab = NA, Casc = TRUE)}

\arguments{
\item{CA}{Data of the CA function.}
  \item{Titles}{Titles of the graphics, if not set, assumes the default text..}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{LinLab}{Vector with the labels for the observations, for frequency data, if not set, assumes the default text.}
  \item{Casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
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

Plot.CA(Resp, Titles = Tit, xlabel = NA, ylabel = NA,
        Color = TRUE, LinLab = NA, Casc = FALSE)


data(DataQuali) # qualitative data set

Data <- DataQuali[,2:ncol(DataQuali)]

Resp <- CA(Data, "c", "b") # performs CA

Tit = c("","","Graph of the variables")

Plot.CA(Resp, Titles = Tit, xlabel = NA, ylabel = NA,
        Color = TRUE, LinLab = NA, Casc = FALSE)

}

\keyword{Correspondence Analysis}
\keyword{CA}
