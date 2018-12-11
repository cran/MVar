\name{Plot.MFA}

\alias{Plot.MFA}

\title{Graphics of the Multiple Factor Analysis (MFA).}

\description{Graphics of the Multiple Factor Analysis  (MFA).}

\usage{Plot.MFA(MFA, Titles = NA, xlabel = NA, ylabel = NA,
         PosLeg = 2, BoxLeg = TRUE, Color = TRUE, 
         NamArr = FALSE, LinLab = NA, Casc = TRUE)}
 
\arguments{
  \item{MFA}{Data of the MFA function.}
  \item{Titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{PosLeg}{1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner (default),\cr
                3 for caption in the right lower corner,\cr
                4 for caption in the left lower corner.}
  \item{BoxLeg}{Puts frame in legend (default = TRUE).}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{NamArr}{Puts the points names in the cloud around the centroid in the graph corresponding to the global analysis of the Individuals and Variables (default = FALSE).}
  \item{LinLab}{Vector with the labels for the observations, if not set, assumes the default text.}
  \item{Casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{Returns several graphs.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{MFA}}}

\examples{
data(DataMix) # set of mixed data

Data <- DataMix[,2:ncol(DataMix)] 

rownames(Data) <- DataMix[1:nrow(DataMix),1]

GroupNames = c("Grade Cafes/Work", "Formation/Dedication", "Coffees")
           
MF <- MFA(Data, c(2,2,2), TypeGroups = c("n","c","f"), GroupNames) # performs MFA

Tit = c("Scree-Plot","Observations","Observations/Variables","Inertia of the Variable Groups")

Plot.MFA(MF, Titles = Tit, xlabel = NA, ylabel = NA,
         PosLeg = 2, BoxLeg = FALSE, Color = TRUE, 
         NamArr = FALSE, LinLab = NA, 
         Casc = FALSE) # plotting several graphs on the screen

Plot.MFA(MF, Titles = Tit, xlabel = NA, ylabel = NA,
         PosLeg = 2, BoxLeg = FALSE, Color = TRUE, 
         NamArr = FALSE, LinLab = rep("A?",10), 
         Casc = FALSE) # plotting several graphs on the screen
}

\keyword{Multiple Factor Analysis}
\keyword{MFA}
\keyword{MFACT}
