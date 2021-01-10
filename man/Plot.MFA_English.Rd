\name{Plot.MFA}

\alias{Plot.MFA}

\title{Graphics of the Multiple Factor Analysis (MFA).}

\description{Graphics of the Multiple Factor Analysis  (MFA).}

\usage{Plot.MFA(MFA, titles = NA, xlabel = NA, ylabel = NA,
         posleg = 2, boxleg = TRUE, size = 1.1, grid = TRUE, 
         color = TRUE, groupscolor = NA, namarr = FALSE, 
         linlab = NA, savptc = FALSE, width = 3236, 
         height = 2000, res = 300, casc = TRUE)}
 
\arguments{
  \item{MFA}{Data of the MFA function.}
  \item{titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{posleg}{1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner (default),\cr
                3 for caption in the right lower corner,\cr
                4 for caption in the left lower corner.}
  \item{boxleg}{Puts frame in legend (default = TRUE).}
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{groupscolor}{Vector with the colors of the groups.}
  \item{namarr}{Puts the points names in the cloud around the centroid in the graph corresponding to the global analysis of the Individuals and Variables (default = FALSE).}
  \item{linlab}{Vector with the labels for the observations, if not set, assumes the default text.}
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

\seealso{\code{\link{MFA}}}

\examples{
data(DataMix) # set of mixed data

data <- DataMix[,2:ncol(DataMix)] 

rownames(data) <- DataMix[1:nrow(DataMix),1]

group.names = c("Grade Cafes/Work", "Formation/Dedication", "Coffees")
           
mf <- MFA(data, c(2,2,2), typegroups = c("n","c","f"), group.names) # performs MFA

tit <- c("Scree-Plot","Observations","Observations/Variables",
         "Correlation Circle","Inertia of the Variable Groups")

Plot.MFA(MFA = mf, titles = tit, xlabel = NA, ylabel = NA,
         posleg = 2, boxleg = FALSE, color = TRUE, 
         groupscolor = c("blue3","red","goldenrod3"),
         namarr = FALSE, linlab = NA, savptc = FALSE, 
         width = 3236, height = 2000, res = 300, 
         casc = TRUE) # plotting several graphs on the screen

Plot.MFA(MFA = mf, titles = tit, xlabel = NA, ylabel = NA,
         posleg = 2, boxleg = FALSE, color = TRUE, 
         namarr = FALSE, linlab = rep("A?",10), 
         savptc = FALSE, width = 3236, height = 2000,
         res = 300, casc = TRUE) # plotting several graphs on the screen
}

\keyword{Multiple Factor Analysis}
\keyword{MFA}
\keyword{MFACT}
