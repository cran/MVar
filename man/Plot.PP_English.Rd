\name{Plot.PP}

\alias{Plot.PP}

\title{Graphics of the Projection Pursuit (PP).}

\description{Graphics of the Projection Pursuit (PP).}

\usage{Plot.PP(PP, Titles = NA, xlabel = NA, ylabel = NA,
        PosLeg = 2, BoxLeg = TRUE, Color = TRUE, Label = FALSE,
        LinLab = NA, AxisVar = TRUE, Axis = TRUE, Casc = TRUE)}
 
\arguments{
  \item{PP}{Data of the PP_Optimizer function.}
  \item{Titles}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{PosLeg}{0 with no caption,\cr
                1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner (default),\cr
                3 for caption in the right lower corner,\cr
                4 for caption in the left lower corner.}
  \item{BoxLeg}{Puts the frame in the caption (default = TRUE).}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{Label}{Puts the labels on observations (default = FALSE).}
  \item{LinLab}{Vector with the labels for the observations, if not set, assumes the default text.}
  \item{AxisVar}{Puts axes of rotation of the variables, only when DimProj > 1 (default = TRUE).}
  \item{Axis}{Plots the X and Y axes (default = TRUE).}
  \item{Casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{Graph of the evolution of the indices, and graphs whose data were reduced in two dimensions.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PP_Optimizer}} and \code{\link{PP_Index}}}

\examples{
data(iris) # dataset

# Example 1 - Without the classes in the data
Data <- iris[,1:4]

FcIndex <- "kurtosismax" # index function

Dim <- 1 # dimension of data projection

Sphere <- TRUE # spherical data

Res <- PP_Optimizer(Data = Data, Class = NA, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 500, Half = 30)
    
Plot.PP(Res, Titles = NA, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = NA, AxisVar = TRUE, Axis = TRUE,
        Casc = FALSE)


# Example 2 - With the classes in the data
Class <- iris[,5] # data class

Res <- PP_Optimizer(Data = Data, Class = Class, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 500, Half = 30)

Tit <- c(NA,"Graph example") # titles for the graphics

Plot.PP(Res, Titles = Tit, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = Class, AxisVar = TRUE, Axis = TRUE,
        Casc = FALSE)
        

# Example 3 - Without the classes in the data, but informing 
#             the classes in the plot function
Res <- PP_Optimizer(Data = Data, Class = NA, Findex = "Moment",
                    OptMethod = "GTSA", DimProj = 2, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 500, Half = 30)

Class <- c(rep("a",50),rep("b",50),rep("c",50)) # data class

Plot.PP(Res, Titles = NA, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = Class, AxisVar = TRUE, Axis = TRUE,
        Casc = FALSE)
        

# Example 4 - With the classes in the data, but not informed in plot function
Class <- iris[,5] # data class

Dim <- 2 # dimension of data projection

FcIndex <- "lda" # index function

Res <- PP_Optimizer(Data = Data, Class = Class, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 500, Half = 30)

Tit <- c("",NA) # titles for the graphics

Plot.PP(Res, Titles = Tit, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = NA, AxisVar = TRUE, Axis = TRUE,
        Casc = FALSE)
}

\keyword{Projection pursuit}
\keyword{PP}
