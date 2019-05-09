\name{GrandTour}

\alias{GrandTour}

\title{Animation technique Grand Tour.}

\description{Performs the exploration of the data through the technique of animation Grand Tour.}

\usage{GrandTour(Data, Method = "Interpolation", Title = NA, xlabel = NA, ylabel = NA,
          Color = TRUE, LinLab = NA, Class = NA, PosLeg = 2, BoxLeg = TRUE, 
          AxisVar = TRUE, Axis = FALSE, NumRot = 200, ChoiceRot = NA,
          SavePicture = FALSE)}

\arguments{
\item{Data    }{Numerical data set.}
\item{Method  }{Method used for rotations:\cr
                "Interpolation" - Interpolation method (default),\cr
                "Torus" - Torus method,\cr
                "Pseudo" - Pseudo Grand Tour method.}
  \item{Title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{LinLab}{Vector with the labels for the observations.}
  \item{Class}{Vector with names of data classes.}
  \item{PosLeg}{0 with no caption,\cr
                1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner (default),\cr
                3 for caption in the right lower corner,\cr
                4 for caption in the left lower corner.}
  \item{BoxLeg}{Puts the frame in the caption (default = TRUE).}
  \item{AxisVar }{Puts axes of rotation of the variables (default = TRUE).}
  \item{Axis}{Plots the X and Y axes (default = TRUE).}
  \item{NumRot}{Number of rotations (default = 200). If Method = "Interpolation", NumRot represents the angle of rotation.}
  \item{ChoiceRot}{Choose specific rotation and display on the screen, or save the image if SavePicture = TRUE.}
  \item{SavePicture}{Saves graphics images to files (default = FALSE).}          
}

\value{{Graphs with rotations.}
\item{Proj.Data  }{Projected data.}
\item{Vector.Opt }{Vector projection.}
\item{Method     }{Method used on Grand Tour.}
}

\references{
ASIMOV, D. The Grand Tour: A Tool for Viewing Multidimensional Data. \emph{SIAM Journal of Scientific and Statistical Computing},  6(1), 128-143, 1985.

ASIMOV, D.; BUJA, A. The grand tour via geodesic interpolation of 2-frames. in Visual Data Exploration and Analysis. \emph{Symposium on Electronic Imaging Science and Technology}, IS&T/SPIE. 1994.

BUJA, A. ; ASIMOV, D. Grand tour methods: An outline. \emph{Computer Science and Statistics}, 17:63-67. 1986.

BUJA, A.; COOK, D.; ASIMOV, D.; HURLEY, C. Computational Methods for High-Dimensional Rotations in Data Visualization, in C. R. Rao, E. J. Wegman & J. L. Solka, eds, \emph{"Handbook of Statistics: Data Mining and Visualization"}, Elsevier/North Holland, http://www.elsevier.com, pp. 391-413. 2005.

HURLEY, C.; BUJA, A. Analyzing high-dimensional data with motion graphics, \emph{SIAM Journal of Scientific and Statistical Computing}, 11 (6), 1193-1211. 1990.

MARTINEZ, W.  L., MARTINEZ, A. R., SOLKA, J.; \emph{Exploratory Data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

YOUNG, F. W.; RHEINGANS P.  Visualizing structure in high-dimensional multivariate data, \emph{IBM Journal of Research and Development}, 35:97-107, 1991.

YOUNG, F. W.; FALDOWSKI R. A.; McFARLANE M. M. \emph{Multivariate statistical visualization, in Handbook of Statistics}, Vol 9, C. R. Rao (ed.), The Netherlands: Elsevier Science Publishers, 959-998, 1993.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # database

Res <- GrandTour(iris[,1:4], Method = "Torus", Title = NA, xlabel = NA, ylabel = NA,
                 Color = TRUE, LinLab = NA, Class = NA, PosLeg = 2, BoxLeg = TRUE, 
                 AxisVar = TRUE, Axis = FALSE, NumRot = 10, ChoiceRot = NA,
                 SavePicture = FALSE)

print("Projected data:"); Res$Proj.Data
print("Projection vectors:"); Res$Vector.Opt
print("Grand Tour projection method:"); Res$Method

        
Res <- GrandTour(iris[,1:4], Method = "Interpolation", Title = NA, xlabel = NA, ylabel = NA,
                 Color = TRUE, LinLab = NA, Class = iris[,5], PosLeg = 2, BoxLeg = FALSE, 
                 AxisVar = FALSE, Axis = FALSE, NumRot = 10, ChoiceRot = NA,
                 SavePicture = FALSE)
          
print("Projected data:"); Res$Proj.Data
print("Projection vectors:"); Res$Vector.Opt
print("Grand Tour projection method:"); Res$Method

}

\keyword{Grand Tour}
