\name{GrandTour}

\alias{GrandTour}

\title{Animation technique Grand Tour.}

\description{Performs the exploration of the data through the technique of animation Grand Tour.}

\usage{GrandTour(data, method = "Interpolation", title = NA, xlabel = NA, 
          ylabel = NA, size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
          class = NA, classcolor = NA, posleg = 2, boxleg = TRUE,  
          axesvar = TRUE, axes = TRUE, numrot = 200, choicerot = NA, 
          savptc = FALSE, width = 3236, height = 2000, res = 300)}

\arguments{
\item{data    }{Numerical data set.}
\item{method  }{Method used for rotations:\cr
                "Interpolation" - Interpolation method (default),\cr
                "Torus" - Torus method,\cr
                "Pseudo" - Pseudo Grand Tour method.}
  \item{title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{linlab}{Vector with the labels for the observations.}
  \item{class}{Vector with names of data classes.}
  \item{classcolor}{Vector with the colors of the classes.}
  \item{posleg}{0 with no caption,\cr
                1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner (default),\cr
                3 for caption in the right lower corner,\cr
                4 for caption in the left lower corner.}
  \item{boxleg}{Puts the frame in the caption (default = TRUE).}
  \item{axesvar }{Puts axes of rotation of the variables (default = TRUE).}
  \item{axes}{Plots the X and Y axes (default = TRUE).}
  \item{numrot}{Number of rotations (default = 200). If method = "Interpolation", numrot represents the angle of rotation.}
  \item{choicerot}{Choose specific rotation and display on the screen, or save the image if savptc = TRUE.}
  \item{savptc}{Saves graphics images to files (default = FALSE).} 
  \item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
  \item{height}{Graphics images height when savptc = TRUE (default = 2000).}
  \item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
}

\value{{Graphs with rotations.}
\item{proj.data  }{Projected data.}
\item{vector.opt }{Vector projection.}
\item{method     }{method used on Grand Tour.}
}

\references{
Asimov, D. The Grand Tour: A Tool for Viewing Multidimensional data. \emph{SIAM Journal of Scientific and Statistical Computing},  6(1), 128-143, 1985.

Asimov, D.; Buja, A. The grand tour via geodesic interpolation of 2-frames. in Visual data Exploration and Analysis. \emph{Symposium on Electronic Imaging Science and Technology}, IS&T/SPIE. 1994.

Buja, A.; Asimov, D. Grand tour methods: An outline. \emph{Computer Science and Statistics}, 17:63-67. 1986.

Buja, A.; Cook, D.; Asimov, D.; Hurley, C. Computational methods for High-Dimensional Rotations in data Visualization, in C. R. Rao, E. J. Wegman & J. L. Solka, eds, \emph{"Handbook of Statistics: data Mining and Visualization"}, Elsevier/North Holland, http://www.elsevier.com, pp. 391-413. 2005.

Hurley, C.; Buja, A. Analyzing high-dimensional data with motion graphics, \emph{SIAM Journal of Scientific and Statistical Computing}, 11 (6), 1193-1211. 1990.

Martinez, W.  L.; Martinez, A. R.; Solka, J.; \emph{Exploratory data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

Young, F. W.; Rheingans P. Visualizing structure in high-dimensional multivariate data, \emph{IBM Journal of Research and Development}, 35:97-107, 1991.

Young, F. W.; Faldowski R. A.; McFarlane M. M. \emph{Multivariate statistical visualization, in Handbook of Statistics}, Vol 9, C. R. Rao (ed.), The Netherlands: Elsevier Science Publishers, 959-998, 1993.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # database

res <- GrandTour(iris[,1:4], method = "Torus", title = NA, xlabel = NA, ylabel = NA,
                 color = TRUE, linlab = NA, class = NA, posleg = 2, boxleg = TRUE, 
                 axesvar = TRUE, axes = FALSE, numrot = 10, choicerot = NA,
                 savptc = FALSE, width = 3236, height = 2000, res = 300)

print("Projected data:"); res$proj.data
print("Projection vectors:"); res$vector.opt
print("Grand Tour projection method:"); res$method

        
res <- GrandTour(iris[,1:4], method = "Interpolation", title = NA, xlabel = NA, ylabel = NA,
                 color = TRUE, linlab = NA, posleg = 2, boxleg = FALSE, axesvar = FALSE, 
                 axes = FALSE, numrot = 10, choicerot = NA, class = iris[,5],
                 classcolor = c("goldenrod3","gray53","red"),savptc = FALSE, 
                 width = 3236, height = 2000, res = 300)
         
print("Projected data:"); res$proj.data
print("Projection vectors:"); res$vector.opt
print("Grand Tour projection method:"); res$method

}

\keyword{Grand Tour}
