\name{Scatter}

\alias{Scatter}

\title{Scatter plot.}

\description{Performs the scatter plot.}

\usage{Scatter(data, ellipse = TRUE, ellipse.level = 0.95, rectangle = FALSE,  
        title = NA, xlabel = NA, ylabel = NA,  posleg = 2, boxleg = TRUE, 
        axes = TRUE, size = 1.1, grid = TRUE, color = TRUE, linlab = NA,   
        class = NA, classcolor = NA, savptc = FALSE, width = 3236, 
        height = 2000, res = 300)}
          


\arguments{
\item{data   }{Data with x and y coordinates.}
\item{ellipse}{Place an ellipse around the classes (default = TRUE).}
\item{ellipse.level}{Significance level of the ellipse (defaul = 0.95).}
\item{rectangle}{Place rectangle to differentiate classes (default = FALSE).}
\item{title}{Titles of the graphics, if not set, assumes the default text.}
\item{xlabel}{Names the X axis, if not set, assumes the default text.}
\item{ylabel}{Names the Y axis, if not set, assumes the default text.}
\item{posleg}{0 with no caption,\cr
              1 for caption in the left upper corner,\cr
              2 for caption in the right upper corner (default),\cr
              3 for caption in the right lower corner,\cr
              4 for caption in the left lower corner.}
\item{boxleg}{Puts the frame in the caption (default = TRUE).} 
\item{axes}{Plots the X and Y axes (default = TRUE).}
\item{size}{Size of the points in the graphs.}
\item{grid}{Put grid on graphs (default = TRUE).}
\item{color}{Colored graphics (default = TRUE).}
\item{linlab}{Vector with the labels for the observations.}
\item{class}{Vector with names of data classes.}
\item{classcolor}{Vector with the colors of the classes.}
\item{savptc}{Saves graphics images to files (default = FALSE).} 
\item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
\item{height}{Graphics images height when savptc = TRUE (default = 2000).}
\item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
}

\value{{Scatter plot.}}

\references{
Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

Anton, H.; Rorres, C. \emph{Elementary linear algebra: applications version.} 10th ed. New Jersey: John Wiley & Sons, 2010. 768 p.
}

\author{
Paulo Cesar Ossani
}

\examples{
data(iris) # data set

data <- iris[,3:4]

cls <- iris[,5] # data class

Scatter(data, ellipse = TRUE, ellipse.level = 0.95, rectangle = FALSE,  
        title = NA, xlabel = NA, ylabel = NA,  posleg = 1, boxleg = FALSE, 
        axes = FALSE, size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
        class = cls, classcolor = c("goldenrod3","blue","red"),
        savptc = FALSE, width = 3236, height = 2000, res = 300)

Scatter(data, ellipse = FALSE, ellipse.level = 0.95, rectangle = TRUE,  
        title = NA, xlabel = NA, ylabel = NA,  posleg = 1, boxleg = TRUE, 
        axes = FALSE, size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
        class = cls, classcolor = c("goldenrod3","blue","red"),
        savptc = FALSE, width = 3236, height = 2000, res = 300)
}

\keyword{Scatter Plot}
