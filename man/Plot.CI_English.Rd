\name{Plot.CI}

\alias{Plot.CI}

\title{Graph of Univariate and Multivariate Confidence Intervals.}

\description{Graph of Univariate and Multivariate Confidence Intervals.}

\usage{Plot.CI(ci, title = NA, xlabel = NA, ylabel = NA, size = 0.9, 
        grid = TRUE, color = TRUE, savptc = FALSE, width = 3236,
        height = 2000, res = 300, casc = FALSE)}

\arguments{
\item{ci}{Data from the CI function, or with the same structure.}
\item{title}{Plot title; if not specified, default text is used.}
\item{xlabel}{X-axis label; if not specified, default text is used.}
\item{ylabel}{Y-axis label; if not specified, default text is used.}
\item{size}{Point size in the plot.}
\item{grid}{Adds a grid to the plot (default = TRUE).}
\item{color}{Colored plot (default = TRUE).}
\item{savptc}{Saves graphics images to files (default = FALSE).} 
\item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
\item{height}{Graphics images height when savptc = TRUE (default = 2000).}
\item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
\item{casc}{Cascade effect in the presentation of the graphics (default = FALSE).}
}

\value{Returns plot.}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{CI}}}

\examples{
data(iris) # data set

res <- CI(data = iris[,1:4], sigma = NA, conf = 0.95, type = "T")
        
tit <- "Univariate Confidence Interval"

Plot.CI(ci = res$ciu$CIu, title = tit, xlabel = NA, ylabel = '', 
        size = 1, grid = TRUE, color = TRUE, savptc = FALSE,
        width = 3236, height = 2000, res = 300, casc = FALSE) 
        
        
tit <- "Multivariate Confidence Interval"

Plot.CI(ci = res$cim$CIm, title = tit, xlabel = NA, ylabel = '', 
        size = 1, grid = TRUE, color = TRUE, savptc = FALSE,
        width = 3236, height = 2000, res = 300, casc = FALSE) 
      
}

\keyword{Confidence Intervals}
\keyword{CI}
