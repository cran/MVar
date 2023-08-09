\name{Plot.Regr}

\alias{Plot.Regr}

\title{Graphs of the linear regression results.}

\description{Graphs of the linear regression results.}

\usage{Plot.Regr(Reg, typegraf = "Scatterplot", title = NA, xlabel = NA, 
          ylabel = NA, namevary = NA, namevarx = NA, size = 1.1, 
          grid = TRUE, color = TRUE, intconf = TRUE, intprev = TRUE,
          savptc = FALSE, width = 3236, height = 2000, res = 300, 
          casc = TRUE)}

\arguments{
\item{Reg}{Regression function data.}
\item{typegraf}{Type of graphic:\cr
                "Scatterplot" - Scatterplot 2 to 2,\cr
                "Regression"  - Graph of the linear regression,\cr
                "QQPlot"      - Graph of the normal probability of the residues,\cr
                "Histogram"   - Histogram of the residues,\cr
                "Fits"        - Graph of the adjusted values versus residuals,\cr
                "Order"       - Graph of the order of the observations versus the residuals.}
  \item{title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{namevary}{Variable name Y, if not set, assumes the default text.}
  \item{namevarx}{Name of the variable X, or variables X, if not set, assumes the default text.}
  \item{size}{Size of the points in the graphs.}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{intconf}{Case typegraf = "Regression". Graphics with confidence interval (default = TRUE).}
  \item{intprev}{Case typegraf = "Regression". Graphics with predictive interval (default = TRUE).}
  \item{savptc}{Saves graphics images to files (default = FALSE).} 
  \item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
  \item{height}{Graphics images height when savptc = TRUE (default = 2000).}
  \item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
  \item{casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
}

\value{Returns several graphs.}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Regr}}}

\examples{
data(DataMix)

Y <- DataMix[,2]

X <- DataMix[,7]

name.y <- "Medium grade"

name.x <- "Commercial coffees"

res <- Regr(Y, X, namevarx = name.x , intercept = TRUE, sigf = 0.05)

tit <- c("Scatterplot")
Plot.Regr(res, typegraf = "Scatterplot", title = tit,
          namevary = name.y, namevarx = name.x, color = TRUE, 
          savptc = FALSE, width = 3236, height = 2000, res = 300)

tit <- c("Scatterplot with the adjusted line")
Plot.Regr(res, typegraf = "Regression", title = tit, 
          xlabel = name.x, ylabel = name.y, color = TRUE,
          intconf = TRUE, intprev = TRUE, savptc = FALSE, 
          width = 3236, height = 2000, res = 300)

dev.new() # necessary to not overlap the following graphs to the previous graph

par(mfrow = c(2,2)) 

Plot.Regr(res, typegraf = "QQPlot", casc = FALSE)
Plot.Regr(res, typegraf = "Histogram", casc = FALSE)
Plot.Regr(res, typegraf = "Fits", casc = FALSE)
Plot.Regr(res, typegraf = "Order", casc = FALSE)
}

\keyword{Regression}
