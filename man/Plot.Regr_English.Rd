\name{Plot.Regr}

\alias{Plot.Regr}

\title{Graphs of the linear regression results.}

\description{Graphs of the linear regression results.}

\usage{Plot.Regr(Reg, TypeGraf = "Scatterplot", Title = NA,
          xlabel = NA, ylabel = NA, NameVarY = NA, 
          NameVarX = NA, Color = TRUE, IntConf = TRUE, 
          IntPrev = TRUE, Casc = TRUE)}

\arguments{
\item{Reg}{Regression function data.}
\item{TypeGraf}{Type of graphic:\cr
                "Scatterplot" - Scatterplot 2 to 2,\cr
                "Regression"  - Graph of the linear regression,\cr
                "QQPlot"      - Graph of the normal probability of the residues,\cr
                "Histogram"   - Histogram of the residues,\cr
                "Fits"        - Graph of the adjusted values versus residuals,\cr
                "Order"       - Graph of the order of the observations versus the residuals.}
  \item{Title}{Titles of the graphics, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{NameVarY}{Variable name Y, if not set, assumes the default text.}
  \item{NameVarX}{Name of the variable, or variables X, if not set, assumes the default text.}
  \item{Color}{Colored graphics (default = TRUE).}
  \item{IntConf}{Case TypeGraf = "Regression". Graphics with confidence interval (default = TRUE).}
  \item{IntPrev}{Case TypeGraf = "Regression". Graphics with predictive interval (default = TRUE).}
  \item{Casc}{Cascade effect in the presentation of the graphics (default = TRUE).}
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

NomeY <- "Medium grade"

NomeX <- "Commercial coffees"

Resp <- Regr(Y, X, NameVarX = NomeX , Intercepts = TRUE, SigF = 0.05)

Tit <- c("Scatterplot")
Plot.Regr(Resp, TypeGraf = "Scatterplot", Title = Tit,
          NameVarY = NomeY, NameVarX = NomeX, Color = TRUE)

Tit <- c("Scatterplot with the adjusted line")
Plot.Regr(Resp, TypeGraf = "Regression", Title = Tit, 
          xlabel = NomeX, ylabel = NomeY, Color = TRUE,
          IntConf = TRUE, IntPrev = TRUE)

dev.new() # necessary to not overlap the following graphs to the previous graph

par(mfrow = c(2,2)) 

Plot.Regr(Resp, TypeGraf = "QQPlot", Casc = FALSE)
Plot.Regr(Resp, TypeGraf = "Histogram", Casc = FALSE)
Plot.Regr(Resp, TypeGraf = "Fits", Casc = FALSE)
Plot.Regr(Resp, TypeGraf = "Order", Casc = FALSE)
}

\keyword{Regression}
