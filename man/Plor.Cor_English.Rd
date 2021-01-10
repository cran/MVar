\name{Plot.Cor}

\alias{Plot.Cor}

\title{Plot of correlations between variables.}

\description{It performs the correlations between the variables of a database and presents it in graph form.}

\usage{Plot.Cor(data, title = NA, grid = TRUE, leg = TRUE, boxleg = FALSE, 
         text = FALSE, arrow = TRUE, color = TRUE, namesvar = NA,
         savptc = FALSE, width = 3236, height = 2000, res = 300)}
          

\arguments{
\item{data}{Numeric data set.}
\item{title}{Title for the plot, if not defined it assumes standard text.}
\item{grid}{Puts grid on plot (default = TRUE).}
\item{leg}{Put the legend on the plot (default = TRUE)}
\item{boxleg}{Put frame in the legend (default = FALSE).}
\item{text}{Puts correlation values in circles (default = FALSE).}
\item{arrow}{Positive (up) and negative (down) correlation arrows (default = TRUE).}
\item{color}{Colorful plot (default = TRUE).}
\item{namesvar}{Vector with the variable names, if omitted it assumes the names in 'date'.}
\item{savptc}{Saves graphics images to files (default = FALSE).} 
\item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
\item{height}{Graphics images height when savptc = TRUE (default = 2000).}
\item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
}

\value{{Plot with the correlations between the variables in 'date'.}
}


\author{
Paulo Cesar Ossani
}

\examples{
data(iris) # data set

Plot.Cor(data = iris[,1:4], title = NA, grid = TRUE, leg = TRUE, boxleg = FALSE, 
         text = FALSE, arrow = TRUE, color = TRUE, namesvar = NA, savptc = FALSE, 
         width = 3236, height = 2000, res = 300)
         
Plot.Cor(data = iris[,1:4], title = NA, grid = TRUE, leg = TRUE, boxleg = FALSE, 
         text = TRUE, arrow = TRUE, color = TRUE, namesvar = c("A1","B2","C3","D4"),
         savptc = FALSE, width = 3236, height = 2000, res = 300)
         
}

\keyword{Plot.Cor}
