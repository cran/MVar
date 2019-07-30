\name{IM}

\alias{IM}

\title{Indicator matrix.}

\description{In the indicator matrix the elements are arranged in the form of \emph{dummy} variables, in other words, 1 for a category chosen as a response variable and 0 for the other categories of the same variable.}

\usage{IM(data, names = TRUE)}

\arguments{
\item{data}{Categorical data.}
\item{names}{Include the names of the variables in the levels of the Indicator Matrix (default = TRUE).}
}

\value{\item{mtxIndc}{Returns converted data in the indicator matrix.}}

\references{
RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data <- matrix(c("S","S","N","N",1,2,3,4,"N","S","T","N"), nrow = 4, ncol = 3)

IM(data, names = FALSE)

data(DataQuali) # qualitative data set

IM(DataQuali, names = TRUE)
}

\keyword{Dummy variables}
\keyword{Indicator matrix}
