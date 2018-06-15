\name{IM}

\alias{IM}

\title{Indicator matrix.}

\description{In the indicator matrix the elements are arranged in the form of \emph{dummy} variables, in other words, 1 for a category chosen as a response variable and 0 for the other categories of the same variable.}

\usage{IM(Data, Names = TRUE)}

\arguments{
\item{Data}{Categorical data.}
\item{Names}{Include the names of the variables in the levels of the Indicator Matrix (default = TRUE).}
}

\value{\item{Dados}{Returns converted data in the indicator matrix.}}

\references{RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
Date = matrix(c("S","S","N","N",1,2,3,4,"N","S","T","N"), nrow = 4, ncol = 3)

IM(Date, Names = FALSE)

data(DataQuali) # qualitative data set

IM(DataQuali, Names = TRUE)
}

\keyword{Dummy variables}
\keyword{Indicator matrix}
