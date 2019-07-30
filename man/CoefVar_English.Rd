\name{CoefVar}

\alias{CoefVar}

\title{Coefficient of variation of the data.}

\description{Find the coefficient of variation of the data, either overall or per column.}

\usage{CoefVar(data, type = 1)}

\arguments{
\item{data}{Data to be analyzed.}
\item{type}{1 Coefficient of overall variation (default),\cr
            2 Coefficient of variation per column.}
}

\value{{Coefficient of variation, either overall or per column.}}

\references{
FERREIRA, D. F.; \emph{Estatistica Basica.} 2 ed. rev. Lavras: UFLA, 2009. 664 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # data set

data <- DataQuan[,2:8]

Resp <- CoefVar(data, type = 1) # Coefficient of overall variation
round(Resp,2)

Resp <- CoefVar(data, type = 2) # Coefficient of variation per column
round(Resp,2)
}

\keyword{Coefficient of variation}
\keyword{CoefVar}
