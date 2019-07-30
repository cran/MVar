\name{NormData}

\alias{NormData}

\title{Normalizes the data.}

\description{Function that normalizes the data globally, or by column.}

\usage{NormData(data, type = 1)}
\arguments{
  \item{data}{Data to be analyzed.}
  \item{type}{1 normalizes overall (default),\cr
              2 normalizes per column.}
}

\value{\item{dataNorm}{Normalized data.}}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # set of quantitative data

data <- DataQuan[,2:8]

Resp = NormData(data, type = 1) # normalizes the data globally

Resp # Globally standardized data

sd(Resp)   # overall standard deviation

mean(Resp) # overall mean


Resp = NormData(data, type = 2) # normalizes the data per column

Resp # standardized data per column

apply(Resp, 2, sd) # standard deviation per column

colMeans(Resp)     # column averages
}

\keyword{Normalizes the data.}

