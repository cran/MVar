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

res  <- NormData(data, type = 1) # normalizes the data globally

res # Globally standardized data

sd(res)   # overall standard deviation

mean(res) # overall mean


res <- NormData(data, type = 2) # normalizes the data per column

res # standardized data per column

apply(res, 2, sd) # standard deviation per column

colMeans(res)     # column averages
}

\keyword{Normalizes the data.}

