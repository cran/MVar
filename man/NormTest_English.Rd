\name{NormTest}

\alias{NormTest}

\title{Test of normality of the data.}

\description{Check the normality of the data, based on the asymmetry coefficient test.}

\usage{NormTest(data, sign = 0.05)}

\arguments{
\item{data}{Data to be analyzed.}
\item{sign}{Test significance level (default 5\%).}
}

\value{
\item{statistic}{Observed Chi-square value, that is, the test statistic.}
\item{chisquare}{Chi-square value calculated.}
\item{gl       }{Degree of freedom.}
\item{p.value  }{p-value.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data <- cbind(rnorm(100,2,3), rnorm(100,1,2))

NormTest(data)

plot(density(data))


data <- cbind(rexp(200,3), rexp(200,3))

NormTest(data, sign = 0.01)

plot(density(data))
}

\keyword{Test of normality of the data.}
\keyword{NormTest}
