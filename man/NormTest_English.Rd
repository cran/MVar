\name{NormTest}

\alias{NormTest}

\title{Test of normality of the data.}

\description{Check the normality of the data, based on the asymmetry coefficient test.}

\usage{NormTest(Data, Sign = 0.05)}

\arguments{
\item{Data}{Data to be analyzed.}
\item{Sign}{Test significance level (default 5\%).}
}

\value{
\item{Statistic  }{Observed Chi-square value, that is, the test statistic.}
\item{ChiQuadrado}{Chi-square value calculated.}
\item{GL         }{Degree of freedom.}
\item{p.Value    }{p-value.}
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
Data <- cbind(rnorm(100,2,3), rnorm(100,1,2))

NormTest(Data)

plot(density(Data))


Data <- cbind(rexp(200,3), rexp(200,3))

NormTest(Data, Sign = 0.01)

plot(density(Data))
}

\keyword{Test of normality of the data.}
\keyword{NormTest}
