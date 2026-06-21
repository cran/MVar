\name{CI}

\alias{CI}

\title{Confidence Intervals: Univariate and Multivariate.}

\description{Computes univariate and multivariate confidence intervals for variables in a dataset.}

\usage{CI(data, sigma = NA, conf = 0.95, type = "T")}

\arguments{
  \item{data}{Data used to compute the confidence intervals.}
  \item{sigma}{Variance–covariance matrix; otherwise the CI will be based on the sample variance (default sigma = NA).}
  \item{conf}{Confidence level of the CI (default conf = 95\%).}
  \item{type}{Type of interval: 'T' for Hotelling’s \(T^2\) or 'B' for Bonferroni.}
}

\value{
  \item{CIu}{Univariate confidence interval with confidence level 'conf'.}
  \item{CIm}{Multivariate confidence interval with confidence level 'conf'.}
}

\references{
Ferreira, D. F. \emph{EstatistCIa Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.. 708 p.
}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Plot.CI}}}

\examples{
data(iris) # data set

# Interval with unknown population variance
res <- CI(data = iris[,1:4], sigma = NA, conf = 0.95, type = 'T') 
# res <- CI(data = iris[,1:4], sigma = NA, conf = 0.95, type = 'B') 

res$ciu # Univariate interval
res$cim # Multivariate interval


# Interval with known population variance
sig <- matrix(c(5.286, 0.942, 1.274, 4.516, # Example of known variance matrix
                0.942, 3.690, 0.730, 1.122,
                1.274, 0.730, 7.116, 1.296,
                4.516, 1.122, 1.296, 4.581), 
                nrow = 4, byrow = TRUE)
           
# res <- CI(data = iris[,1:4], sigma = sig, conf = 0.95, type = 'T')
res <- CI(data = iris[,1:4], sigma = sig, conf = 0.95, type = 'B')

res$ciu # Univariate interval
res$cim # Multivariate interval
}

\keyword{Confidence Intervals}
\keyword{CI}
