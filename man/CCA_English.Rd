\name{CCA}

\alias{CCA}

\title{Canonical Correlation Analysis(CCA).}

\description{Perform Canonical Correlation Analysis (CCA) on a data set.}

\usage{CCA(X = NULL, Y = NULL, type = 1, test = "Bartlett", sign = 0.05)}

\arguments{
  \item{X}{First group of variables of a data set.}
  \item{Y}{Second group of variables of a data set.}
  \item{type}{1 for analysis using the covariance matrix (default),\cr
              2 for analysis using the correlation matrix.}
  \item{test}{Test of significance of the relationship between the group X and Y:\cr
              "Bartlett" (default) or "Rao".}
  \item{sign}{Test significance level (default 5\%).}
}

\value{
  \item{Cxx    }{Covariance matrix or correlation Cxx.}
  \item{Cyy    }{Covariance matrix or correlation Cyy.}
  \item{Cxy    }{Covariance matrix or correlation Cxy.}
  \item{Cyx    }{Covariance matrix or correlation Cyx.}
  \item{var.UV }{Matrix with eigenvalues (variances) of the canonical pairs U and V.}
  \item{corr.UV}{Matrix of the correlation of the canonical pairs U and V.}
  \item{coef.X }{Matrix of the canonical coefficients of the group X.}
  \item{coef.Y }{Matrix of the canonical coefficients of the group Y.}
  \item{corr.X }{Matrix of the correlations between canonical variables and the original variables of the group X.}
  \item{corr.Y }{Matrix of the correlations between the canonical variables and the original variables of the group Y.}
  \item{score.X}{Matrix with the scores of the group X.}
  \item{score.Y}{Matrix with the scores of the group Y.}
  \item{sigtest}{Returns the significance test of the relationship between group X and Y: "Bartlett" (default) or "Rao".}
}

\references{
Mingoti, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Ferreira, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

Lattin, J.; Carrol, J. D.; Green, P. E. \emph{Analise de dados multivariados.} 1th. ed. Sao Paulo: Cengage Learning, 2011. 455 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.CCA}}}

\examples{
data(DataMix) # data set

data <- DataMix[,2:ncol(DataMix)]

rownames(data) <- DataMix[,1]

X <- data[,1:2]

Y <- data[,5:6]

res <- CCA(X, Y, type = 2, test = "Bartlett", sign = 0.05)

print("Matrix with eigenvalues (variances) of the canonical pairs U and V:"); round(res$var.UV,3)

print("Matrix of the correlation of the canonical pairs U and V:"); round(res$corr.UV,3)

print("Matrix of the canonical coefficients of the group X:"); round(res$coef.X,3)

print("Matrix of the canonical coefficients of the group Y:"); round(res$coef.Y,3)

print("Matrix of the correlations between the canonical 
       variables and the original variables of the group X:"); round(res$corr.X,3)

print("Matrix of the correlations between the canonical 
       variables and the original variables of the group Y:"); round(res$corr.Y,3)

print("Matrix with the scores of the group X:"); round(res$score.X,3)

print("Matrix with the scores of the group Y:"); round(res$score.Y,3)

print("test of significance of the canonical pairs:"); res$sigtest
}

\keyword{Analysis of canonical correlation}
\keyword{CCA}
