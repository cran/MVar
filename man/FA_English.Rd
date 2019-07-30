\name{FA}

\alias{FA}

\title{Factor Analysis (FA).}

\description{Performs factorial analysis (FA) in a data set.}

\usage{FA(data, method = "PC", type = 2, nfactor = 1, rotation = "None",
   scoresobs = "Bartlett", converg = 1e-5, iteracao = 1000, 
   testfit = TRUE) 
}

\arguments{
\item{data}{Data to be analyzed.}
\item{method}{Method of analysis:\cr
              "PC" - Principal Components (default),\cr
              "PF" - Principal Factor,\cr
              "ML" - Maximum Likelihood.}
\item{type}{1 for analysis using the covariance matrix,\cr
            2 for analysis using the correlation matrix (default).}
\item{rotation}{Type of rotation: "None" (default) and "Varimax".}
\item{nfactor}{Number of factors (default = 1).}
\item{scoresobs}{Type of scores for the observations: "Bartlett" (default) or "Regression".}
\item{converg}{Limit value for convergence to sum of the squares of the residuals for Maximum likelihood method (default = 1e-5).}
\item{iteracao}{Maximum number of iterations for Maximum Likelihood method (default = 1000).}
\item{testfit}{Tests the model fit to the method of Maximum Likelihood (default = TRUE).}
}

\value{
  \item{mtxMC}{Matrix of correlation / covariance.}
  \item{mtxAutvlr}{Matrix of eigenvalues.}
  \item{mtxAutvec}{Matrix of eigenvectors.}
  \item{mtxvar}{Matrix of variances and proportions.}
  \item{mtxcarga}{Matrix of factor loadings.}
  \item{mtxvaresp}{Matrix of specific variances.}
  \item{mtxcomuna}{Matrix of commonalities.}
  \item{mtxresidue}{Matrix of residues.}
  \item{vlrsqrs}{Upper limit value for sum of squares of the residues.}
  \item{vlrsqr}{Sum of squares of the residues.}
  \item{mtxresult}{Matrix with all associated results.}
  \item{mtxscores}{Matrix with scores of the observations.}
  \item{coefscores}{Matrix with the scores of the coefficients of the factors.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Kaiser, H. F.\emph{The varimax criterion for analytic rotation in factor analysis.} Psychometrika 23, 187-200, 1958.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.FA}}}

\examples{
data(DataQuan) # data set

data <- DataQuan[,2:ncol(DataQuan)]

rownames(data) <- DataQuan[,1]

Resp <- FA(data, method = "PC", type = 2, nfactor = 3, rotation = "None",
           scoresobs = "Bartlett", converg = 1e-5, iteracao = 1000, 
           testfit = TRUE) 

print("Matrix with all associated results:"); round(Resp$mtxresult,3)

print("Sum of squares of the residues:"); round(Resp$vlrsqr,3)

print("Matrix of the factor loadings.:"); round(Resp$mtxcarga,3)

print("Matrix with scores of the observations:"); round(Resp$mtxscores,3)

print("Matrix with the scores of the coefficients of the factors:"); round(Resp$coefscores,3)

}

\keyword{Factor Analysis}
\keyword{FA}

