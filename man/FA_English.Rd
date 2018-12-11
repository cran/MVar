\name{FA}

\alias{FA}

\title{Factor Analysis (FA).}

\description{Performs factorial analysis (FA) in a data set.}

\usage{FA(Data, Method = "PC", Type = 2, NFactor = 1, Rotation = "None",
   ScoresObs = "Bartlett", Converg = 1e-5, Iteracao = 1000, 
   TestFit = TRUE) 
}

\arguments{
\item{Data}{Data to be analyzed.}
\item{Method}{Method of analysis:\cr
              "PC" - Principal Components (default),\cr
              "PF" - Principal Factor,\cr
              "ML" - Maximum Likelihood.}
\item{Type}{1 for analysis using the covariance matrix,\cr
            2 for analysis using the correlation matrix (default).}
\item{Rotation}{Type of rotation: "None" (default) and "Varimax".}
\item{NFactor}{Number of factors (default = 1).}
\item{ScoresObs}{Type of scores for the observations: "Bartlett" (default) or "Regression".}
\item{Converg}{Limit value for convergence to sum of the squares of the residuals for Maximum likelihood method (default = 1e-5).}
\item{Iteracao}{Maximum number of iterations for Maximum Likelihood method (default = 1000).}
\item{TestFit}{Tests the model fit to the method of Maximum Likelihood (default = TRUE).}
}

\value{
  \item{MatrixMC}{Matrix of correlation / covariance.}
  \item{MatrixAutoVlr}{Matrix of eigenvalues.}
  \item{MatrixAutoVec}{Matrix of eigenvectors.}
  \item{MatrixVar}{Matrix of variances and proportions.}
  \item{MatrixCarga}{Matrix of factor loadings.}
  \item{MatrixVarEsp}{Matrix of specific variances.}
  \item{MatrixComuna}{Matrix of commonalities.}
  \item{MatrixResiduo}{Matrix of residues.}
  \item{VlrSQRS}{Upper limit value for sum of squares of the residues.}
  \item{VlrSQR}{Sum of squares of the residues.}
  \item{MatrixResult}{Matrix with all associated results.}
  \item{MatrixScores}{Matrix with scores of the observations.}
  \item{CoefScores}{Matrix with the scores of the coefficients of the factors.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Kaiser, H. F.\emph{The varimax criterion for analytic rotation in factor analysis.} Psychometrika 23, 187-200, 1958.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.FA}}}

\examples{
data(DataQuan) # data set

Data <- DataQuan[,2:ncol(DataQuan)]

rownames(Data) <- DataQuan[,1]

Resp <- FA(Data, Method = "PC", Type = 2, NFactor = 3, Rotation = "None",
           ScoresObs = "Bartlett", Converg = 1e-5, Iteracao = 1000, 
           TestFit = TRUE) 

print("Matrix with all associated results:"); round(Resp$MatrixResult,3)

print("Sum of squares of the residues:"); round(Resp$VlrSQR,3)

print("Matrix of the factor loadings.:"); round(Resp$MatrixCarga,3)

print("Matrix with scores of the observations:"); round(Resp$MatrixScores,3)

print("Matrix with the scores of the coefficients of the factors:"); round(Resp$CoefScores,3)

}

\keyword{Factor Analysis}
\keyword{FA}

