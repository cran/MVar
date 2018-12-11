\name{CCA}

\alias{CCA}

\title{Canonical Correlation Analysis(CCA).}

\description{Perform Canonical Correlation Analysis (CCA) on a data set.}

\usage{CCA(X = NULL, Y = NULL, Type = 1, Test = "Bartlett", Sign = 0.05)}

\arguments{
  \item{X}{First group of variables of a data set.}
  \item{Y}{Second group of variables of a data set.}
  \item{Type}{1 for analysis using the covariance matrix (default),\cr
              2 for analysis using the correlation matrix.}
  \item{Test}{Test of significance of the relationship between the group X and Y:\cr
              "Bartlett" (default) or "Rao".}
  \item{Sign}{Test significance level (default 5\%).}
}

\value{
  \item{Cxx    }{Covariance matrix or correlation Cxx.}
  \item{Cyy    }{Covariance matrix or correlation Cyy.}
  \item{Cxy    }{Covariance matrix or correlation Cxy.}
  \item{Cyx    }{Covariance matrix or correlation Cyx.}
  \item{Var.UV }{Matrix with eigenvalues (variances) of the canonical pairs U and V.}
  \item{Corr.UV}{Matrix of the correlation of the canonical pairs U and V.}
  \item{Coef.X }{Matrix of the canonical coefficients of the group X.}
  \item{Coef.Y }{Matrix of the canonical coefficients of the group Y.}
  \item{Coor.X }{Matrix of the correlations between canonical variables and the original variables of the group X.}
  \item{Coor.Y }{Matrix of the correlations between the canonical variables and the original variables of the group Y.}
  \item{Score.X}{Matrix with the scores of the group X.}
  \item{Score.Y}{Matrix with the scores of the group Y.}
  \item{SigTest}{Returns the significance test of the relationship between group X and Y: "Bartlett" (default) or "Rao".}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

LATTIN, J.; CARROL, J. D.; GREEN, P. E. \emph{Analise de dados multivariados.} 1th. ed. Sao Paulo: Cengage Learning, 2011. 455 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.CCA}}}

\examples{
data(DataMix) # data set

Data <- DataMix[,2:ncol(DataMix)]

rownames(Data) <- DataMix[,1]

X <- as.data.frame(NormData(Data[,1:2],2))

Y <- as.data.frame(NormData(Data[,5:6],2))

Resp <- CCA(X, Y, Type = 1, Test = "Bartlett", Sign = 0.05)

print("Matrix with eigenvalues (variances) of the canonical pairs U and V:"); round(Resp$Var.UV,3)

print("Matrix of the correlation of the canonical pairs U and V:"); round(Resp$Corr.UV,3)

print("Matrix of the canonical coefficients of the group X:"); round(Resp$Coef.X,3)

print("Matrix of the canonical coefficients of the group Y:"); round(Resp$Coef.Y,3)

print("Matrix of the correlations between the canonical 
       variables and the original variables of the group X:"); round(Resp$Coor.X,3)

print("Matrix of the correlations between the canonical 
       variables and the original variables of the group Y:"); round(Resp$Coor.Y,3)

print("Matrix with the scores of the group X:"); round(Resp$Score.X,3)

print("Matrix with the scores of the group Y:"); round(Resp$Score.Y,3)

print("Test of significance of the canonical pairs:"); Resp$SigTest
}

\keyword{Analysis of canonical correlation}
\keyword{CCA}
