\name{PCA}

\alias{PCA}

\title{Principal Components Analysis (PCA).}

\description{Performs principal component analysis (PCA) in a data set.}

\usage{PCA(Data, Type = 1)}

\arguments{
  \item{Data}{Data to be analyzed.}
  \item{Type}{1 for analysis using the covariance matrix (default),\cr
              2 for analysis using the correlation matrix.}
}

\value{
  \item{MatrixMC      }{Matrix of covariance or correlation according to "Type".}
  \item{MatrixAutoVlr }{Matrix of eigenvalues (variances) with the proportions and proportions accumulated.}
  \item{MatrixAutoVec }{Matrix of eigenvectors - principal components.}
  \item{MatrixVCP     }{Matrix of covariance of the principal components with the original variables.}
  \item{MatrixCCP     }{Matrix of correlation of the principal components with the original variables.}
  \item{MatrixEsc     }{Matrix with scores of the principal components.}
}

\references{
HOTELLING, H. Analysis of a complex of statistical variables into principal components. \emph{Journal of Educational Psychology}, Arlington, v. 24, p. 417-441, Sept. 1933.

MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.PCA}}}

\examples{
data(DataQuan) # set of quantitative data

Dat <- DataQuan[,2:8]

rownames(Dat) <- DataQuan[1:nrow(DataQuan),1]

PC <- PCA(Dat, 2) # performs the PCA

print("Covariance matrix / Correlation:"); round(PC$MatrixMC,2)

print("Principal Components:"); round(PC$MatrixAutoVec,2)

print("Principal Component Variances:"); round(PC$MatrixAutoVlr,2)

print("Covariance of the Principal Components:"); round(PC$MatrixVCP,2)

print("Correlation of the Principal Components:"); round(PC$MatrixCCP,2)

print("Scores of the Principal Components:"); round(PC$MatrixEsc,2)
}

\keyword{Principal Components Analysis}
\keyword{PCA}
