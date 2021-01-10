\name{CA}

\alias{CA}

\title{Correspondence Analysis (CA).}

\description{Performs simple correspondence analysis (CA) and multiple (MCA) in a data set.}

\usage{CA(data, typdata = "f", typmatrix = "I")}

\arguments{
\item{data}{Data to be analyzed (contingency table).}
\item{typdata}{"f" for frequency data (default),\cr
               "c" for qualitative data.}
\item{typmatrix}{Matrix used for calculations when typdata = "c".\cr
                 "I" for indicator matrix (default),\cr
                 "B" for Burt's matrix.}
}

\value{
\item{depdata}{Verify if the rows and columns are dependent, or independent by the chi-square test, at the 5\% significance level.}
\item{typdata}{Data type: "F" frequency or "C" qualitative.}
\item{numcood}{Number of principal components.}
\item{mtxP}{Matrix of the relative frequency.}
\item{vtrR}{Vector with sums of the rows.}
\item{vtrC}{Vector with sums of the columns.}
\item{mtxPR}{Matrix with profile of the rows.}
\item{mtxPC}{Matrix with profile of the columns}
\item{mtxZ}{Matrix Z.}
\item{mtxU}{Matrix with the eigenvectors U.}
\item{mtxV}{Matrix with the eigenvectors V.}
\item{mtxL}{Matrix with eigenvalues.}
\item{mtxX}{Matrix with the principal coordinates of the rows.}
\item{mtxY}{Matrix with the principal coordinates of the columns.}
\item{mtxAutvlr}{Matrix of the inertias (variances), with the proportions and proportions accumulated.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.CA}}}

\examples{
data(DataFreq) # frequency data set

data <- DataFreq[,2:ncol(DataFreq)]

rownames(data) <- as.character(t(DataFreq[1:nrow(DataFreq),1]))

res <- CA(data = data, "f") # performs CA

print("Is there dependency between rows and columns?"); res$depdata

print("Number of principal coordinates:"); res$numcood

print("Principal coordinates of the rows:"); round(res$mtxX,2)

print("Principal coordinates of the columns:"); round(res$mtxY,2)

print("Inertia of the principal components:"); round(res$mtxAutvlr,2)
}

\keyword{Simple Correspondence Analysis}
\keyword{Multiple Correspondence Analysis} 
\keyword{CA}
\keyword{MCA}
