\name{CA}

\alias{CA}

\title{Correspondence Analysis (CA).}

\description{Performs simple correspondence analysis (CA) and multiple (MCA) in a data set.}

\usage{CA(Data, TypData = "f", TypMatrix = "I")}

\arguments{
\item{Data}{Data to be analyzed (contingency table).}
\item{TypData}{"f" for frequency data (default),\cr
               "c" for qualitative data.}
\item{TypMatrix}{Matrix used for calculations when TypData = "c".\cr
                 "I" for indicator matrix (default),\cr
                 "B" for Burt's matrix.}
}

\value{
\item{DepData}{Verify if the rows and columns are dependent, or independent by the chi-square test, at the 5\% significance level.}
\item{TypData}{Data type: "F" frequency or "C" qualitative.}
\item{NumCood}{Number of principal components.}
\item{MatrixP}{Matrix of the relative frequency.}
\item{VectorR}{Vector with sums of the rows.}
\item{VectorC}{Vector with sums of the columns.}
\item{MatrixPR}{Matrix with profile of the rows.}
\item{MatrixPC}{Matrix with profile of the columns}
\item{MatrixZ}{Matrix Z.}
\item{MatrixU}{Matrix with the eigenvectors U.}
\item{MatrixV}{Matrix with the eigenvectors V.}
\item{MatrixL}{Matrix with eigenvalues.}
\item{MatrixX}{Matrix with the principal coordinates of the rows.}
\item{MatrixY}{Matrix with the principal coordinates of the columns.}
\item{MatrixAutoVlr}{Matrix of the inertias (variances), with the proportions and proportions accumulated.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.CA}}}

\examples{
data(DataFreq) # frequency data set

Dat <- DataFreq[,2:ncol(DataFreq)]

rownames(Dat) <- as.character(t(DataFreq[1:nrow(DataFreq),1]))

Resp <- CA(Dat, "f") # performs CA

print("Is there dependency between rows and columns?"); Resp$DepData

print("Number of principal coordinates:"); Resp$NumCood

print("Principal coordinates of the rows:"); round(Resp$MatrixX,2)

print("Principal coordinates of the columns:"); round(Resp$MatrixY,2)

print("Inertia of the principal components:"); round(Resp$MatrixAutoVlr,2)
}

\keyword{Simple Correspondence Analysis}
\keyword{Multiple Correspondence Analysis} 
\keyword{CA}
\keyword{MCA}
