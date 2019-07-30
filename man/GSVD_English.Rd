\name{GSVD}

\alias{GSVD}

\title{Generalized Singular Value Decomposition (GSVD).}

\description{Given the matrix \eqn{A} of order \eqn{nxm}, the generalized singular value decomposition (GSVD) involves the use of two sets of positive square matrices of order \eqn{nxn} and \eqn{mxm} respectively. These two matrices express constraints imposed, respectively, on the lines and columns of \eqn{A}.}

\usage{GSVD(data, plin = NULL, pcol = NULL)}
\arguments{
  \item{data}{Matrix used for decomposition.}
  \item{plin}{Weight for rows.}
  \item{pcol}{Weight for columns}
}

\details{If plin or pcol is not used, it will be calculated as the usual singular value decomposition.}

\value{
 \item{d}{Eigenvalues, that is, line vector with singular values of the decomposition.}
 \item{u}{Eigenvectors referring rows.}
 \item{v}{Eigenvectors referring columns.}
}

\references{ABDI, H. Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 907-912.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
M = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 4, ncol = 3)

svd(M)  # Usual Singular Value Decomposition

GSVD(M) # GSVD with the same previous results

# GSVD with weights for rows and columns
GSVD(M, plin = c(0.1,0.5,2,1.5), pcol = c(1.3,2,0.8)) 
}

\keyword{Generalized Singular Value Decomposition}
\keyword{GSVD}
