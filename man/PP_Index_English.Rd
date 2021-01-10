\name{PP_Index}

\alias{PP_Index}

\title{Function to find the Projection Pursuit indexes (PP).}

\description{Function used to find Projection Pursuit indexes (PP).}

\usage{PP_Index(data, class = NA, vector.proj = NA, 
         findex = "HOLES", dimproj = 2, weight = TRUE, 
         lambda = 0.1, r = 1, ck = NA)}

\arguments{
\item{data}{Numeric dataset without class information.}
\item{class}{Vector with names of data classes.}
\item{vector.proj}{Vector projection.}
\item{findex}{Projection index function to be used:\cr
              "lda" - LDA index,\cr
              "pda" - PDA index,\cr
              "lr" - Lr index,\cr
              "holes" - Holes index (default),\cr
              "cm" - Central Mass index,\cr
              "pca" - PCA index,\cr
              "friedmantukey" - Friedman Tukey index,\cr
              "entropy" - Entropy index,\cr
              "legendre" - Legendre index,\cr
              "laguerrefourier" - Laguerre Fourier index,\cr
              "hermite" - Hermite index,\cr
              "naturalhermite" - Natural Hermite index,\cr
              "kurtosismax" - Maximum kurtosis index,\cr
              "kurtosismin" - Minimum kurtosis index,\cr
              "moment" - Moment index,\cr
              "mf" - MF index,\cr
              "chi" - Chi-square index.}
\item{dimproj}{Dimension of data projection (default = 2).}
\item{weight}{Used in index LDA, PDA and Lr to weight the calculations
              for the number of elements in each class (default = TRUE).}
\item{lambda}{Used in the PDA index (default = 0.1).}
\item{r}{Used in the Lr index (default = 1).}
\item{ck}{Internal use of the CHI index function.}
}

\value{
  \item{num.class}{Number of classes.}
  \item{class.names}{Class names.}
  \item{findex}{Projection index function used.}
  \item{vector.proj}{Projection vectors found.}
  \item{index}{Projection index found in the process.}
}

\references{
OSSANI, P. C.; FIGUEIRA, M. R.; CIRILLO, M. A. Proposition of a new index for projection pursuit in the multiple factor analysis. \emph{Computational and Mathematical Methods}, v. 1, p. 1-18, 2020.

COOK, D., BUJA, A., CABRERA, J.. Projection pursuit indexes based on orthonormal function expansions. \emph{Journal of Computational and Graphical Statistics}, 2(3):225-250, 1993.

COOK, D., BUJA, A., CABRERA, J., HURLEY, C.. Grand tour and projection pursuit, \emph{Journal of Computational and Graphical Statistics}, 4(3), 155-172, 1995.

COOK, D., SWAYNE, D. F.. Interactive and Dynamic Graphics for data Analysis: With R and GGobi. Springer. 2007.

ESPEZUA, S., VILLANUEVA, E., MACIEL, C.D., CARVALHO, A.. A projection pursuit framework for supervised dimension reduction of high dimensional small sample datasets. \emph{Neurocomputing}, 149, 767-776, 2015.

FRIEDMAN, J. H., TUKEY, J. W. A projection pursuit algorithm for exploratory data analysis. \emph{IEEE Transaction on Computers}, 23(9):881-890, 1974.

HASTIE, T., BUJA, A., TIBSHIRANI, R.: Penalized discriminant analysis. \emph{The Annals of Statistics}. 23(1), 73-102 . 1995.

HUBER, P. J.. Projection pursuit. \emph{Annals of Statistics}, 13(2):435-475, 1985.

JONES, M. C., SIBSON, R.. What is projection pursuit, (with discussion), \emph{Journal of the Royal Statistical Society}, Series A 150, 1-36, 1987.

LEE, E. K., COOK, D.. A projection pursuit index for large p small n data. \emph{Statistics and Computing}, 20(3):381-392, 2010.

LEE, E., COOK, D., KLINKE, S., LUMLEY, T.. Projection pursuit for exploratory supervised classification. \emph{Journal of Computational and Graphical Statistics}, 14(4):831-846, 2005.

MARTINEZ, W. L., MARTINEZ, A. R.; \emph{Computational Statistics Handbook with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2007. 794 p.

MARTINEZ, W.  L., MARTINEZ, A. R., SOLKA, J.; \emph{Exploratory data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

PENA, D., PRIETO, F.. Cluster identification using projections. \emph{Journal of the American Statistical Association}, 96(456):1433-1445, 2001.

POSSE, C.. Projection pursuit exploratory data analysis, \emph{Computational Statistics and data Analysis}, 29:669-687, 1995a.

POSSE, C.. Tools for two-dimensional exploratory projection pursuit, \emph{Journal of Computational and Graphical Statistics}, 4:83-100, 1995b.

}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PP_Optimizer}} and \code{\link{Plot.PP}}}

\examples{
data(iris) # data set

data <- iris[,1:4]

# Example 1 - Without the classes in the data
ind <- PP_Index(data = data, class = NA, vector.proj = NA, 
                findex = "moment", dimproj = 2, weight = TRUE, 
                lambda = 0.1, r = 1)

print("Number of classes:"); ind$num.class
print("class Names:"); ind$class.names
print("Projection index function:"); ind$findex
print("Projection vectors:"); ind$vector.proj  
print("Projection index:"); ind$index


# Example 2 - With the classes in the data
class <- iris[,5] # data class

findex <- "pda" # index function

sphere <- TRUE # spherical data

res <- PP_Optimizer(data = data, class = class, findex = findex,
                    optmethod = "SA", dimproj = 2, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 1000, half = 30)

# Comparing the result obtained
if (match(toupper(findex),c("LDA", "PDA", "LR"), nomatch = 0) > 0) {
  if (sphere) {
     data <- apply(predict(prcomp(data)), 2, scale) # spherical data
  }
} else data <- as.matrix(res$proj.data[,1:Dim])

ind <- PP_Index(data = data, class = class, vector.proj = res$vector.opt, 
                findex = findex, dimproj = 2, weight = TRUE, lambda = 0.1,
                r = 1)

print("Number of classes:"); ind$num.class
print("class Names:"); ind$class.names
print("Projection index function:"); ind$findex
print("Projection vectors:"); ind$vector.proj  
print("Projection index:"); ind$index
print("Optimized Projection index:"); res$index[length(res$index)]
}

\keyword{Projection pursuit}
\keyword{PP}
