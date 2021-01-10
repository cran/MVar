\name{PP_Optimizer}

\alias{PP_Optimizer}

\title{Optimization function of the Projection Pursuit index (PP).}

\description{Optimization function of the Projection Pursuit index (PP).}

\usage{PP_Optimizer(data, class = NA, findex = "HOLES",   
             dimproj = 2, sphere = TRUE, optmethod = "GTSA",   
             weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9,  
             eps = 1e-3, maxiter = 3000, half = 30)}

\arguments{
  \item{data}{Numeric dataset without class information.}
  \item{class}{Vector with names of data classes.}
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
  \item{dimproj}{Dimension of the data projection (default = 2).}
  \item{sphere}{Spherical data (default = TRUE).}
  \item{optmethod}{Optimization method GTSA - Grand Tour Simulated Annealing or SA - Simulated Annealing (default = "GTSA").}
  \item{weight}{Used in index LDA, PDA and Lr to weight the calculations
                for the number of elements in each class (default = TRUE).}
  \item{lambda}{Used in the PDA index (default = 0.1).}
  \item{r}{Used in the Lr index (default = 1).}
  \item{cooling}{Cooling rate (default = 0.9).}
  \item{eps}{Approximation accuracy for cooling (default = 1e-3).}
  \item{maxiter}{Maximum number of iterations of the algorithm (default = 3000).}
  \item{half}{Number of steps without incrementing the index, then decreasing the cooling value (default = 30).}
}

\value{
  \item{num.class}{Number of classes.}
  \item{class.names}{Class names.}
  \item{proj.data}{Projected data.}
  \item{vector.opt}{Projection vectors found.}
  \item{index}{Vector with the projection indices found in the process, converging to the maximum, or the minimum.}
  \item{findex}{Projection index function used.}
}

\references{
COOK, D., LEE, E. K., BUJA, A., WICKHAM, H.. Grand tours, projection pursuit guided tours and manual controls. In Chen, Chunhouh, Hardle, Wolfgang, Unwin, e Antony (Eds.), \emph{Handbook of data Visualization}, Springer Handbooks of Computational Statistics, chapter III.2, p. 295-314. Springer, 2008.

LEE, E., COOK, D., KLINKE, S., LUMLEY, T.. Projection pursuit for exploratory supervised classification. \emph{Journal of Computational and Graphical Statistics}, 14(4):831-846, 2005.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.PP}} and \code{\link{PP_Index}}}

\examples{
data(iris) # data set

# Example 1 - Without the classes in the data
data <- iris[,1:4]

class <- NA # data class

findex <- "kurtosismax" # index function

dim <- 1 # dimension of data projection

sphere <- TRUE # spherical data

res <- PP_Optimizer(data = data, class = class, findex = findex,
                    optmethod = "GTSA", dimproj = dim, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 1000, half = 30)
 
print("Number of classes:"); res$num.class
print("class Names:"); res$class.names
print("Projection index function:"); res$findex
print("Projected data:"); res$proj.data
print("Projection vectors:"); res$vector.opt
print("Projection index:"); res$index


# Example 2 - With the classes in the data
class <- iris[,5] # classe dos dados

res <- PP_Optimizer(data = data, class = class, findex = findex,
                    optmethod = "GTSA", dimproj = dim, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 1000, half = 30)

print("Number of classes:"); res$num.class
print("class Names:"); res$class.names
print("Projection index function:"); res$findex
print("Projected data:"); res$proj.data
print("Projection vectors:"); res$vector.opt
print("Projection index:"); res$index
}

\keyword{Projection pursuit}
\keyword{PP}
