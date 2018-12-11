\name{PP_Optimizer}

\alias{PP_Optimizer}

\title{Optimization function of the Projection Pursuit index (PP).}

\description{Optimization function of the Projection Pursuit index (PP).}

\usage{PP_Optimizer(Data, Class = NA, Findex = "HOLES",   
             DimProj = 2, Sphere = TRUE, OptMethod = "GTSA",   
             Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9,  
             Eps = 1e-3, Maxiter = 3000, Half = 30)}

\arguments{
  \item{Data}{Numeric dataset without class information.}
  \item{Class}{Vector with names of data classes.}
  \item{Findex}{Projection index function to be used:\cr
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
                "hermite" - Hermite Index,\cr
                "naturalhermite" - Natural Hermite Index,\cr
                "kurtosismax" - Maximum kurtosis index,\cr
                "kurtosismin" - Minimum kurtosis index,\cr
                "moment" - Moment index,\cr
                "chi" - Chi-square index.}
  \item{DimProj}{Dimension of the data projection (default = 2).}
  \item{Sphere}{Spherical data (default = TRUE).}
  \item{OptMethod}{Optimization method GTSA - Grand Tour Simulated Annealing or SA - Simulated Annealing (default = "GTSA").}
  \item{Weight}{Used in index LDA, PDA and Lr to weight the calculations
                for the number of elements in each class (default = TRUE).}
  \item{Lambda}{Used in the PDA index (default = 0.1).}
  \item{r}{Used in the Lr index (default = 1).}
  \item{Cooling}{Cooling rate (default = 0.9).}
  \item{Eps}{Approximation accuracy for Cooling (default = 1e-3).}
  \item{Maxiter}{Maximum number of iterations of the algorithm (default = 3000).}
  \item{Half}{Number of steps without incrementing the index, then decreasing the Cooling value (default = 30).}
}

\value{
  \item{Num.Class}{Number of classes.}
  \item{Class.Names}{Class names.}
  \item{Proj.Data}{Projected data.}
  \item{Vector.Opt}{Projection vectors found.}
  \item{Index}{Vector with the projection indices found in the process, converging to the maximum, or the minimum.}
  \item{Findex}{Projection index function used.}
}

\references{
COOK, D., LEE, E. K., BUJA, A., WICKHAM, H.. Grand tours, projection pursuit guided tours and manual controls. In Chen, Chunhouh, Hardle, Wolfgang, Unwin, e Antony (Eds.), \emph{Handbook of Data Visualization}, Springer Handbooks of Computational Statistics, chapter III.2, p. 295-314. Springer, 2008.

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
Data <- iris[,1:4]

Class <- NA # data class

FcIndex <- "kurtosismax" # index function

Dim <- 1 # dimension of data projection

Sphere <- TRUE # spherical data

Res <- PP_Optimizer(Data = Data, Class = Class, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 1000, Half = 30)
 
print("Number of classes:"); Res$Num.Class
print("Class Names:"); Res$Class.Names
print("Projection index function:"); Res$Findex
print("Projected data:"); Res$Proj.Data
print("Projection vectors:"); Res$Vector.Opt
print("Projection index:"); Res$Index


# Example 2 - With the classes in the data
Class <- iris[,5] # classe dos dados

Res <- PP_Optimizer(Data = Data, Class = Class, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 1000, Half = 30)

print("Number of classes:"); Res$Num.Class
print("Class Names:"); Res$Class.Names
print("Projection index function:"); Res$Findex
print("Projected data:"); Res$Proj.Data
print("Projection vectors:"); Res$Vector.Opt
print("Projection index:"); Res$Index
}

\keyword{Projection pursuit}
\keyword{PP}
