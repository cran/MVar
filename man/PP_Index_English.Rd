\name{PP_Index}

\alias{PP_Index}

\title{Function to find the Projection Pursuit indexes (PP).}

\description{Function used to find Projection Pursuit indexes (PP).}

\usage{PP_Index(Data, Class = NA, Vector.Proj = NA, 
         Findex = "HOLES", DimProj = 2, Weight = TRUE, 
         Lambda = 0.1, r = 1, ck = NA)}

\arguments{
\item{Data}{Numeric dataset without class information.}
\item{Class}{Vector with names of data classes.}
\item{Vector.Proj}{Vector projection.}
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
\item{DimProj}{Dimension of data projection (default = 2).}
\item{Weight}{Used in index LDA, PDA and Lr to weight the calculations
              for the number of elements in each class (default = TRUE).}
\item{Lambda}{Used in the PDA index (default = 0.1).}
\item{r}{Used in the Lr index (default = 1).}
\item{ck}{Internal use of the CHI index function.}
}

\value{
  \item{Num.Class}{Number of classes.}
  \item{Class.Names}{Class names.}
  \item{Findex}{Projection index function used.}
  \item{Vector.Proj}{Projection vectors found.}
  \item{Index}{Projection index found in the process.}
}

\references{
COOK, D., BUJA, A., CABRERA, J.. Projection pursuit indexes based on orthonormal function expansions. \emph{Journal of Computational and Graphical Statistics}, 2(3):225-250, 1993.

COOK, D., BUJA, A., CABRERA, J., HURLEY, C.. Grand tour and projection pursuit, \emph{Journal of Computational and Graphical Statistics}, 4(3), 155-172, 1995.

COOK, D., SWAYNE, D. F.. Interactive and Dynamic Graphics for Data Analysis: With R and GGobi. Springer. 2007.

ESPEZUA, S., VILLANUEVA, E., MACIEL, C.D., CARVALHO, A.. A projection pursuit framework for supervised dimension reduction of high dimensional small sample datasets. \emph{Neurocomputing}, 149, 767-776, 2015.

FRIEDMAN, J. H., TUKEY, J. W. A projection pursuit algorithm for exploratory data analysis. \emph{IEEE Transaction on Computers}, 23(9):881-890, 1974.

HASTIE, T., BUJA, A., TIBSHIRANI, R.: Penalized discriminant analysis. \emph{The Annals of Statistics}. 23(1), 73-102 . 1995.

HUBER, P. J.. Projection pursuit. \emph{Annals of Statistics}, 13(2):435-475, 1985.

JONES, M. C., SIBSON, R.. What is projection pursuit, (with discussion), \emph{Journal of the Royal Statistical Society}, Series A 150, 1-36, 1987.

LEE, E. K., COOK, D.. A projection pursuit index for large p small n data. \emph{Statistics and Computing}, 20(3):381-392, 2010.

LEE, E., COOK, D., KLINKE, S., LUMLEY, T.. Projection pursuit for exploratory supervised classification. \emph{Journal of Computational and Graphical Statistics}, 14(4):831-846, 2005.

MARTINEZ, W. L., MARTINEZ, A. R.; \emph{Computational Statistics Handbook with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2007. 794 p.

MARTINEZ, W.  L., MARTINEZ, A. R., SOLKA, J.; \emph{Exploratory Data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

PENA, D., PRIETO, F.. Cluster identification using projections. \emph{Journal of the American Statistical Association}, 96(456):1433-1445, 2001.

POSSE, C.. Projection pursuit exploratory data analysis, \emph{Computational Statistics and Data Analysis}, 29:669-687, 1995a.

POSSE, C.. Tools for two-dimensional exploratory projection pursuit, \emph{Journal of Computational and Graphical Statistics}, 4:83-100, 1995b.

}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PP_Optimizer}} and \code{\link{Plot.PP}}}

\examples{
data(iris) # data set

Dat <- iris[,1:4]

# Example 1 - Without the classes in the data
Ind <- PP_Index(Data = Dat, Class = NA, Vector.Proj = NA, 
                Findex = "moment", DimProj = 2, Weight = TRUE, 
                Lambda = 0.1, r = 1)

print("Number of classes:"); Ind$Num.Class
print("Class Names:"); Ind$Class.Names
print("Projection index function:"); Ind$Findex
print("Projection vectors:"); Ind$Vector.Proj  
print("Projection Index:"); Ind$Index


# Example 2 - With the classes in the data
Class <- iris[,5] # data class

FcIndex <- "pda" # index function

Sphere <- TRUE # spherical data

Res <- PP_Optimizer(Data = Dat, Class = Class, Findex = FcIndex,
                    OptMethod = "SA", DimProj = 2, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 1000, Half = 30)

# Comparing the result obtained
if (match(toupper(FcIndex),c("LDA", "PDA", "LR"), nomatch = 0) > 0) {
  if (Sphere) {
     Data <- apply(predict(prcomp(Dat)), 2, scale) # spherical data
  } else  Data <- Dat # non-spherical data
} else Data <- as.matrix(Res$Proj.Data[,1:Dim])

Ind <- PP_Index(Data = Data, Class = Class, Vector.Proj = Res$Vector.Opt, 
                Findex = FcIndex, DimProj = 2, Weight = TRUE, Lambda = 0.1,
                r = 1)

print("Number of classes:"); Ind$Num.Class
print("Class Names:"); Ind$Class.Names
print("Projection index function:"); Ind$Findex
print("Projection vectors:"); Ind$Vector.Proj  
print("Projection index:"); Ind$Index
print("Optimized Projection Index:"); Res$Index[length(Res$Index)]
}

\keyword{Projection pursuit}
\keyword{PP}
