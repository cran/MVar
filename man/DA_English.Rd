\name{DA}

\alias{DA}

\title{Linear (LDA) and quadratic discriminant analysis (QDA).}

\description{Perform linear and quadratic discriminant analysis.}

\usage{DA(Data, Class = NA, Type = "lda", Validation = "Learning", 
   Method = "moment", Prior = NA, Testing = NA)}

\arguments{
  \item{Data}{Data to be classified.}
  \item{Class}{Vector with data classes names.}
  \item{Type}{"lda": linear discriminant analysis (default), or \cr
              "qda": quadratic discriminant analysis.}
  \item{Validation}{Type of validation:\cr
                    "Learning" - data training (default), or\cr
                    "Testing" - classifies the data of the vector "Testing".}
  \item{Method}{Classification method:\cr 
                "mle" to MLEs,\cr
                "mve" to use cov.mv, \cr
                "moment" (default) for standard mean and variance estimators, or \cr
                "t" for robust estimates based on the t distribution.}
  \item{Prior}{Probabilities of occurrence of classes. If not specified, it will take the proportions of the classes. If specified, probabilities must follow the order of factor levels.}
  \item{Testing}{Vector with indices that will be used in Data as test. For Validation = "Learning", one has Testing = NA.}
}

\value{
\item{Confusion}{Confusion table.}
\item{Error.rate}{Overall error ratio.}
\item{Prior}{Probability of classes.}
\item{Type}{Type of discriminant analysis.}
\item{Validation}{Type of validation.}
\item{Num.Class}{Number of classes.}
\item{Class.Names}{Class names.}
\item{Method}{Classification method.}
\item{Num.Correct}{Number of correct observations.}
\item{Results}{Matrix with comparative classification results.}
}

\references{
FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

RIPLEY, B. D. \emph{Pattern Recognition and Neural Networks.} Cambridge University Press, 1996.

VENABLESs, W. N. and RIPLEY, B. D. \emph{Modern Applied Statistics with S.} Fourth edition. Springer, 2002. 
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # data set

Data  = iris[,1:4] # data to be classified
Class = iris[,5]   # data class
Prior = c(1,1,1)/3 # a priori probability of the classs

Res <- DA(Data, Class, Type = "lda", Validation = "Learning", 
          Method = "mle", Prior = Prior, Testing = NA)

print("Confusion table:"); Res$Confusion
print("Overall hit ratio:"); 1 - Res$Error.rate
print("Probability of classes:"); Res$Prior
print("Classification method:"); Res$Method
print("Type of discriminant analysis:"); Res$Type
print("Class names:"); Res$Class.Names
print("Number of classess:"); Res$Num.Class
print("Type of validation:"); Res$Validation
print("Number of correct observations:"); Res$Num.Correct
print("Matrix with comparative classification results:"); Res$Results


### cross-validation ###
amostra   = sample(2, nrow(Data), replace = TRUE, prob = c(0.7,0.3))
Datatrain = Data[amostra == 1,] # training data
Datatest  = Data[amostra == 2,] # test data

dim(Datatrain) # training data dimension
dim(Datatest)  # test data dimension

Testing  = as.integer(rownames(Datatest)) # test data index

Res <- DA(Data, Class, Type = "qda", Validation = "testing", 
          Method = "moment", Prior = NA, Testing = Testing)

print("Confusion table:"); Res$Confusion
print("Overall hit ratio:"); 1 - Res$Error.rate
print("Number of correct observations:"); Res$Num.Correct
print("Matrix with comparative classification results:"); Res$Results

}

\keyword{Linear and quadratic discriminant analysis}
\keyword{LDA}
\keyword{QDA}