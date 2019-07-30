\name{DA}

\alias{DA}

\title{Linear (LDA) and quadratic discriminant analysis (QDA).}

\description{Perform linear and quadratic discriminant analysis.}

\usage{DA(data, class = NA, type = "lda", validation = "Learning", 
   method = "moment", prior = NA, testing = NA)}

\arguments{
  \item{data}{Data to be classified.}
  \item{class}{Vector with data classes names.}
  \item{type}{"lda": linear discriminant analysis (default), or \cr
              "qda": quadratic discriminant analysis.}
  \item{validation}{Type of validation:\cr
                    "Learning" - data training (default), or\cr
                    "testing" - classifies the data of the vector "testing".}
  \item{method}{Classification method:\cr 
                "mle" to MLEs,\cr
                "mve" to use cov.mv, \cr
                "moment" (default) for standard mean and variance estimators, or \cr
                "t" for robust estimates based on the t distribution.}
  \item{prior}{Probabilities of occurrence of classes. If not specified, it will take the proportions of the classes. If specified, probabilities must follow the order of factor levels.}
  \item{testing}{Vector with indices that will be used in data as test. For validation = "Learning", one has testing = NA.}
}

\value{
\item{confusion}{Confusion table.}
\item{error.rate}{Overall error ratio.}
\item{prior}{Probability of classes.}
\item{type}{Type of discriminant analysis.}
\item{validation}{Type of validation.}
\item{num.class}{Number of classes.}
\item{class.names}{Class names.}
\item{method}{Classification method.}
\item{num.correct}{Number of correct observations.}
\item{results}{Matrix with comparative classification results.}
}

\references{
FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

RIPLEY, B. D. \emph{Pattern Recognition and Neural Networks.} Cambridge University Press, 1996.

VENABLESs, W. N. and RIPLEY, B. D. \emph{Modern Applied Statistics with S.} Fourth edition. Springer, 2002. 
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # data set

data  = iris[,1:4] # data to be classified
class = iris[,5]   # data class
prior = c(1,1,1)/3 # a priori probability of the classs

Res <- DA(data, class, type = "lda", validation = "Learning", 
          method = "mle", prior = prior, testing = NA)

print("confusion table:"); Res$confusion
print("Overall hit ratio:"); 1 - Res$error.rate
print("Probability of classes:"); Res$prior
print("classification method:"); Res$method
print("type of discriminant analysis:"); Res$type
print("class names:"); Res$class.names
print("Number of classess:"); Res$num.class
print("type of validation:"); Res$validation
print("Number of correct observations:"); Res$num.correct
print("Matrix with comparative classification results:"); Res$results


### cross-validation ###
amostra   = sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
datatrain = data[amostra == 1,] # training data
datatest  = data[amostra == 2,] # test data

dim(datatrain) # training data dimension
dim(datatest)  # test data dimension

testing  = as.integer(rownames(datatest)) # test data index

Res <- DA(data, class, type = "qda", validation = "testing", 
          method = "moment", prior = NA, testing = testing)

print("confusion table:"); Res$confusion
print("Overall hit ratio:"); 1 - Res$error.rate
print("Number of correct observations:"); Res$num.correct
print("Matrix with comparative classification results:"); Res$results

}

\keyword{Linear and quadratic discriminant analysis}
\keyword{LDA}
\keyword{QDA}