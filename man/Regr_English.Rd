\name{Regr}

\alias{Regr}

\title{Linear regression.}

\description{Performs linear regression on a data set.}

\usage{Regr(Y, X, namevarx = NA, intercepts = TRUE, sigf = 0.05)}

\arguments{
\item{Y}{Variable response.}
\item{X}{Regression variables.}
\item{namevarx}{Name of the variable, or variables X, if not set, assumes the default text.}
\item{intercepts}{Consider the intercept in the regression (default = TRUE).}
\item{sigf}{Level of significance of residue tests(default = 5\%).}
}

\value{
\item{Betas}{Regression coefficients.}
\item{CovBetas}{Covariance matrix of the regression coefficients.}
\item{ICc}{Confidence interval of the regression coefficients.}
\item{hip.test}{Hypothesis test of the regression coefficients.}
\item{ANOVA}{Regression analysis of the variance.}
\item{R}{Determination coefficient.}
\item{Rc}{Corrected coefficient of determination.}
\item{Ra}{Adjusted coefficient of determination.}
\item{QME}{Variance of the residues.}
\item{ICQME}{Confidence interval of the residue variance.}
\item{prev}{Prediction of the regression fit.}
\item{IPp}{Predictions interval}
\item{ICp}{Interval of prediction confidence}
\item{error}{Residuals of the regression fit.}
\item{error.test}{It returns to 5\% of significance the test of independence, normality and homogeneity of the variance of the residues.}
}

\references{
CHARNET, R.; at al.. \emph{Analise de modelos de regressao lienar,} 2a ed. Campinas: Editora da Unicamp, 2008. 357 p.

RENCHER, A. C.; SCHAALJE, G. B. \emph{Linear models in statisctic.} 2th. ed. New Jersey: John & Sons, 2008. 672 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Plot.Regr}}}

\examples{
data(DataMix)

Y <- DataMix[,2]

X <- DataMix[,6:7]

name.x <- c("Special Coffees", "Commercial Coffees")

res <- Regr(Y, X, namevarx = name.x , intercepts = TRUE, sigf = 0.05)

print("Regression Coefficients:"); round(res$Betas,4)
print("Analysis of Variance:"); res$ANOVA
print("Hypothesis test of regression coefficients:"); round(res$hip.test,4)
print("Determination coefficient:"); round(res$R,4)
print("Corrected coefficient of determination:"); round(res$Rc,4) 
print("Adjusted coefficient of determination:"); round(res$Ra,4)
print("Tests of the residues"); res$error.test
}

\keyword{Regression}
