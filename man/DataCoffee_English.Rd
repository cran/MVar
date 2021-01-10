\name{DataCoffee}

\alias{DataCoffee}

\docType{data}

\title{Frequency data set.}

\description{Set of data categorized by coffees, on sensorial abilities in the consumption of special coffees.}

\usage{data(DataCoffee)}

\format{Data set of a research done with the purpose of evaluating the concordance between the responses of different groups of consumers with different sensorial abilities. The experiment relates the sensorial analysis of special coffees defined by (A) Yellow Bourbon, cultivated at altitudes greater than 1200 m; (D) idem to (A) differing only in the preparation of the samples; (B) Acaia cultivated at an altitude of less than 1,100 m; (C) identical to (B) but differentiating the sample preparation. Here the data are categorized by coffees. The example given demonstrates the results found in OSSANI et al. (2017).}


\references{
OSSANI, P. C.; CIRILLO, M. A.; BOREM, F. M.; RIBEIRO, D. E.; CORTEZ, R. M.. Quality of specialty coffees: a sensory evaluation by consumers using the MFACT technique. \emph{Revista Ciencia Agronomica (UFC. Online)}, v. 48, p. 92-100, 2017.

OSSANI, P. C. \emph{Qualidade de cafes especiais e nao especiais por meio da analise de multiplos fatores para tabelas de contingencias.} 2015. 107 p. Dissertacao (Mestrado em Estatistica e Experimentacao Agropecuaria) - Universidade Federal de Lavras, Lavras, 2015.
}


\examples{
data(DataCoffee) # categorized data set

data <- DataCoffee[,2:ncol(DataCoffee)] 

rownames(data) <- as.character(t(DataCoffee[1:nrow(DataCoffee),1]))

group.names = c("Coffee A", "Coffee B", "Coffee C", "Coffee D")

mf <- MFA(data, c(16,16,16,16), c(rep("f",4)), group.names) 

print("Principal components variances:"); round(mf$mtxA,2)

print("Matrix of the Partial Inertia / Score of the Variables:"); round(mf$mtxEV,2)

tit <- c("Scree-plot","Individuals","Individuals / Types coffees","Inercias Groups")

Plot.MFA(mf, titles = tit, xlabel = NA, ylabel = NA,
         posleg = 2, boxleg = FALSE, color = TRUE, 
         namarr = FALSE, linlab = NA, casc = FALSE) # plotting several graphs on the screen

}

\keyword{Data set}

