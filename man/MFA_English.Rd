\name{MFA}

\alias{MFA}

\title{Multiple Factor Analysis (MFA).}

\description{Perform Multiple Factor Analysis (MFA) on groups of variables. The groups of variables can be quantitative, qualitative, frequency (MFACT) data, or mixed data.}

\usage{MFA(Data, Groups, TypeGroups = rep("n",length(Groups)), NameGroups = NULL)}

\arguments{
  \item{Data}{Data to be analyzed.}
  \item{Groups}{Number of columns for each group in order following the order of data in 'Data'.}
  \item{TypeGroups}{Type of group:\cr
                    "n" for numerical data (default),\cr
                    "c" for categorical data,\cr
                    "f" for frequency data.}
  \item{NameGroups}{Names for each group.}
}

\value{
  \item{VectorG}{Vector with the sizes of each group.}
  \item{VectorNG}{Vector with the names of each group.}
  \item{VectorPLin}{Vector with the values used to balance the lines of the Z matrix.}
  \item{VectorPCol}{Vector with the values used to balance the columns of the Z matrix.}
  \item{MatrixZ}{Matrix concatenated and balanced.}
  \item{MatrixA}{Matrix of the eigenvalues (variances) with the proportions and proportions accumulated.}
  \item{MatrixU}{Matrix U of the singular decomposition of the matrix Z.}
  \item{MatrixV}{Matrix V of the singular decomposition of the matrix Z.}
  \item{MatrixF}{Matrix global factor scores where the lines are the observations and the columns the components.}
  \item{MatrixEFG}{Matrix of the factor scores by group.}
  \item{MatrixCCP}{Matrix of the correlation of the principal components with original variables.}
  \item{MatrixEscVar}{Matrix of the partial inertias / scores of the variables}
}

\references{
ABDESSEMED, L.; ESCOFIER, B. Analyse factorielle multiple de tableaux de frequencies: comparaison avec l'analyse canonique des correspondences. \emph{Journal de la Societe de Statistique de Paris}, Paris, v. 137, n. 2, p. 3-18, 1996..

ABDI, H. Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 907-912.

ABDI, H.; VALENTIN, D. Multiple factor analysis (MFA). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 657-663.

ABDI, H.; WILLIAMS, L. Principal component analysis. \emph{WIREs Computational Statatistics}, New York, v. 2, n. 4, p. 433-459, July/Aug. 2010.

ABDI, H.; WILLIAMS, L.; VALENTIN, D. Multiple factor analysis: principal component analysis for multitable and multiblock data sets. \emph{WIREs Computational Statatistics}, New York, v. 5, n. 2, p. 149-179, Feb. 2013.

BECUE-BERTAUT, M.; PAGES, J. A principal axes method for comparing contingency tables: MFACT. \emph{Computational Statistics & Data Analysis}, New York, v. 45, n. 3, p. 481-503, Feb. 2004

BECUE-BERTAUT, M.; PAGES, J. Multiple factor analysis and clustering of a mixture of quantitative, categorical and frequency data. \emph{Computational Statistics & Data Analysis}, New York, v. 52, n. 6, p. 3255-3268, Feb. 2008.

BENZECRI, J. Analyse de l'inertie intraclasse par l'analyse d'un tableau de contingence: intra-classinertia analysis through the analysis of a contingency table. \emph{Les Cahiers de l'Analyse des Donnees}, Paris, v. 8, n. 3, p. 351-358, 1983.

ESCOFIER, B. Analyse factorielle en reference a un modele: application a l'analyse d'un tableau d'echanges. \emph{Revue de Statistique Appliquee}, Paris, v. 32, n. 4, p. 25-36, 1984.

ESCOFIER, B.; DROUET, D. Analyse des differences entre plusieurs tableaux de frequence. \emph{Les Cahiers de l'Analyse des Donnees}, Paris, v. 8, n. 4, p. 491-499, 1983.

ESCOFIER, B.; PAGES, J. \emph{Analyse factorielles simples et multiples.} Paris: Dunod, 1990. 267 p.

ESCOFIER, B.; PAGES, J. \emph{Analyses factorielles simples et multiples:} objectifs, methodes et interpretation. 4th ed. Paris: Dunod, 2008. 318 p.

ESCOFIER, B.; PAGES, J. \emph{Comparaison de groupes de variables definies sur le meme ensemble d'individus:} un exemple d'applications. Le Chesnay: Institut National de Recherche en Informatique et en Automatique, 1982. 121 p.

ESCOFIER, B.; PAGES, J. Multiple factor analysis (AFUMULT package). \emph{Computational Statistics & Data Analysis}, New York, v. 18, n. 1, p. 121-140, Aug. 1994

GREENACRE, M.; BLASIUS, J. \emph{Multiple correspondence analysis and related methods.} New York: Taylor and Francis, 2006. 607 p.

OSSANI, P. C.; CIRILLO, M. A.; BOREM, F. M.; RIBEIRO, D. E.; CORTEZ, R. M.. Quality of specialty coffees: a sensory evaluation by consumers using the MFACT technique. \emph{Revista Ciencia Agronomica (UFC. Online)}, v. 48, p. 92-100, 2017.

PAGES, J. Analyse factorielle multiple appliquee aux variables qualitatives et aux donnees mixtes. \emph{Revue de Statistique Appliquee}, Paris, v. 50, n. 4, p. 5-37, 2002.

PAGES, J.. Multiple factor analysis: main features and application to sensory data. \emph{Revista Colombiana de Estadistica}, Bogota, v. 27, n. 1, p. 1-26, 2004.

}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.MFA}}}

\examples{
data(DataMix) # mixed dataset

Data <- DataMix[,2:ncol(DataMix)] 

rownames(Data) <- DataMix[1:nrow(DataMix),1]

GroupNames = c("Grade Cafes/Work", "Formation/Dedication", "Coffees")

MF <- MFA(Data, c(2,2,2), TypeGroups = c("n","c","f"), GroupNames) # performs MFA

print("Principal Component Variances:"); round(MF$MatrixA,2)

print("Matrix of the Partial Inertia / Score of the Variables:"); round(MF$MatrixEscVar,2)
}

\keyword{Multiple Factor Analysis}
\keyword{MFA}
\keyword{MFACT}