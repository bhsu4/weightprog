\name{twoway_lda}
\alias{twoway_lda}
\title{Two Way Linear Discriminant Analysis of Objective Variable}
\description{Two way linear discriminant analysis of the objective variable finds the linear combination of the original variables chosen that give the best possible classification. The best classification is defined as groups that are most distinctly separated from one another. twoway_lda() allows a visualization of how each of the definitive subgroups of our objective variables are separated through both histograms and scatterplots.
}
\usage{
twoway_lda(main, avar, bvar, cvar, dvar, data, bylabel)
}
\arguments{
\item{main}{Chosen variable with subgroups (objective variable recommended).}
\item{avar}{First variable for the discriminant function.}
\item{bvar}{Second variable for the discriminant function.}
\item{cvar}{Third variable for the discriminant function.}
\item{dvar}{Fourth variable for the discriminant function.}
\item{data}{Dataset for the variables above.}
\item{bylabel}{Label for scatterplot of discriminant function data points (objective variable subgroups.)}
}
\details{
twoway_lda() returns a scatterplot of the top two discriminant functions from linear combinations, with the utilization of the objective variable's subgroups as labels to see how well each subgroup is separated. Along with the scatterplot, the histogram for the top two discriminant functions can be seen.
}
\examples{
example <- iris
twoway_lda(example$Species, example$Sepal.Length,
           example$Petal.Length, example$Sepal.Width,
           example$Petal.Width, example, example$Species)
}
