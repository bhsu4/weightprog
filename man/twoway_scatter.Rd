\name{twoway_scatter}
\alias{twoway_scatter}
\alias{twoway_scatterg}
\alias{twoway_scatterc}
\title{Two Way Scatter with Objective Variable}
\description{
Two way scatter plot by the objective variable with the data points' color and size denoting the attribute of an objective variable. twoway_scatterg() subgroups an objective variable's attribute, and twoway_scatterc() does not subgroup the objective variable.
}
\usage{
twoway_scatterg(data, xname, yname,
                bysize, bycolor, xlabel, ylabel,
                guidesize, guidecolor)
twoway_scatterc(data, xname, yname,
                bycolor, shape, size,
                xlabel, ylabel, guidecolor)
}
\arguments{
\item{data}{Dataset to use for plot, must be in data.frame to plot.}
\item{xname}{Consists of specification of x variable.}
\item{yname}{Consists of specification of y variable.}
\item{bysize}{Consists of specificiation of sorting variable by size.}
\item{bycolor}{Consists of specificiation of sorting variable by color.}
\item{xlabel}{Name for x axis labels.}
\item{ylabel}{Name for y axis labels.}
\item{shape}{Includes shape of data points (numerical value).}
\item{size}{Includes size of data points (numerical value).}
\item{guidesize}{Guide title for size.}
\item{guidecolor}{Guide title for color.}
}
\details{
twoway_scatterg returns a ggplot of the two variables, along with the utilization of color and size of a subgroup of the objective variables (linear model is fit through the subgroups). twoway_scatterc returns a ggplot of two variables, with a sequential palette of an objective variable denoting specific attributes.
}

\examples{
example <- iris
example$obj <- example[,1]-example[,2]
twoway_scatterg(example, example$Sepal.Length, example$Petal.Length,
                example$obj, example$Species, "Sepal Length", "Petal Length",
                "Petal Length", "Species")
twoway_scatterc(example, example$Sepal.Length, example$Petal.Length,
                example$obj, 16, 3, "Sepal Length", "Petal Length",
                "Sepal and Petal Length Diff")
}
