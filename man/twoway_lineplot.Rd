\name{twoway_lineplot}
\alias{twoway_lineplot}
\alias{twoway_lineplotf}
\title{Two Way Lineplot}
\description{
Two way lineplot with variable(s) of interest over time period. twoway_lineplot() simply creates a lineplot of a variable progression over chosen time period. twoway_lineplotf() facets by a variable so that the objective variable is isolated for observation in each characteristic.
}
\usage{
twoway_lineplot(data, date, yname, ylabel)
twoway_lineplotf(data, xname, yname, bycolor, bywrap, xlabel, ylabel)
}
\arguments{
\item{data}{Dataset to use for plot, must be in data.frame to plot.}
\item{date}{Date or time period considered, has to be in format of "month/date/year"}
\item{xname}{Consists of x variable of interest.}
\item{yname}{Consists of y variable of interest.}
\item{bywrap}{Chosen variable to facet by in order to display by each characteristic}
\item{bycolor}{Chosen variable for different lines within facet's characteristics (recommend objective variable)}
\item{xlabel}{Name for x axis labels.}
\item{ylabel}{Name for y axis labels.}
}

\examples{
example <- iris
example$date <- seq(as.Date("1910/1/1"), as.Date("1910/5/30"), "days")
twoway_lineplot(example, date, example$Sepal.Length, "Sepal Length")
}
