\name{twoway_strip}
\alias{twoway_strip}
\title{Two Way Strip Plot of Groups}
\description{Two way strip plot of a variable of interest and a grouped variable with data points' color denoting the specific groups (i.e. day of week or species). The mean will be displayed, with lineplot running through the means of each different group.
}
\usage{
twoway_strip(data, xname, yname,
             bysize, bycolor, xlabel, ylabel)
}
\arguments{
\item{data}{Dataset to use for plot, must be in data.frame to plot.}
\item{xname}{Consists of specification of x variable.}
\item{yname}{Consists of specification of y variable.}
\item{bysize}{Consists of specificiation of sorting variable by size.}
\item{bycolor}{Consists of specificiation of sorting variable by color.}
\item{xlabel}{Name for x axis labels.}
\item{ylabel}{Name for y axis labels.}
}
\details{
twoway_strip is similar to twoway_scatter, both utilizing the exact same arguments. The strip plot is specifically useful for looking at grouped variables (i.e. time, species).
}
\examples{
example <- iris
day <- c("Mon", "Tues", "Wed")
example$time <- rep_len(day, length.out=length(example))
twoway_strip(example, example$time, example$Petal.Length,
       " ", example$time, "Day of Week", "Petal Length")
twoway_strip(example, example$Species, example$Petal.Length,
        " ", example$Species, "Species", "Petal Length")
}
