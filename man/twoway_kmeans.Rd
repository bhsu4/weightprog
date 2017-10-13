\name{twoway_kmeans}
\alias{twoway_kmeans}
\title{Two Way K-Means Clustering with Objective Variable}
\description{Two way k-means with objective variable creates 1 to a chosen "k" k-mean clusters with each centroid featured by a different mean between the x and y variable chosen. twoway_kmeans() allows an easy observation of how the clusters are created, and how the subgroups of the objective variable is assigned to each cluster. If the data does have a specific trend that is subsequently clustered by different means for each groups, the k-means is an accurate tool to allow for observations to choose how many clusters to use, and what each would look like.}
\usage{
twoway_kmeans(data, kmax, xname, yname,
              xvar, yvar, byobj, xlabel, ylabel)
}
\arguments{
\item{data}{Dataset to use for plot, must be in data.frame to plot.}
\item{kmax}{Choosing how many k-means clusters to create (numeric value)}
\item{xname}{Chosen x variable's name for k-means.}
\item{yname}{Chosen y variable's name for k-means.}
\item{xvar}{Consists of x variable for plotting.}
\item{yvar}{Consists of y variable for plotting.}
\item{byobj}{Consists of text beside each data point (recommend objective variable).}
\item{xlabel}{Name for x axis labels.}
\item{ylabel}{Name for y axis labels.}
}
\details{
twoway_kmeans() returns a ggplot of the k different created k-means clustering, with the utilization of the objective variable's subgroups as labels to see how each subgroup is represented in clusters.
}
\examples{
example <- iris
twoway_kmeans(iris, 3, "Sepal.Length", "Petal.Length",
              iris$Sepal.Length, iris$Petal.Length,
              iris$Species, "Sepal.Length", "Petal.Length")
}
