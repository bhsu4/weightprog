\name{twoway_kmeans}
\alias{twoway_kmeans}
\title{Two Way K-Means Clustering with Objective Variable}
\description{Two way k-means with objective variable creates 1 to a chosen "k" k-mean clusters with each centroid featured by a different mean between the x and y variable chosen. twoway_kmeans() allows an easy observation of how the clusters are created, and how the subgroups of the objective variable is assigned to each cluster. If the data does have a specific trend that is subsequently clustered by different means for each groups, the k-means is an accurate tool to allow for observations to choose how many clusters to use, and what each would look like.}
\usage{
twoway_kmeans(xname, yname, kmax, xlabel, ylabel, guidecolor)
}
\arguments{
\item{xname}{Chosen x variable's name for k-means.}
\item{yname}{Chosen y variable's name for k-means.}
\item{kmax}{Choosing how many k-means clusters to create (numeric value)}
\item{xlabel}{Name for x axis labels.}
\item{ylabel}{Name for y axis labels.}
\item{guidecolor}{Guide title for color of clusters.}
}
\details{
twoway_kmeans() returns a ggplot of the k different created k-means clustering, with the utilization of the objective variable's subgroups as labels to see how each subgroup is represented in clusters.
}
\examples{
example <- iris
twoway_kmeans(example$Sepal.Length, example$Petal.Length, 3,
              "Sepal.Length", "Petal.Length", "Cluster by Color")
}
