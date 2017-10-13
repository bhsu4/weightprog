twoway_kmeans <- function(data, kmax, xname, yname, xvar, yvar, byobj, xlabel, ylabel){

    res.kmeans <- lapply(1:kmax, function(i) {
    kmeans(data[,c(xname, yname)], centers = i)
  })
  cluster.colors <- lapply(res.kmeans, function(x) x$cluster)

  l_ply(cluster.colors,
        function(colors) {
            gg.k <- ggplot(data, aes(xvar, yvar, col = colors)) +
            geom_point() + labs(title = paste(nlevels(factor(colors)), "Cluster(s)")) +
            geom_text(aes(label=byobj),hjust=0, vjust=0, cex=3) +
            xlab(xlabel) + ylab(ylabel)
          print(gg.k)

        })
}
