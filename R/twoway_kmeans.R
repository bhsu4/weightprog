twoway_kmeans <- function(xname, yname, kmax, xlabel, ylabel, guidecolor) {
  data.km <- cbind(x1 = xname, x2 = yname)
  kclusts <- data.frame(kmax=1:kmax) %>% group_by(kmax) %>% do(kclust=kmeans(data.km, .$kmax))
  #do kmeans on data for each k-number of clusters
  assignments <- kclusts %>% group_by(kmax) %>% do(augment(.$kclust[[1]], data.km))

  twkm <- ggplot(assignments, aes(x1, x2)) + geom_point(aes(color=.cluster)) +
    facet_wrap(~ kmax) + xlab(xlabel) + ylab(ylabel) +
    guides(color = guide_legend(title = guidecolor, title.position = "left", nrow=1)) +
    theme(legend.position="bottom")
  twkm
}
