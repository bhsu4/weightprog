twoway_scatterg <- function(data, xname, yname, bysize, bycolor, xlabel, ylabel, guidesize, guidecolor) {
  ggplot(data,aes(x=xname,y=yname)) +
    geom_point(aes(size=bysize, fill=bycolor), shape=21,alpha=.8) +
    geom_smooth(aes(col=bycolor), method="lm", se=F) +
    guides(size = guide_legend(title = guidesize, title.position = "top")) +
    guides(col = guide_legend(title = guidecolor, title.position = "top")) +
    guides(fill = guide_legend(title = guidecolor, title.position = "top")) +
    xlab(xlabel) +
    ylab(ylabel) + theme_bw()
}

twoway_scatterc <- function(data, xname, yname, bycolor, shape, size, xlabel, ylabel, guidecolor) {
  ggplot(data, aes(x = xname, y = yname, color = bycolor)) +
    geom_point(shape = shape, size = size, show.legend = TRUE) +
    geom_rug() +
    theme_bw() +
    scale_color_gradient(high = "#E51300", low = "#3BBF00") +
    guides(color = guide_legend(title = guidecolor, title.position = "left")) +
    xlab(xlabel) +
    ylab(ylabel) +
    theme(legend.position="top")
}
