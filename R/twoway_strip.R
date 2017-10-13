twoway_strip <- function(data, xname, yname, bysize, bycolor, xlabel, ylabel) {
  ggplot(data, aes(x = xname, y = yname, color = bycolor)) +
    geom_jitter(position=position_jitter(0)) +
    stat_summary(fun.y=mean, geom="line", aes(group = 1), size=0.25, color="grey40") +
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black") +
    xlab(xlabel) +
    ylab(ylabel) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)) +
    theme(legend.position="none")
}
