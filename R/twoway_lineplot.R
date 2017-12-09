twoway_lineplot <- function(data, date, yname, xlabel, ylabel, ymean, ymlabel, guidecolor){
    ggplot(data) +
    geom_line(aes(x = date, y = yname, color = ylabel)) +
    geom_point(aes(x = date, y = ymean, color = ymlabel)) +
    xlab(xlabel) + ylab(ylabel) + theme_bw() +
    guides(color = guide_legend(title = guidecolor, title.position = "top"))
}


twoway_lineplotf <- function(data, xname, yname, bycolor, bywrap, xlabel, ylabel) {
  p = ggplot(data, aes_string(x=xname, y=yname, colour=bycolor)) +
    geom_point() +
    geom_smooth(method='lm', se=FALSE) +
    facet_wrap(as.formula(paste("~", bywrap)), nrow = 2, scales="free") +
    xlab(xlabel) +
    ylab(ylabel)
  return(p)
}


