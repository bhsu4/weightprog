twoway_lineplot <- function(data, date, yname, ylabel){
  ggplot(data, aes(x = date, y = yname)) +
    geom_line(col="black") +
    xlab("") +
    ylab(ylabel) +
    theme_bw()
}

twoway_lineplotf <- function(data, xname, yname, bycolor, bywrap, xlabel, ylabel) {
  p = ggplot(data, aes_string(x=xname, y=yname, colour=bycolor)) +
    geom_line() +
    facet_wrap(as.formula(paste("~", bywrap)), nrow = 2, scales="free") +
    xlab(xlabel) +
    ylab(ylabel)
  return(p)
}
