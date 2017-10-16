twoway_lda <- function(main, avar, bvar, cvar, dvar, data, bylabel){
  weight.lda <- lda(main ~ avar + bvar + cvar + dvar, data)
  weight.lda.values <- predict(weight.lda)
  lda1 <- ldahist(data = weight.lda.values$x[,1], g = bylabel)
  lda2 <- ldahist(data = weight.lda.values$x[,2], g = bylabel)
  lda.dat <- data.frame(cbind(weight.lda.values$x[,1], weight.lda.values$x[,2]))
  lda3 <- ggplot(lda.dat, aes_string(x = "X1", y = "X2")) +
    geom_point() + geom_text(aes(label = bylabel)) +
    xlab("LDA (Linear Combination #1)") +
    ylab("LDA (Linear Combination #2)") +
    theme_bw()
  lda1
  lda2
  lda3
}
