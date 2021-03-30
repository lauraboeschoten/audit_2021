# functie om een loglineair model te schatten
fitmodel <- function(model, tab) {
  M <- glm(formula = model, data = tab, family = poisson)
  G2 <- M$deviance
  Resdf <- M$df.residual
  fit <- M$fitted.values
  p <- pchisq(G2, df = Resdf, lower.tail = FALSE)
  return(list(G2 = G2, df = Resdf, p = p, fit = fit))
}