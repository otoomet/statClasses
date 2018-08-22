
nlsToEstimates <- function(from, to="Estimates", vCov=vcov) {
   ## transforms 'lm' to estimates allowing to specify a specific,
   ## such as HC-consistent var-covar matrix
   new(to,
       coefficients=coef(from),
       stdEr=stdEr(from),
       auxiliary = list()
       )
}
setAs("nls", "Estimates", function(from) nlsToEstimates(from))

