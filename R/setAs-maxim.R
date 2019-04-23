#' @export
maximToEstimates <- function(from, to="Estimates", vCov=vcov) {
   ## transforms 'lm' to estimates allowing to specify a specific,
   ## such as HC-consistent var-covar matrix
   new(to,
       coefficients=coef(from),
       stdEr=rep(NA_real_, length(coef(from))),
       auxiliary = list()
       )
}
setAs("maxim", "Estimates", function(from) maximToEstimates(from))

