#' @export
lmToEstimates <- function(from, to="Estimates", vCov=vcov) {
   ## transforms 'lm' to estimates allowing to specify a specific,
   ## such as HC-consistent var-covar matrix
   new(to,
       coefficients=coef(from),
       stdEr=stdEr(from, vCov=vCov),
       auxiliary = list(nObs=nObs(from), rSquared=rSquared(from))
       )
}
setAs("lm", "Estimates", function(from) lmToEstimates(from))
