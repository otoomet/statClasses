
## show stat objects
#' @export
show.Estimates <- function(object) {
   ## print an object with methods 'coef' and 'sd'
   t <- abs(coef(object)/stdEr(object))
   mat <- cbind("Estimate"=coef(object), "Std. Error"=stdEr(object),
                      "t value"=t, "Pr(>|t|)"=2*stats::pnorm(-t))
   row.names(mat) <- names(coef(object))
   stats::printCoefmat(mat, cs.ind=1:2, tst.ind=3, P.values=TRUE)
}
setMethod("show", "Estimates", show.Estimates)
setMethod("show", "Stat", show.Estimates)
