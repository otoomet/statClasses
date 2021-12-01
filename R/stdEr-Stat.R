#' @export
setMethod("stdEr", "Stat", function(x) sqrt(diag(vcov(x))))
