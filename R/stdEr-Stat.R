#' @export
setMethod("stdEr", "Stat", function(object) sqrt(diag(vcov(object))))
