#' @importFrom miscTools nObs nParam stdEr
setMethod("stdEr", "lm", function(object, ...) sqrt(diag(vcov(object))))
