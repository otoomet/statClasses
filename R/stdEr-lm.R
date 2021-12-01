#' @importFrom miscTools nObs nParam stdEr
setMethod("stdEr", "lm", function(x, ...) sqrt(diag(vcov(x))))
