
setMethod("stdEr", "nls", function(x, ...) sqrt(diag(vcov(x))))
