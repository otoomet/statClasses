
setMethod("stdEr", "nls", function(object, ...) sqrt(diag(vcov(object))))
