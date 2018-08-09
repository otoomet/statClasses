
setMethod("stdEr", "lm", function(object, ...) sqrt(diag(vcov(object))))
