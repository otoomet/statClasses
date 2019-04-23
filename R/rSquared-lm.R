### rSquared method for 'lm'
#' @export
setMethod("rSquared", "lm", function(object, ...) {
   summary(object)$r.squared
})
