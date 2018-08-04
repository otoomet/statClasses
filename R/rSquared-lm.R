### rSquared method for 'lm'

setMethod("rSquared", "lm", function(object, ...) {
   summary(object)$r.squared
})
