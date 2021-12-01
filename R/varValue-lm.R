
## varValue: extract values of identical variables from a series of models
## Return it as numeric matrix
setGeneric("varValue",
           function(x, ...) {
              res <- standardGeneric("varValue")
              res
           }
           )

varValue.lm <- function(x, var="^Estonian", group) {
   ## extract coefs of certain variables from models
   ## group: for compatibility with 'separate'
   coef <- coef(x)
   i <- grep(var, names(coef))
   if(length(i) == 0)
       stop("no such variable")
   iv <- grep(var, names(coef[!is.na(coef)]))
   new("Stat",
       coefficients = coef[i],
       vcov = methods::as(vcov(x)[iv,iv], "matrix")
       )
}
setMethod("varValue", "lm", varValue.lm)
rm(varValue.lm)
