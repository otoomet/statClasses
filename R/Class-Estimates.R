
### class 'Estimates': vectors with standard errors
### Designed for various estimated vectors which have
### corresponding standard errors

#' @export
setClass("Estimates",
         representation("Results",
                        stdEr = "numeric"
                        ))
