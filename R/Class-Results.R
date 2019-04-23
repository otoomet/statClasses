### class 'Results' keeps various numeric vectors

#' @importFrom methods "new" "show" "slot" "slot<-" "slotNames" "validObject"

#' @export
setClass("Results",
         representation(coefficients = "numeric",
                           # should be named
                        ## The following slots are for specific
                        ## purposes (like # of obs)
                        auxiliary = "list",
                        description = "character"
                           # name or description of data
                        ))
