## formatC with custom NA string

#' @export
formatCNA <- function(x, naStr="", ...) {
   ifelse(!is.na(x), formatC(x, ...), naStr)
}
