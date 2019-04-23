
## show tables on screen
#' @export
show.CoefTable <- function(object) {
   tt <- matrix("", nrow(object@table), ncol(object@table))
                           # matrix of formatted coefficients and significance marks
   dimnames(tt) <- dimnames(object@table)
   iCoef <- object@iCoef
   iStdd <- object@iStdd
   jCoef <- object@jCoef
   jStdd <- object@jStdd
   jStar <- object@jStar
   tt[iCoef,jCoef] <- formatCNA(object@table[iCoef, jCoef], width=6, digits=3, format="f")
   tt[iStdd,jStdd] <- formatCNA(object@table[iStdd, jStdd], width=6, digits=3, format="f")
   tt[iCoef,jStar] <- as.character(cut(object@table[iCoef, jStar], 
                                       breaks=qnorm(c(0, 0.95, 0.975, 0.995, 0.9995, 1)),
                                       labels=c("", ".", "*", "**", "***"),
                                       right=FALSE))
   tt[is.na(tt)] <- ""
   aMat <- auxMat(object@auxiliary)
   aTt <- matrix("", nrow(aMat), ncol(tt))
   row.names(aTt) <- row.names(aMat)
   aTt[,jCoef] <- aMat
   tt <- rbind(tt, aTt)
   print(tt, quote=FALSE)
}
setMethod("show", "CoefTable", show.CoefTable)
rm(show.CoefTable)
