
xtable.CoefTable <- function(object, sd=TRUE,
                             ...) {
   library(xtable)
   tt <- matrix("", nrow(object@table), ncol(object@table))
   dimnames(tt) <- dimnames(object@table)
   colnames(tt)[colnames(tt) == ""] <- "."
   iCoef <- object@iCoef
   iStdd <- object@iStdd
   jCoef <- object@jCoef
   jStdd <- object@jStdd
   jStar <- object@jStar
   tt[iCoef, jCoef] <- formatCNA(object@table[iCoef, jCoef], width=6, digits=3, format="f")
   tt[iStdd, jStdd] <- paste("\\std{", formatCNA(object@table[iStdd, jStdd], width=6, digits=3, format="f"), "}", sep="")
   tt[iCoef,jStar] <- as.character(cut(object@table[iCoef, jStar], 
                                       breaks=qnorm(c(0, 0.95, 0.975, 0.995, 1)),
                                       labels=c("", "*", "**", "***"),
                                       right=FALSE))
   tt[is.na(tt)] <- ""
   if(!sd) {
      if(all(iCoef == iStdd)) {
                           # wide format
         tt <- tt[,-jStdd]
      }
      else {
         tt <- tt[-jCoef,]
      }
   }
   align <- if(all(iCoef == iStdd))
                           # wide
               c("r", "l", rep(c("r", "r@{}", "l"), length(jCoef)))
            else
                           # long
               c("r", "l", rep(c("r@{}", "l"), length(jCoef)))
   ## make aux matrix
   if(!is.null(object@auxiliary)) {
      aMat <- auxMat(object@auxiliary)
      aTt <- matrix("", nrow(aMat), ncol(tt))
      row.names(aTt) <- row.names(aMat)
      aTt[,jCoef] <- aMat
      tt <- rbind(tt, aTt)
   }
   ##
   tt <- cbind("."=row.names(tt), tt)
   row.names(tt) <- NULL
   xt <- xtable::xtable(tt, align=align)
   xt
}
setMethod("xtable", "CoefTable", xtable.CoefTable)
