
auxMat <- function(auxList) {
   ##
   auxFormat <- function(dat) {
      if(inherits(dat, "integer"))
         formatCNA(dat, format="d")
      else if(inherits(dat, "numeric"))
         formatCNA(dat, format="f", width=3)
      else
         as.character(dat)
   }
   ##
   aList <- lapply(auxList,
                  function(aux) sapply(aux, auxFormat)
                  )
   aMat <- data.frame(names=character())
   for(i in seq(along=aList)) {
      aVec <- aList[[i]]
      aFrame <- data.frame(names=names(aVec), val=aVec, stringsAsFactors=FALSE)
      if(length(names(aFrame)) > 1) {
         names(aFrame)[2] <- names(aList)[i]
      }
      aMat <- merge(aMat, aFrame, all=TRUE)
   }
   names <- aMat$names
   aMat <- as.matrix(aMat[,-1, drop=FALSE])
   row.names(aMat) <- names
   aMat
}

