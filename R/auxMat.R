
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
   aMat <- lapply(auxList
                  function(aux) sapply(aux, auxFormat)
                  )
   
}

