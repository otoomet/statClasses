
epsFName <- function(fn) {
   ## return file name in the suitable form for saveing graphs
    if(Sys.getenv("HOSTNAME") %in% c("oliver", "maria", "julie")) {
        fn <- paste(Sys.getenv("HOME"), "/udbakke/", fn, ".eps", sep="")
    }
    else
        fn <- paste(fn, ".eps", sep="")
   return(fn)
 }
