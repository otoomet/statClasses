
grSetup <- function(N, eps, pdf, fName, lwd=1, rightScale=FALSE,
                    cex=NULL) {
   ## Sets up the graphic device.  Should used as follows:
   ##
   ## opar <- grSetup(NGraphs, eps, pdf, fName, lwd)
   ## if(eps|pdf)
   ##     on.exit({par(opar); dev.off()}, add=TRUE)
   ##
   ## N         how many graph panels
   ## eps, pdf  T/F - whether to output eps/pdf
   ## lwd       lwd set by (par), mainly for axes etc.
   ## rightScale  should a space left for axis + label at right?
   ##
   if(rightScale)
       mar4 <- 3
   else
       mar4 <- 0
   if(eps|pdf) {
      ## dimensions twice as big because otherwise the lines are too fat
      width <- 116/25.4
      if(N == 2) {
         height=55/25.4
      }
      else {
         height <- 80/25.4
      }
      if(eps)
          postscript(file=epsFName(fName),
                     onefile=FALSE, horizontal=FALSE,
                     paper="special", width=width, height=height)
      else
          pdf(file=pdfFName(fName),
              onefile=FALSE, width=width, height=height)
      cex <- ifelse(is.null(cex), 0.6, cex)
      if(N == 2) {
         opar <- par(mar=c(3,3,0, mar4 - 0.1) + 0.1,
                     mfcol=c(1,2),
                     mgp=c(2,1,0),
                     lwd=lwd[1],
                     cex=0.8*cex)
      }
      else {
         opar <- par(mar=c(3,3,0, mar4) + 0.1,
                     mfcol=c(1,1),
                     mgp=c(2,1,0),
                     lwd=lwd[1],
                     cex=cex)
      }
   }
   else {
                                        # plot on screen
      cex <- ifelse(is.null(cex), 0.9, cex)
      if(N == 2) {
         opar <- par(mar=c(3, 3 ,0, mar4 - 0.1) + 0.3,
                     mfcol=c(1,2),
                     mgp=c(2,1,0),
                     lwd=lwd[1],
                     cex=cex)
      }
      else {
         opar <- par(mar=c(3,3,0, mar4) + 0.3,
                     mex=10,
                     mfcol=c(1,1),
                     mgp=c(2,1,0),
                     lwd=lwd[1],
                     cex=cex)
      }
   }
   opar
}
