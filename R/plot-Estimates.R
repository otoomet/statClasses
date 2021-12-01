
plot.Estimates <- function(x, y, ..., conf.int=FALSE,
                      xlab="",
                           ylab,
                           legend=names(objects),
                      ex=seq(along=estimate),
                      type="p", pch=1:nStat,
                           lwd=1, cex=1, col=1, lty=1,
                           xlim=NULL, ylim=NULL,
                           log="",
                           colS=col, shade=FALSE, density=10,
                           jitter=FALSE,
                           smooth=NULL,
                           xNames=nStat==1,
                           xaxt="s",
                           eps=FALSE, pdf=FALSE,
                           fName="plot",
                           add=FALSE
                           ) {
   ##
   ## colS      (transpartent) colors for confidence intervals
   ## jitter    whether to jitter x-values a bit (useful if plotting
   ##           several statistics on a single plot)
   ## xNames    whether to add individual names to the x-axis as
   ##           labels
   ## smooth    type of smoothing line to add to the graph (or NULL
   ##           for none).  'lowess' for lowess smoother
   ##
   if(!is(x, "Estimates")) {
      ex <- x
   }
   else {
      ex <- seq(along=coef(x))
   }
   if(type == "s") {
      ## for step plots, we add one more interval
      ex <- c(ex, tail(ex, 1) + tail(diff(ex), 1))
   }
   objects <- list()
   if(missing(y)) {
      l <- list(x, ...)
   }
   else {
      l <- list(x, y, ...)
   }
   for(d in l) {
      if(!is(d, "Estimates")) {
         next;
      }
      objects <- c(objects, d)
   }
   nStat <- length(objects)
                           # how many different statistics to plot
   if(missing(ylab)) {
      if(inherits(x, "Estimates"))
          ylab <- x@description
      else
          ylab <- ""
   }
   if(is.null(ylim)) {
      for(obj in objects) {
         estimate <- coef(obj)
         stde <- stats::sd(obj)
         ylim <- range(ylim, estimate+1.96*stde, estimate-1.96*stde,
                       na.rm=TRUE)
      }
   }
   lty <- rep(lty, length.out=length(objects))
   lwd <- rep(lwd, length.out=length(objects))
   col <- rep(col, length.out=length(objects))
   cex <- rep(cex, length.out=length(objects))
   pch <- rep(pch, length.out=length(objects))
   if(type == "s")
       pch <- NULL
   opar <- grSetup(1, eps, pdf, fName, lwd=lwd, cex=cex)
   if(eps|pdf)
       on.exit( { par(opar); dev.off() }, add=TRUE)
   angle <- c(45,135)
   if(!shade) {
      colS <- rgb(t(col2rgb(colS))/255, alpha=0.1)
                           # semi-transparent color for intervals
      density <- -density
   }
   if(!add) {
      plot(ex, rep(1, length(ex)),
           ylim=ylim, xlim=xlim, log=log,
           xaxt=xaxt, xlab=xlab, ylab=ylab, type="n"
           )
   }
   for(i in seq(along=objects)) {
      estimate <- coef(objects[[i]])
      stde <- stats::sd(objects[[i]])
      if(jitter) {
         exj <- jitter(ex, factor=0.3)
      }
      else
          exj <- ex
      if(type == "s") {
         estimate <- c(estimate, tail(estimate, 1))
         stde <- c(stde, tail(stde, 1))
      }
      lines(exj, estimate,
            pch=pch[i], col=col[i], lwd=lwd[i], lty=lty[i],
            type=type,
            cex=cex[i])
      if(!is.null(smooth)) {
         if(smooth=="lowess") {
            lines(stats::lowess(ex, estimate), lty=lty[i], col=col[i])
         }
         else {
            stop("wrong 'smooth'")
         }
      }
      if(conf.int) {
         if(type != "s") {
            order <- order(ex)
            rOrder <- length(ex) + 1 - order
            px <- c(exj[order], exj[rOrder])
            py <- c((estimate - 1.96*stde)[order],
                    (estimate+1.96*stde)[rOrder])
         }
         else {
            order <- rep(seq(along=ex), each=2)
            xOrder <- order[c(-1,-length(order))]
            yOrder <- utils::head(order, length(order)-2)
            rXOrder <- length(ex) + 1 - xOrder
            rYOrder <- seq(from=length(ex)-1, to=1)
            rYOrder <- rep(rYOrder, each=2)
            px <- c(exj[xOrder], exj[rXOrder])
            py <- c((estimate - 1.96*stde)[yOrder],
                    (estimate+1.96*stde)[rYOrder])
         }
         graphics::polygon(px, py,
                           density=density, col=colS[i], lty=3, angle=angle)
      }
   }
   if(!is.null(legend))
       legend("topleft", pch=pch, col=col, lwd=lwd, lty=lty,
              legend=legend, bty="n")
   ## add names on the x-axis
   if(xNames) {
      names <- names(coef(objects[[1]]))
      if(!is.null(names)) {
         graphics::mtext(names, at=seq(along=names), side=1, cex=cex, las=2)
      }
   }
}
setMethod("plot", signature(x="Estimates", y="missing"),
          plot.Estimates)
setMethod("plot", signature(x="numeric", y="Estimates"),
          plot.Estimates)
setMethod("plot", signature(x="POSIXlt", y="Estimates"),
          plot.Estimates)
rm(plot.Estimates)

plot.IntervalEstimates <- function(x, y, ..., type="s") {
   ex <- x@start
   xlab <- x@sequenceName
   if(missing(y)) {
      plot(ex, as(x, "Estimates"), xlab=xlab, type=type, xNames=FALSE, ...)
   }
   else {
      plot(ex, as(x, "Estimates"), y, ..., xlab=xlab, type=type, xNames=FALSE)
   }
}
setMethod("plot", signature(x="IntervalEstimates", y="missing"),
          plot.IntervalEstimates)
setMethod("plot", signature(x="IntervalEstimates", y="IntervalEstimates"),
          plot.IntervalEstimates)
rm(plot.IntervalEstimates)

plot.SequentialEstimates <- function(x, y, xlab=x@sequenceName,
                                     lwd=1, cex=0.8,
                                     eps=FALSE, pdf=FALSE,
                                     fName="Estimates",
                                     add=FALSE,
                                     ...) {
   ##
   opar <- grSetup(1, eps, pdf, fName, lwd=lwd, cex=cex)
   if(eps|pdf)
       on.exit( { par(opar); dev.off() }, add=TRUE)
   if(missing(y)) {
      plot(x@sequence, as(x, "Estimates"),
           xNames=FALSE, xaxt="n", xlab=xlab,
           lwd=lwd, cex=cex,
           eps=FALSE, pdf=FALSE,
           add=add,
           ...)
   }
   else {
      plot(x@sequence, as(x, "Estimates"), y,
           xNames=FALSE, xaxt="n", xlab=xlab,
           lwd=lwd, cex=cex,
           eps=FALSE, pdf=FALSE,
           add=add,
           ...)
   }
   if(!add) {
                           # add axis for the sequence, but only
                           # for fresh plots
      if(!is.null(names(x@sequence))) {
         axis(1, at=x@sequence, labels=names(x@sequence))
      }
      else {
         axis(1)
      }
   }
}
setMethod("plot", signature(x="SequentialEstimates", y="missing"),
          plot.SequentialEstimates)
setMethod("plot", signature(x="SequentialEstimates", y="SequentialEstimates"),
          plot.SequentialEstimates)
rm(plot.SequentialEstimates)
