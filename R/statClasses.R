
### Class definitions
library(methods)



### class 'Results' keeps various numeric vectors
setClass("Results",
         representation(coefficients = "numeric",
                           # should be named
                        ## The following slots are for specific
                        ## purposes (like # of obs)
                        auxiliary = "list",
                        description = "character"
                           # name or description of data
                        ))

### class 'Estimates' vectors with standard errors various estimated vectors which have
### corresponding standard errors
setClass("Estimates",
         representation("Results",
                        sd = "numeric"
                        ))

### class 'SequentialEstimates' vectors with standard errors
### various estimated vectors which have
### corresponding standard errors.
### The values correspond to an ordered sequence, which can
### be plotted in this way
setClass("SequentialEstimates",
         representation("Estimates",
                        sequence = "numeric",
                           # numeric sequence (like 1:N)
                           # eventual names used as labels for plotting
                           # the x-axis (like c("Mon", "Tue", ...))
                        sequenceName = "character"
                           # name of the sequence (like "years")
                        ),
         validity=function(object) {
            if(length(object@coefficients) != length(object@sequence)) {
               stop("'sequence' must be of the same length as 'coefficients'\n",
                    "currently ", length(object@sequence), " and ",
                    length(object@coefficients))
            }
         })

### class 'intervalEstimates' vectors with standard errors
### various estimated vectors which have
### corresponding standard errors.
### The values correspond to ordered intervals and can
### be plotted in this way
setClass("IntervalEstimates",
         representation("Estimates",
                        start = "numeric",
                           # start of the intervals
                        end = "numeric",
                        sequenceName = "character"
                        ))

### class 'Stat' results estimates with var-covar matrix
setClass("Stat",
         representation("Results",
                        vcov = "matrix"
                        ))

### Various tables.  Intended to merge several of objects into a single
### table
setClass("CoefTable",
         representation(table = "matrix",
                           # the table contents in a matrix
                        iCoef = "numeric",
                           # rows for coefficients
                        iStdd = "numeric",
                           # rows for standard errors
                        jCoef = "numeric",
                        jStdd = "numeric",
                        jStar = "numeric",
                           # rows for t-values
                        auxiliary = "matrix"
                           # matrix to keep nObs, R^2, etc.
                           # Row names are the names of this data
                        ))


### ------------------------------------------------
### Explain the inheritance of the old classes
### ------------------------------------------------

setOldClass(c("coef.intReg","numeric"))
setOldClass(c("regionDummy","intReg", "maxLik"))
setOldClass(c("lmc", "lm"))
setOldClass(c("Grid", "matrix"))

### -------------------------------------------------
### As
### -------------------------------------------------

setAs("Stat", "Estimates", function(from, to)
      new(to,
          coefficients=coef(from),
          sd=sd(from),
          auxiliary=from@auxiliary))
          
lmToEstimates <- function(from, to="Estimates", vCov=vcov) {
   ## transforms 'lm' to estimates allowing to specify a specific,
   ## such as HC-consistent var-covar matrix
   new(to,
       coefficients=coef(from),
       sd=sd(from, vCov=vCov),
       auxiliary = list(nObs=nObs(from), rSquared=rSquared(from))
       )
}
setAs("lm", "Estimates", function(from) lmToEstimates(from))
setAs("lmc", "Estimates", function(from) lmToEstimates(from))

maxLikToEstimates <- function(from, to="Estimates") {
   n <- try(nObs(from),
            silent=TRUE)
   if(inherits(n, "try-error")) {
      n <- NULL
   }
   new(to,
       coefficients=coef(from),
       sd=sd(from),
       auxiliary=list(nObs=n))
}
setAs("maxLik", "Estimates", maxLikToEstimates)
rm(maxLikToEstimates)

lmToStat <- function(from, to) {
   vcov <- matrix(0, nrow=length(coef(from)), ncol=length(coef(from)))
   vcov[] <- NA
   colnames(vcov) <- row.names(vcov) <- names(coef(from))
   vclm <- vcov(from)
   vcov[row.names(vclm), colnames(vclm)] <- vclm
   new(to,
       coefficients=coef(from),
       vcov=vcov,
       auxiliary = list(nObs=nObs(from), rSquared=rSquared(from))
       )
}
setAs("lm", "Stat", lmToStat)
setAs("lmc", "Stat", lmToStat)
rm(lmToStat)

setAs("maxLik", "Stat",
      function(from, to)
      new(to,
          coefficients=coef(from),
          vcov=as(vcov(from), "matrix"),
          auxiliary=list(nObs=nObs(from))
          ))

polrToEstimates <- function(from, to) {
   ## 'polr' (ordered) models from MASS
   ## the problem is that this class includes two types of coefficients:
   ## the real coefficients (returned by 'coef') and 'zetas'.
   ## vcov, however, is for all coefs.
   ## 
   allCoefs <- c(coef(from), from$zeta)
   new(to,
       coefficients=allCoefs,
       sd=sd(from),
       auxiliary = list(nObs=nObs(from), AIC=AIC(from))
       )
}
setAs("polr", "Estimates", polrToEstimates)
rm(polrToEstimates)

polrToStat <- function(from, to) {
   ## 'polr' (ordered) models from MASS
   ## the problem is that this class includes two types of coefficients:
   ## the real coefficients (returned by 'coef') and 'zetas'.
   ## vcov, however, is for all coefs.
   ## 
   allCoefs <- c(coef(from), from$zeta)
   vcov <- matrix(0, nrow=length(allCoefs), ncol=length(allCoefs))
   vcov[] <- NA
   colnames(vcov) <- row.names(vcov) <- names(allCoefs)
   vclm <- vcov(from)
   vcov[row.names(vclm), colnames(vclm)] <- vclm
   new(to,
       coefficients=allCoefs,
       vcov=vcov,
       auxiliary = list(nObs=nObs(from), AIC=AIC(from))
       )
}
setAs("polr", "Stat", polrToStat)
rm(polrToStat)

### -------------------------------------------------
### Methods
### -------------------------------------------------

minus.IntervalEstimates <- function(e1, e2) {
   new("IntervalEstimates",
       coefficients=coef(e1) - coef(e2),
       sd=numeric(length(coef(e1))),
       auxiliary=c(e1@auxiliary, e2@auxiliary),
       description=e1@description,
       start=e1@start, end=e2@end,
       sequenceName=e1@sequenceName
       )
}
setMethod("-", signature(e1="IntervalEstimates", e2="IntervalEstimates"),
          minus.IntervalEstimates)
rm(minus.IntervalEstimates)

mult.IntervalEstimates <- function(e1, e2) {
   if(inherits(e1, "IntervalEstimates") & inherits(e2, "numeric")) {
      a <- e1
      e1 <- e2
      e1 <- a
   }
   new("IntervalEstimates",
       coefficients=e1*coef(e2),
       sd=numeric(length(coef(e2))),
       auxiliary=e2@auxiliary,
       description=e2@description,
       start=e2@start, end=e2@end,
       sequenceName=e2@sequenceName
       )
}
setMethod("*", signature(e1="numeric", e2="IntervalEstimates"),
          mult.IntervalEstimates)
setMethod("*", signature(e1="IntervalEstimates", e2="numeric"),
          mult.IntervalEstimates)
rm(mult.IntervalEstimates)


plusNumeric.IntervalEstimates <- function(e1, e2) {
   ## add numeric and IntervalEstimates
   if(inherits(e1, "IntervalEstimates") & inherits(e2, "numeric")) {
      a <- e1
      e1 <- e2
      e1 <- a
   }
   new("IntervalEstimates",
       coefficients=e1 + coef(e2),
       sd=sd(e2),
       auxiliary=e2@auxiliary,
       description=e2@description,
       start=e2@start, end=e2@end,
       sequenceName=e2@sequenceName
       )
}
setMethod("+", signature(e1="numeric", e2="IntervalEstimates"),
          plusNumeric.IntervalEstimates)
setMethod("+", signature(e1="IntervalEstimates", e2="numeric"),
          plusNumeric.IntervalEstimates)
rm(plusNumeric.IntervalEstimates)

plus.IntervalEstimates <- function(e1, e2) {
   new("IntervalEstimates",
       coefficients=coef(e1) + coef(e2),
       sd=numeric(length(coef(e1))),
       auxiliary=c(e1@auxiliary, e2@auxiliary),
       description=e1@description,
       start=e1@start, end=e2@end,
       sequenceName=e1@sequenceName
       )
}
setMethod("+", signature(e1="IntervalEstimates", e2="IntervalEstimates"),
          plus.IntervalEstimates)
rm(plus.IntervalEstimates)

# ------ [ methods ----------

subset.coefTable <- function(x, i) {
   ## i   index vector, applies both for coefs and stdds
   ##     applies for rows only
   ##     
   ## find the subset for coef and std indices
   if(is.character(i)) {
      iCSubset <- x@iCoef[which(names(x@iCoef) %in% i)]
      iSSubset <- x@iStdd[which(names(x@iStdd) %in% i)]
   }
   else {
      iCSubset <- x@iCoef[i]
      iSSubset <- x@iStdd[i]
   }
   if(identical(x@jCoef, x@jStdd)) {
                           # long table
      iCoef <- seq(length=length(iCSubset), by=2)
      iStdd <- 1 + iCoef
      jCoef <- x@jCoef
      jStdd <- x@jStdd
   }
   else {
                           # wide table
      iCoef <- seq(along=iCSubset)
      iStdd <- seq(along=iSSubset)
      jCoef <- x@jCoef
      jStdd <- x@jStdd
   }
   jStar <- x@jStar
   names(iCoef) <- names(iStdd) <- row.names(x@table)[iCSubset]
   tt <- matrix(0, max(iCoef, iStdd), max(jCoef, jStdd, jStar))
   tt[iCoef,jCoef] <- x@table[iCSubset,x@jCoef]
   tt[iStdd,jStdd] <- x@table[iSSubset,x@jStdd]
   tt[iCoef,jStar] <- x@table[iCSubset,x@jStar]
   row.names(tt) <- rep("", nrow(tt))
   row.names(tt)[iCoef] <- row.names(x@table)[iCSubset]
   colnames(tt) <- colnames(x@table)
   new("CoefTable",
       table = tt,
       iCoef = iCoef,
       iStdd = iStdd,
       jCoef = jCoef,
       jStdd = jStdd,
       jStar = jStar,
       auxiliary = x@auxiliary)
}
setMethod("[", "CoefTable", subset.coefTable)
rm(subset.coefTable)

subset.Estimates <- function(x, i) {
   new("Estimates",
       coefficients=coef(x)[i],
       sd=sd(x)[i],
       auxiliary=x@auxiliary)
}
setMethod("[", "Estimates", subset.Estimates)
rm(subset.Estimates)

subset.IntervalEstimates <- function(x, i) {
   new("IntervalEstimates",
       coefficients=coef(x)[i],
       sd=sd(x)[i],
       auxiliary=x@auxiliary,
       description=x@description,
       start=x@start[i], end=x@end[i],
       sequenceName=x@sequenceName
       )
}
setMethod("[", "IntervalEstimates", subset.IntervalEstimates)
rm(subset.IntervalEstimates)

c.Estimates <- function(x, y) {
   new("Estimates",
       coefficients=c(coef(x), coef(y)),
       sd=c(sd(x), sd(y)),
       auxiliary=x@auxiliary)
}
setMethod("c", "Estimates", c.Estimates)
rm(c.Estimates)

c.IntervalEstimates <- function(x, y) {
   new("IntervalEstimates",
       coefficients=c(coef(x), coef(y)),
       sd=c(sd(x), sd(y)),
       auxiliary=c(x@auxiliary, y@auxiliary),
       description=x@description,
       start=c(x@start, y@start), end=c(x@end, y@end),
       sequenceName=x@sequenceName)
}
setMethod("c", "IntervalEstimates", c.IntervalEstimates)
rm(c.IntervalEstimates)

# --------------- coef methods ----------------
setMethod("coef", "Results", function(object) object@coefficients)

## CoefTable: various tables for various objects
setGeneric("coefTable",
           function(object, ...) {
              standardGeneric("coefTable")
           }
           )

coefTable.maxLik <- function(object, ...) {
   coefTable(as(object, "Estimates"), ...)   
}
setMethod("coefTable", "maxLik", coefTable.maxLik)
rm(coefTable.maxLik)

setMethod("coefTable", "glmerMod", function(object, ...) coefTable(as(object, "Estimates"), ...))
setMethod("coefTable", "lm", function(object, ...) coefTable(as(object, "Stat"), ...))
setMethod("coefTable", "lmc", function(object, ...) coefTable(as(object, "Stat"), ...))
setMethod("coefTable", "polr", function(object, ...) coefTable(as(object, "Stat"), ...))

coefTable.prq <- function(object, ...) {
   ##
   colnames <- colnames(coef(object))
   coefNames <- row.names(coef(object))
   t <- matrix(0, 2*length(coefNames), 2*length(colnames))
   nObs <- integer(ncol(coef(object)))
   t[] <- NA
   iCoef <- seq(from=1, to=nrow(t), by=2)
   iStdd <- 1 + iCoef
   names(iCoef) <- names(iStdd) <- coefNames
   jCoef <- seq(from=1, to=ncol(t), by=2)
   jStar <- 1 + jCoef
   colnames(t) <- rep("", ncol(t))
   colnames(t)[jCoef] <- colnames
   row.names(t) <- rep("", nrow(t))
   row.names(t)[iCoef] <- coefNames
   for(i in seq(length=ncol(coef(object)))) {
      t[iCoef, jCoef[i]] <- cf <- coef(object)[,i]
      t[iStdd, jCoef[i]] <- ste <- sd(object)[,i]
      t[iCoef, jStar[i]] <- abs(cf/ste)
      nObs[i] <- nObs(object)
   }
   new("CoefTable",
       table = t,
       iCoef = iCoef,
       iStdd = iStdd,
       jCoef = jCoef,
       jStar = jStar,
       nObs = nObs,
       )
}
setMethod("coefTable", "prq", coefTable.prq)
rm(coefTable.prq)

coefTable.Estimates <- function(object, ..., direction="wide") {
   ## create matrix where std errors are below/after coefficents and significance stars are next to coefficients.
   ## Here we return a matrix with numeric values, and numeric t-values.
   ## The presentation methods must convert it to a meaningful output.
   ## We construct one (well, two) vertical columns for each object
   ## and merge them sidewise.
   ##               
   if(!missing(object)) {
      data <- list(object)
      names(data) <- paste(names(match.call())[[2]], collapse="-")
                           # name of the first argument
   }
   else {
      data <- names <- NULL
   }
   args <- list(...)
   for(i in seq(along=args)) {
      y <- args[[i]]
      sy <- as(y, "Estimates")
      if(class(sy) == "Estimates") {
         data <- c(data, list(sy))
         names(data)[1 + i] <- names(match.call())[[2 + i]]
      }
   }
   coefTable(data, direction=direction)
                           # calls the list method
}
setMethod("coefTable", "Estimates", coefTable.Estimates)
setMethod("coefTable", "Stat", function(object, ...) coefTable(as(object, "Estimates"), ...))
rm(coefTable.Estimates)

coefTable.List <- function(object, 
                           direction="long") {
   ## create matrix where std errors are below/after coefficents and significance stars are next to coefficients.
   ## Here we return a matrix with numeric values, and numeric t-values.
   ## The presentation methods must convert it to a meaningful output.
   ## We construct one (well, two) vertical columns for each object
   ## and merge them sidewise.
   ##
   ## object        a list of objects, iherited from 'Estimates'
   ## direction     whether to put standard errors below ('long') or
   ##               left ('wide') of the estimates
   ##
   data <- list()
                           # data: holds only objects which can be coerced to 'Estimates'
   names <- character(0)
                           # all coefficent names
   for(i in seq(along=object)) {
      sy <- as(object[[i]], "Estimates")
      if(class(sy) == "Estimates") {
         data <- c(data, list(sy))
         if(!is.null(n <- names(sy)))
             newNames <- n
         else
             newNames <- seq(along=coef(sy))
         names <- union(names, newNames)
         names(data)[length(data)] <- names(object)[i]
      }
      else {
         warning("Object of class", class(sy), "cannot be coerced to 'Estimates'")
      }
   }
#   data <- object
   if(is.null(names(data))) {
      names(data) <- paste("C", seq(along=data), sep="")
   }
   if(direction == "long") {
      rowHeight <- 2
      colWidth <- 2
      iStdOffset <- 1
      jStdOffset <- 0
   }
   else if(direction == "wide") {
      rowHeight <- 1
      colWidth <- 3
      iStdOffset <- 0
      jStdOffset <- 1
   }
   else {
      stop("'direction' must be either 'long' or 'wide'")
   }
   t <- matrix(0, rowHeight*length(names), colWidth*length(data))
   nObs <- integer(length(data))
   t[] <- NA
   iCoef <- seq(from=1, to=nrow(t), by=rowHeight)
   iStdd <- iStdOffset + iCoef
   names(iCoef) <- names(iStdd) <- names
   jCoef <- seq(from=1, to=ncol(t), by=colWidth)
   jStdd <- jStdOffset + jCoef
   jStar <- jStdOffset + 1 + jCoef
   colnames(t) <- rep("", ncol(t))
   if(!is.null(names(data)))
       colnames(t)[jCoef] <- names(data)
   row.names(t) <- rep("", nrow(t))
   row.names(t)[iCoef] <- names
   for(i in seq(length=length(data))) {
      x <- data[[i]]
      nmx <- names(coef(x))
      if(is.null(nmx))
          nmx <- seq(along=coef(x))
      t[iCoef[nmx], jCoef[i]] <- cf <- coef(x)
      ## now we have to handle the case where some estimates are NA and we have less
      ## components in sd (sometimes NA-s are not present).
      ## We do this by using the corresponding names
      ste <- sd(x)
      nme <- names(ste)
      if(is.null(nmx))
          nme <- seq(along=ste)
      iStE <- iStdd[nme]
      t[iStE, jStdd[i]] <- ste
                           # only write non-NA standard errors
      iCf <- iCoef[nmx]
      iCf <- iCf[!is.na(cf)]
      t[iCoef, jStar[i]] <- abs(t[iCoef,jCoef[i]]/t[iStdd,jStdd[i]])
                           # transform all coefficients/stdd-s, including NA-s
                           # otherwise there will be problems with different
                           # lengths of vectors
   }
   aux <- sapply(data, function(x) unlist(x@auxiliary))
                           # as @auxiliary is a list, we have to transform it to a vector in order for 'sapply' to return a matrix
   if(all(sapply(aux, is.null)))
       aux <- matrix(0, 0, length(aux))
   else if(!is.matrix(aux)) {
      aux <- matrix(aux, 1, length(aux))
   }
   if(length(aux) == 0)
       aux <- matrix(0,0,0)
   new("CoefTable",
       table = t,
       iCoef = iCoef,
       iStdd = iStdd,
       jCoef = jCoef,
       jStdd = jStdd,
       jStar = jStar,
       auxiliary = aux
                           # aux is a matrix where cols correspond to models and rows to data
       )
}
setMethod("coefTable", "list", coefTable.List)
rm(coefTable.List)

## return estimated coefficients and std errors, eventually for parameter 'name'
## This contrasts to 'coef' which returns only coefficients and 'sd' which returns just std errors
setGeneric("estimate",
           function(object, name, ...) {
              res <- standardGeneric("estimate")
              res
           }
           )

## length
length.Estimates <- function(x) {
   length(coef(x))
}
setMethod("length", "Estimates", length.Estimates)
rm(length.Estimates)

## --------- names methods -----------------
names.CoefTable <- function(x) {
   t <- x@table
   row.names(t)[x@iCoef]
}
setMethod("names", "CoefTable", names.CoefTable)
rm(names.CoefTable)

setMethod("names", "Results", function(x) names(coef(x)))
setMethod("names", "Stat", function(x) names(coef(x)))

## names<- operator
names.CoefTable <- function(x, value) {
   row.names(x@table)[x@iCoef] <- value
   names(x@iCoef) <- value
   names(x@iStdd) <- value
   x
}
setMethod("names<-", "CoefTable", names.CoefTable)
rm(names.CoefTable)

names.Estimates <- function(x, value) {
   names(x@coefficients) <- value
   x
}
setMethod("names<-", "Estimates", names.Estimates)
rm(names.Estimates)

library(miscTools)
setMethod("nObs", "minority", function(x) length(x$residuals))
setMethod("nObs", "polr", function(x) x$n)
setMethod("nObs", "prq", function(x) as.integer(x$nObs))
setMethod("nObs", "Stat", function(x) x@nObs)

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
         stde <- sd(obj)
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
      stde <- sd(objects[[i]])
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
            lines(lowess(ex, estimate), lty=lty[i], col=col[i])
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
            yOrder <- head(order, length(order)-2)
            rXOrder <- length(ex) + 1 - xOrder
            rYOrder <- seq(from=length(ex)-1, to=1)
            rYOrder <- rep(rYOrder, each=2)
            px <- c(exj[xOrder], exj[rXOrder])
            py <- c((estimate - 1.96*stde)[yOrder],
                    (estimate+1.96*stde)[rYOrder])
         }
         polygon(px, py,
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
         mtext(names, at=seq(along=names), side=1, cex=cex, las=2)
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

## ------------ print methods ----------------
## print tables on screen
print.CoefTable <- function(x, mode="plain",
                            sep="",
                            sd=TRUE,
                            auxiliary=TRUE,
                            colnames=TRUE,
                            breaks=qnorm(c(0, 0.95, 0.975, 0.995, 1)),
                            labels=c("", "*", "**", "***"),
                            ...) {
   ## mode: how to transform estimates:
   ##       plain: do not transform
   ##       exp:  print exponents of the estimates
   ## sep:  separate columns with this character
   ## auxiliary   Logical, print the auxiliary information
   ## colnames    logical, print colnames
   ## 
   tt <- matrix("", nrow(x@table), ncol(x@table))
                           # matrix of formatted coefficients and significance marks
   row.names(tt) <- row.names(x@table)
   if(colnames)
       colnames(tt) <- colnames(x@table)
   iCoef <- x@iCoef
   iStdd <- x@iStdd
   jCoef <- x@jCoef
   jStdd <- x@jStdd
   jStar <- x@jStar
   convertedTable <- x@table
   if(mode == "exp") {
      convertedTable[iCoef,jCoef] <- exp(x@table[iCoef,jCoef])
      convertedTable[iStdd,jStdd] <- exp(x@table[iCoef,jCoef])*x@table[iStdd,jCoef]
   }
   else if(mode != "plain") {
      stop("'mode' must be either 'plain' or 'exp'")
   }
   tt[iCoef,jCoef] <- formatCNA(convertedTable[iCoef, jCoef], width=6, digits=3, format="f")
   tt[iStdd,jStdd] <- formatCNA(convertedTable[iStdd, jStdd], width=6, digits=3, format="f")
   tt[iCoef,jStar] <- as.character(cut(convertedTable[iCoef, jStar], 
                                       breaks=breaks, labels=labels,
                                       right=FALSE))
   tt[is.na(tt)] <- ""
   if(auxiliary & (nrow(x@auxiliary) > 0)) {
      aMat <- matrix("", nrow(x@auxiliary), ncol(tt))
      row.names(aMat) <- row.names(x@auxiliary)
      aMat[,jCoef] <- formatCNA(x@auxiliary)
      tt <- rbind(tt, aMat)
   }
   if(!sd) {
      if(all(iCoef == iStdd)) {
                           # wide format
         tt <- tt[,-jStdd]
      }
      else
          tt <- tt[-jCoef,]
   }
   if(sep != "" ) {
                           # add separation string
      sepTab <- ""
      if(colnames) {
         tt <- rbind(" "=colnames(tt), tt)
      }
      for(col in seq(length=ncol(tt))) {
         sepTab <- cbind(sepTab, " "=sep, tt[,col])
      }
      colnames(sepTab) <- NULL
      tt <- sepTab
   }
   print(tt, quote=FALSE)
}
setMethod("print", "CoefTable", print.CoefTable)
rm(print.CoefTable)

# --------- rBind methods: should use rbind() instead ---------------
setGeneric("rBind",
           function(x, y) {
              standardGeneric("rBind")
           }
           )

rBind.CoefTable <- function(x, y) {
   if(!(identical(x@jCoef, y@jCoef) & identical(x@jStdd, y@jStdd) & identical(x@jStar, y@jStar))) {
      stop("Coeftables of different shape")
   }
   tt <- rbind(x@table, y@table)
   iCoef <- c(x@iCoef, max(x@iCoef, x@iStdd) + y@iCoef)
   iStdd <- c(x@iStdd, max(x@iCoef, x@iStdd) + y@iStdd)
   auxiliary <- NULL
   if(identical(x@auxiliary, y@auxiliary)) {
      auxiliary <- x@auxiliary
   }
   new("CoefTable",
       table=tt,
       iCoef=iCoef, iStdd=iStdd, jCoef=x@jCoef, jStdd=x@jStdd, jStar=x@jStar)
}
setMethod("rBind", signature("CoefTable", "CoefTable"), rBind.CoefTable)
rm(rBind.CoefTable)
setMethod("rBind", signature("NULL", "CoefTable"), function(x, y) y)

## show tables on screen
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
   if(nrow(object@auxiliary) > 0) {
      aMat <- matrix("", nrow(object@auxiliary), ncol(tt))
      row.names(aMat) <- row.names(object@auxiliary)
      aMat[,jCoef] <- formatCNA(object@auxiliary)
      tt <- rbind(tt, aMat)
   }
   print(tt, quote=FALSE)
}
setMethod("show", "CoefTable", show.CoefTable)
rm(show.CoefTable)

## show stat objects
show.Estimates <- function(object) {
   ## print an object with methods 'coef' and 'sd'
   t <- abs(coef(object)/sd(object))
   mat <- cbind("Estimate"=coef(object), "Std. Error"=sd(object),
                      "t value"=t, "Pr(>|t|)"=2*pnorm(-t))
   row.names(mat) <- names(coef(object))
   printCoefmat(mat, cs.ind=1:2, tst.ind=3, P.values=TRUE)
}
setMethod("show", "Estimates", show.Estimates)
setMethod("show", "Stat", show.Estimates)
rm(show.Estimates)

## show SequentialEstimates objects
show.SequentialEstimates <- function(object) {
   ## print an object with methods 'coef' and 'sd' and "sequence"
   if(length(coef(object)) == 0) {
      cat("Zero-length object")
      return()
   }
   t <- abs(coef(object)/sd(object))
   mat <- cbind(object@sequence,
                coef(object), sd(object), t,2*pnorm(-t))
   row.names(mat) <- NULL
                           # to avoid repetition of the sequence
   colnames(mat) <- c(object@sequenceName, "Estimate", "Std. Error",
                      "t value", "Pr(>|t|)")
   print(object@description)
   printCoefmat(mat, cs.ind=2:3, tst.ind=4, P.values=TRUE)
                           # cs.ind: columns for coefs and stdds
                           # tst.ind: col for test statistics
}
setMethod("show", "SequentialEstimates", show.SequentialEstimates)
rm(show.SequentialEstimates)

## sd: extract the standard errors.  If necessary, specify the component
## Return it as numeric vector
## setGeneric("sd",
##            function(object, ...) {
##               standardGeneric("sd")
##            }
##            )
# cannot use in this way -- clashes with S3 method

sd.Estimates <- function(x) {
   x@sd
}
setMethod("sd", "Estimates", sd.Estimates)
rm(sd.Estimates)
setMethod("sd", "Stat", function(x) sqrt(diag(x@vcov)))

## sd.lm <- function(object, vCov=vcov) {
##    ## vCov: you can supply various other (such as robust)
##    ## CV estimator functions here
##    sqrt(diag(vCov(object)))
## }
## setMethod("sd", "lm", sd.lm)
## rm(sd.lm)
## setMethod("sd", "polr",
##           function(object) sqrt(diag(vcov(object))))
## setMethod("sd", "prq", function(object) object$sd)
## setMethod("sd", "Stat", function(object) sqrt(diag(vcov(object))))

setGeneric("UED",
           function(x, ...) {
              res <- standardGeneric("UED")
              res
           }
           )

## varValue: extract values of identical variables from a series of models
## Return it as numeric matrix
setGeneric("varValue",
           function(x, ...) {
              res <- standardGeneric("varValue")
              res
           }
           )

varValue.lm <- function(x, var="^Estonian", group) {
   ## extract coefs of certain variables from models
   ## group: for compatibility with 'separate'
   coef <- coef(x)
   i <- grep(var, names(coef))
   if(length(i) == 0)
       stop("no such variable")
   iv <- grep(var, names(coef[!is.na(coef)]))
   new("Stat",
       coefficients = coef[i],
       vcov = as(vcov(x)[iv,iv], "matrix")
       )
}
setMethod("varValue", "lm", varValue.lm)
setMethod("varValue", "minority", varValue.lm)
rm(varValue.lm)
   
setMethod("vcov", "Stat", function(object, ...) object@vcov)

vcov.rq <- function(object, ...) {
   s <- summary(object, ..., se="iid", covariance=TRUE)
   vc <- s$cov
   dimnames(vc) <- list(names(coef(object)), names(coef(object)))
   vc
}

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
   if(nrow(object@auxiliary) > 0) {
      aMat <- matrix("", nrow(object@auxiliary), ncol(tt))
      row.names(aMat) <- row.names(object@auxiliary)
      aMat[,jCoef] <- formatCNA(object@auxiliary, format="d")
      tt <- rbind(tt, aMat)
   }
   tt <- cbind("."=row.names(tt), tt)
   row.names(tt) <- NULL
   xt <- xtable(tt, align=align)
   xt
}
