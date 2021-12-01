

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

### ------------------------------------------------
### Explain the inheritance of the old classes
### ------------------------------------------------

setOldClass(c("coef.intReg","numeric"))
setOldClass(c("regionDummy","intReg", "maxLik"))
setOldClass(c("lmc", "lm"))

### -------------------------------------------------
### As
### -------------------------------------------------
          
maxLikToEstimates <- function(from, to="Estimates") {
   n <- try(nObs(from),
            silent=TRUE)
   if(inherits(n, "try-error")) {
      n <- NULL
   }
   new(to,
       coefficients=coef(from),
       sd=stats::sd(from),
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
       sd=stats::sd(e2),
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
       sd=stats::sd(x)[i],
       auxiliary=x@auxiliary)
}
setMethod("[", "Estimates", subset.Estimates)
rm(subset.Estimates)

subset.IntervalEstimates <- function(x, i) {
   new("IntervalEstimates",
       coefficients=coef(x)[i],
       sd=stats::sd(x)[i],
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
       sd=c(stats::sd(x), stats::sd(y)),
       auxiliary=x@auxiliary)
}
setMethod("c", "Estimates", c.Estimates)
rm(c.Estimates)

c.IntervalEstimates <- function(x, y) {
   new("IntervalEstimates",
       coefficients=c(coef(x), coef(y)),
       sd=c(stats::sd(x), stats::sd(y)),
       auxiliary=c(x@auxiliary, y@auxiliary),
       description=x@description,
       start=c(x@start, y@start), end=c(x@end, y@end),
       sequenceName=x@sequenceName)
}
setMethod("c", "IntervalEstimates", c.IntervalEstimates)
rm(c.IntervalEstimates)

# --------------- coef methods ----------------
setMethod("coef", "Results", function(object) object@coefficients)

coefTable.maxLik <- function(object, ...) {
   coefTable(as(object, "Estimates"), ...)   
}
setMethod("coefTable", "maxLik", coefTable.maxLik)
rm(coefTable.maxLik)

setMethod("coefTable", "lm", function(object, ...) coefTable(as(object, "Stat"), ...))
setMethod("coefTable", "lmc", function(object, ...) coefTable(as(object, "Stat"), ...))

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

## ------------ print methods ----------------
## print tables on screen
print.CoefTable <- function(x, mode="plain",
                            sep="",
                            sd=TRUE,
                            auxiliary=TRUE,
                            colnames=TRUE,
                            breaks=stats::qnorm(c(0, 0.95, 0.975, 0.995, 1)),
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

## show SequentialEstimates objects
show.SequentialEstimates <- function(object) {
   ## print an object with methods 'coef' and 'sd' and "sequence"
   if(length(coef(object)) == 0) {
      cat("Zero-length object")
      return()
   }
   t <- abs(coef(object)/stats::sd(object))
   mat <- cbind(object@sequence,
                coef(object), stats::sd(object), t,2*stats::pnorm(-t))
   row.names(mat) <- NULL
                           # to avoid repetition of the sequence
   colnames(mat) <- c(object@sequenceName, "Estimate", "Std. Error",
                      "t value", "Pr(>|t|)")
   print(object@description)
   stats::printCoefmat(mat, cs.ind=2:3, tst.ind=4, P.values=TRUE)
                           # cs.ind: columns for coefs and stdds
                           # tst.ind: col for test statistics
}
setMethod("show", "SequentialEstimates", show.SequentialEstimates)
rm(show.SequentialEstimates)

setGeneric("UED",
           function(x, ...) {
              res <- standardGeneric("UED")
              res
           }
           )
   
setMethod("vcov", "Stat", function(object, ...) object@vcov)

vcov.rq <- function(object, ...) {
   s <- summary(object, ..., se="iid", covariance=TRUE)
   vc <- s$cov
   dimnames(vc) <- list(names(coef(object)), names(coef(object)))
   vc
}
