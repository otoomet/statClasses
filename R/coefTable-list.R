
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
                           # as @auxiliary is a list, we have to transform it to a vector in order
                           # for 'sapply' to return a matrix
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
