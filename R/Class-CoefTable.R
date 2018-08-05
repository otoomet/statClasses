
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
                        auxiliary = "list"
                        ))

