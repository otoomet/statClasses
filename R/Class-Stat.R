### class 'Stat' results estimates with var-covar matrix

setClass("Stat",
         representation("Results",
                        vcov = "matrix"
                        ))
