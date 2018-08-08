
setAs("Stat", "Estimates", function(from, to)
      new(to,
          coefficients=coef(from),
          stdEr=stdEr(from),
          auxiliary=from@auxiliary))
