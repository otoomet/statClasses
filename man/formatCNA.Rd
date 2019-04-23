\name{formatCNA}
\alias{formatCNA}
\title{formatC with custom NA-string}
\description{
  Allows to use a custom NA-string in \code{\link{formatC}} instead of
  the defaults \sQuote{NA}.
}
\usage{
formatCNA(x, naStr = "", ...)
}
\arguments{
  \item{x}{vector to be formatted, normally numeric}
  \item{naStr}{NA-value to be used, character}
  \item{...}{other arguments for \code{\link{formatC}}
  }
}
\value{
  character vector, formatted vector \code{x} with NA-s replaced by \code{naStr}
}
\seealso{
  \code{\link{formatC}}
}
\examples{
x <- c(log(10), NA)
formatCNA(x, "-")  # replace NA by '-'
}
\keyword{print}
\keyword{utilities}
