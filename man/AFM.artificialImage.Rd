% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AFM.artificialImage.R
\name{AFM.artificialImage}
\alias{AFM.artificialImage}
\title{Artificial AFM image}
\usage{
AFM.artificialImage(
  width = 10,
  height = 10,
  minZ = 0,
  maxZ = 100,
  imageWidth = 1000,
  imageHeight = 1000,
  type = c("random", "gradient", "calibration"),
  addNoise = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{width}{width in pixels}

\item{height}{height in pixels}

\item{minZ}{minimum z height}

\item{maxZ}{maximum z height}

\item{imageWidth}{width of image in nm}

\item{imageHeight}{height of image in nm}

\item{type}{can be random gradient calibration}

\item{addNoise}{if \code{TRUE}, add a bit of noise to the data}

\item{verbose}{if \code{TRUE}, output additional information}
}
\value{
AFMdata object
}
\description{
generates an artificially made AFM object, which is sometimes
useful for testing algorithms or models
}
\examples{
a = AFM.artificialImage(type='calibration')
plot(a)
}
\author{
Thomas Gredig
}
