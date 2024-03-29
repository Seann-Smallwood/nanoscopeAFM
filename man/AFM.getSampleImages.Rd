% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AFM.getSampleImages.R
\name{AFM.getSampleImages}
\alias{AFM.getSampleImages}
\title{Sample AFM images}
\usage{
AFM.getSampleImages(type = "*")
}
\arguments{
\item{type}{could be "ibw" for Asylum Research, "nid" for NanoSurf, "tiff" for Park,
if empty or "*", all files will be returned}
}
\value{
vector with path/filename to AFM sample images
}
\description{
returns sample AFM images included in this library
}
\examples{
file.list = AFM.getSampleImages()
print(paste("Found",length(file.list),"sample files."))
}
\author{
Thomas Gredig
}
