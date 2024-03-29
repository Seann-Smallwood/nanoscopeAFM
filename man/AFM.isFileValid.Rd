% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AFMdata.R
\name{AFM.isFileValid}
\alias{AFM.isFileValid}
\title{Valid AFM file}
\usage{
AFM.isFileValid(filename)
}
\arguments{
\item{filename}{filename with full path}
}
\value{
\code{TRUE} if filename is a supported AFM image
}
\description{
Checks that filename is an AFM data file either Asylum Research
Igor Wavefile, Nanoscope Veeco file, Park AFM file, or Nanosurf file
}
\examples{
AFM.isFileValid(AFM.getSampleImages()[1])
}
\author{
Thomas Gredig
}
