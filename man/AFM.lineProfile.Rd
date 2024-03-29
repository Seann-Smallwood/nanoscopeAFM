% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AFM.lineProfile.R
\name{AFM.lineProfile}
\alias{AFM.lineProfile}
\title{Line Profile}
\usage{
AFM.lineProfile(obj, x1, y1, x2, y2, verbose = FALSE)
}
\arguments{
\item{obj}{AFMdata object}

\item{x1}{start x position in  units of nm from bottom left}

\item{y1}{start y position in  units of nm from bottom left}

\item{x2}{end x position in  units of nm from bottom left}

\item{y2}{end y position in  units of nm from bottom left}

\item{verbose}{logical, if \code{TRUE}, output additional information}
}
\value{
AFMdata object with line data
}
\description{
create a profile data line across an image (d), providing
  the starting point (x1,y1) and end point (x2,y2). The start and end
  points are provided in units of nanometers
}
\examples{
afmd = AFM.artificialImage(width=128, height=128, type='calibration', verbose=FALSE)
AFM.lineProfile(afmd, 100, 500, 900, 500) -> afmd2
AFM.linePlot(afmd2)

}
\seealso{
\code{\link{AFM.getLine}}, \code{\link{AFM.linePlot}}
}
\author{
Thomas Gredig
}
