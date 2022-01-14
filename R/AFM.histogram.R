#' Histogram of AFM image
#'
#' @param obj AFMdata object
#' @param no channel number of the image
#' @param dataOnly if \code{TRUE} a data frame with the histogram
#' data is returned
#' @return data frame or ggplot
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw')[1])
#' head(AFM.histogram(d, dataOnly=TRUE),n=20)
#' AFM.histogram(d)
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density
#' @importFrom graphics hist
AFM.histogram <- function(obj, no=1, dataOnly=FALSE) {
  dr = AFM.raster(obj,no)
  if (dataOnly) {
    graphics::hist(dr$z, breaks=10, plot=FALSE) -> q
    result = data.frame(mids = q$mids , zDensity = q$density/sum(q$density))
  } else {
    result =
      ggplot(dr, aes(x=z)) +
      geom_histogram(aes(y=..density..),
                     colour="black", fill="pink", bins=200)+
      geom_density(alpha=0.2, fill='red') +
      theme_bw()
  }
  result
}
