#' loads images
#'
#' @param filename filename including path
#' @param no number of the channel
#' @return image with attributes
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = read.IBW_file(filename)
#' @export
read.IBW_file <- function(filename, no=1) {
  no = 2
  d = read.ibw(filename)
  q2 = attr(d, "WaveHeader")
  x = 1:dim(d)[1]
  y = 1:dim(d)[2]
  z = 1:(length(x)*length(y)) + (no-1)*(length(x)*length(y))
  dr = data.frame(
    x = rep(x, length(y)),
    y = rep(y, each=length(x)),
    z.nm = d[z]*1e9
  )
  dr$z.nm=dr$z.nm-min(dr$z.nm)
  attr(dr,"imageDim") <- q2$nDim[1:2]
  attr(dr,"noChannels") <- q2$nDim[3]
  sfA = q2['sfA']$sfA
  attr(dr,"convFactor") <- sfA[no]
  attr(dr,"Units") <- q2$dimUnits[no]
  dr
}
