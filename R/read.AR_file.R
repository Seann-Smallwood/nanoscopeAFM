#' loads Asylum Research Igor Wave AFM files
#'
#' @param filename filename including path
#' @param no number of the channel
#' @return image with attributes
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = read.AR_file(filename)
#' @export
read.AR_file <- function(filename, no=1) {
  d = IgorR::read.ibw(filename)
  q2 = attr(d, "WaveHeader")
  x = 1:dim(d)[1]
  y = 1:dim(d)[2]
  zp = 1:(length(x)*length(y)) + (no-1)*(length(x)*length(y))
  dr = data.frame(
    x = rep(x, length(y)),
    y = rep(y, each=length(x)),
    z = d[zp]
  )
  imageDim <- q2$nDim[1:2]
  noChannels <- q2$nDim[3]
  sfA = q2['sfA']$sfA
  convFactor <- sfA[no]
  Units <- q2$dimUnits[no]

  dr$x.nm = convFactor*1e6 * dr$x
  dr$y.nm = convFactor*1e6 * dr$y
  dr$z.nm = 1e9 * dr$z
  dr$z.nm = dr$z.nm - min(dr$z.nm)
  dr
}

