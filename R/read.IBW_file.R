#' loads images
#'
#' @param filename filename including path
#' @return list with header, file ID, and images
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = read.IBW_file(filename)
#' @export
read.IBW_file <- function(filename) {
  if (NID.checkFile(filename) == 0) {
    d = read.ibw(filename)
    q1 = read.ibw(filename, HeaderOnly = TRUE)
    q2 = attr(q1, "WaveHeader")
    imagePixels = q2$nDim[1]
    sfA = q2['sfA']
    imageWidths.um = sfA$sfA*1e6*(imagePixels-1)

    x = 1:dim(d)[1]
    y = 1:dim(d)[2]
    dr = data.frame(
      x = (rep(x, length(y))-1)*sfA$sfA[1]*1e6,
      y = (rep(y, each=length(x))-1)*sfA$sfA[1]*1e6,
      z.nm = d[1:(length(x)*length(y))]*1e9
    )
    dr$z.nm=dr$z.nm-min(dr$z.nm)
  }
  dr
}
