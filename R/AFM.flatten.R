#' flattens an AFM image using a plane fit
#' this flattens only z, not z.nm, so scaling
#' factor would need to be applied for z.nm
#'
#' @param d data.frame with AFM image
#' @return flattened matrix with AFM image
#' @author thomasgredig
#' @examples
#' d = AFM.read(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' d$z.flatten = AFM.flatten(d)
#' @export
AFM.flatten <- function(d) {
  SZ = nrow(d)
  b = with(d, c(sum(x*z), sum(y*z), sum(z)))
  a = with(d, matrix(data = c(sum(x*x), sum(x*y), sum(x),
                      sum(x*y), sum(y*y), sum(y),
                      sum(x), sum(y), SZ),
             nrow=3))
  solvX = solve(a,b)
  with(d, x*solvX[1] + y*solvX[2] + solvX[3] - z)
}
