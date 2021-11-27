#' flattens an AFM image using a plane fit
#' this flattens only z, not z.nm, so scaling
#' factor would need to be applied for z.nm
#'
#' @param obj AFMdata object
#' @return flattened matrix with AFM image
#' @author thomasgredig
#' @examples
#' d = AFM.read(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' d2 = AFM.flatten(d)
#' @export
AFM.flatten <- function(obj) {
  AFMcopy = obj
  AFMcopy@history <- paste(AFMcopy@history,"AFM.flatten();")

  d = AFM.raster(AFMcopy)
  SZ = nrow(d)
  x = d$x
  y = d$y
  z = d$z

  b = c(sum(x*z), sum(y*z), sum(z))
  a = matrix(data = c(sum(x*x), sum(x*y), sum(x),
                      sum(x*y), sum(y*y), sum(y),
                      sum(x), sum(y), SZ),
             nrow=3)
  a
  solvX = solve(a,b)

  AFMcopy@data$z =  x*solvX[1] + y*solvX[2] + solvX[3] - z
  AFMcopy
}
