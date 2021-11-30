#' flattens an AFM image using a plane fit
#'
#' this flattens only z of one of the images
#' using a plane fit
#'
#' @param obj AFMdata object
#' @param no Image number
#' @return flattened matrix with AFM image
#' @author thomasgredig
#' @examples
#' d = AFM.read(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' d2 = AFM.flatten(d)
#' plot(d2)
#' @export
AFM.flatten <- function(obj,no=1) {
  AFMcopy = obj
  AFMcopy@history <- paste(AFMcopy@history,"AFM.flatten(",no,");")

  d = AFM.raster(AFMcopy,no)
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

  AFMcopy@data$z[[no]] =  x*solvX[1] + y*solvX[2] + solvX[3] - z
  AFMcopy
}
