#' Crops an AFM image
#'
#' @param obj AFMdata object
#' @param x0 x origin location
#' @param y0 y origin location
#' @param width.pixels width of cropped image in pixels
#' @param height.pixels height of cropped image in pixels
#' @param verbose output fitting parameters
#' @returns returns cropped image
#'
#' @author Thomas Gredig
#'
#' @export
AFM.crop <- function(obj,x0=1,y0=1,
                     width.pixels, height.pixels,
                     verbose=FALSE) {
  AFMcopy <- obj

  AFMcopy@data$z -> dat
  AFMcopy@x.pixels -> xPixels
  AFMcopy@y.pixels -> yPixels
  startLoc = (y0-1)*yPixels + x0

  p = c()
  for(j in 0:(height.pixels-1)) {
    p = c(p,(startLoc + j*xPixels):(j*xPixels + startLoc+width.pixels-1))
  }
  q = lapply(dat,function(x) { x[p] })
  AFMcopy@data$z <- q

  AFMcopy@x.pixels = width.pixels
  AFMcopy@y.pixels = height.pixels
  AFMcopy@x.nm = (width.pixels-1)*AFMcopy@x.conv
  AFMcopy@y.nm = (height.pixels-1)*AFMcopy@y.conv


  AFMcopy
}
