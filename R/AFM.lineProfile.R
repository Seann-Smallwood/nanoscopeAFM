#' Line Profile
#'
#' create a profile data line across an image (d), providing
#'   the starting point (x1,y1) and end point (x2,y2). The start and end
#'   points are provided in length units of the images (such as micrometers)
#'
#' @param obj AFMdata object
#' @param x1 start x position in AFM units from bottom right
#' @param y1 start y position in AFM units from bottom right
#' @param x2 end x position in AFM units from bottom right
#' @param y2 end y position in AFM units from bottom right
#' @return AFMdata object with line data
#' @export
AFM.lineProfile <- function(obj,x1,y1,x2,y2) {
  AFMcopy = obj
  AFMcopy@history <- paste(AFMcopy@history,"AFM.lineProfile();")
  d = AFM.raster(AFMcopy)
  range.x = max(d$x) - min(d$x)
  range.y = max(d$y) - min(d$y)
  if(x1 >= range.x) { warning("x1: Out of range"); x1=0.99*range.x }
  if(y1 >= range.y) { warning("y1: Out of range"); y1=0.99*range.y }
  if(x2 >= range.x) { warning("x2: Out of range"); x2=0.99*range.x}
  if(y2 >= range.y) { warning("y2: Out of range"); y2=0.99*range.y}
  width.x = which(d$x>0)[1]-1
  width.y = nrow(d)/width.x
  x1.pixel = round(x1/range.x*width.x)
  y1.pixel = round(y1/range.y*width.y)
  x2.pixel = round(x2/range.x*width.x)
  y2.pixel = round(y2/range.y*width.y)

  Dx = abs(x2.pixel - x1.pixel)
  sx = sign(x2.pixel - x1.pixel)
  Dy = - abs(y2.pixel - y1.pixel)
  sy = sign(y2.pixel - y1.pixel)
  er = Dx + Dy
  r = c(x1.pixel*width.x+y1.pixel)
  # Bresenham's Line Algorithm
  while (!((x1.pixel == x2.pixel) & (y1.pixel == y2.pixel))) {
    er2 = 2*er
    if(er2 >= Dy) {
      er = er + Dy
      x1.pixel = x1.pixel + sx;
    }
    if(er2 <= Dx) {
      er = er + Dx
      y1.pixel = y1.pixel + sy
    }
    # add data point
    q1= x1.pixel*width.x+y1.pixel
    r=c(r, q1)
  }
  AFMcopy@data = data.frame(r)
  AFMcopy
}
