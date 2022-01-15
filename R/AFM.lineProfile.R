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
#' @author Thomas Gredig
#' @return AFMdata object with line data
#' @export
AFM.lineProfile <- function(obj,x1,y1,x2,y2) {
  AFMcopy <- obj
  AFMcopy@history <- paste(AFMcopy@history,
                           "AFM.lineProfile(",x1,",",y1,",",x2,",",y2,");")
  d = AFM.raster(AFMcopy)
  range.x = max(d$x) - min(d$x)
  range.y = max(d$y) - min(d$y)
  if(x1 >= range.x) { warning("x1: Out of range"); x1=0.99*range.x }
  if(y1 >= range.y) { warning("y1: Out of range"); y1=0.99*range.y }
  if(x2 >= range.x) { warning("x2: Out of range"); x2=0.99*range.x}
  if(y2 >= range.y) { warning("y2: Out of range"); y2=0.99*range.y}
  width.x = AFMcopy@x.pixels
  width.y = AFMcopy@y.pixels
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
  q2=0
  r2 = c(q2)
  # Bresenham's Line Algorithm
  while (!((x1.pixel == x2.pixel) & (y1.pixel == y2.pixel))) {
    er2 = 2*er
    lx = ly = 0
    if(er2 >= Dy) {
      er = er + Dy
      x1.pixel = x1.pixel + sx
      lx = AFMcopy@x.conv
    }
    if(er2 <= Dx) {
      er = er + Dx
      y1.pixel = y1.pixel + sy
      ly = AFMcopy@y.conv
    }
    # add data point
    q1 = x1.pixel + y1.pixel * width.y
    q2 = q2+sqrt(lx^2 + ly^2)
    r=c(r, q1)
    r2 = c(r2, q2)
  }
  if (is.null(AFMcopy@data$line)) AFMcopy@data$line = list()
  AFMcopy@data$line = append(AFMcopy@data$line,list(r))
  if (is.null(AFMcopy@data$line.nm)) AFMcopy@data$line.nm = list()
  AFMcopy@data$line.nm = append(AFMcopy@data$line.nm,list(r2))
  AFMcopy
}

#' Plots AFM line
#'
#' @param obj AFMdata object
#' @param no channel number
#' @author Thomas Gredig
#' @param dataOnly if \code{TRUE} no graph is returned
#' @importFrom ggplot2 ggplot geom_path scale_color_discrete xlab theme_bw theme
#' @examples
#' filename = AFM.getSampleImages(type='ibw')
#' d = AFM.import(filename)
#' AFM.lineProfile(d, 0,0, 2000,2000) -> d1
#' AFM.lineProfile(d1, 0,0, 100,2500) -> d2
#' AFM.linePlot(d2)
#' plot(d2,addLines=TRUE)
#' @export
AFM.linePlot<- function(obj,no=1,dataOnly=FALSE) {
  if (is.null(obj@data$line)) { warning("No lines in object."); return() }
  zData = obj@data$z[[no]]
  i=1
  r = data.frame()
  for(ln in obj@data$line) {
    dz = data.frame(x=obj@data$line.nm[[i]],z=zData[ln+1])
    dz$type=i
    i=i+1
    r=rbind(r, dz)
  }
  if (dataOnly) return(r)
  ggplot(r, aes(x,z,col=as.factor(type))) +
    geom_path() +
    xlab('d (nm)') +
    scale_color_discrete('Line No') +
    theme_bw() +
    theme(legend.position = c(0.01,0.99),
          legend.justification = c(0,1))
}
