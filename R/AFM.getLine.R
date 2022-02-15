#' AFM line
#'
#' create a profile data line for a particular
#' line, given in pixels; easy way to reiterate
#' tbrough all lines for example
#'
#' @param obj AFMdata object
#' @param yPixel line number from 1 to max yPixel
#' @param dataOnly if \code{TRUE}, returns data instead of AFMdata object
#' @param verbose if \code{TRUE}, output additional information
#' @author Thomas Gredig
#' @return AFMdata object with line data or data frame with data
#' @examples
#' filename = AFM.getSampleImages()[1]
#' afmd = AFM.import(filename)
#' afmd2 = AFM.getLine(afmd, 50)
#' plot(afmd2, addLines = TRUE)
#' head(AFM.linePlot(afmd2, dataOnly = TRUE))
#' @export
AFM.getLine <- function(obj,
                        yPixel =1, 
                        dataOnly = FALSE, 
                        verbose=FALSE) {
  AFMcopy <- obj
  AFMcopy@history <- paste(AFMcopy@history,
                           "AFM.getLine(",yPixel,");")
  
  # distance from one to the next one in nm:
  r2 = 0:(obj@x.pixels-1)*obj@x.conv
  # pixels selected
  r = obj@y.pixels*(yPixel-1)+1:(obj@x.pixels)
  
  if (verbose) print(paste("delta Y:",signif(AFMcopy@y.conv,4),
                           "nm/px and delta X:",signif(AFMcopy@x.conv,4),"nm/px"))
  if(dataOnly) {
    d = AFM.raster(AFMcopy)
    m1 = which(d$y == obj@y.conv*(yPixel-1))
    return(d[m1,])
  }
  
  
  if (is.null(AFMcopy@data$line)) AFMcopy@data$line = list()
  AFMcopy@data$line = append(AFMcopy@data$line,list(r))
  if (is.null(AFMcopy@data$line.nm)) AFMcopy@data$line.nm = list()
  AFMcopy@data$line.nm = append(AFMcopy@data$line.nm,list(r2))
  AFMcopy
}