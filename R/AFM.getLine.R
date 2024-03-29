#' AFM line
#'
#' create a profile data line for a particular
#' position given in pixels; easy way to reiterate
#' through all lines in an image
#'
#' @param obj AFMdata object
#' @param yPixel line number from 1 to max yPixel
#' @param no channel number
#' @param dataOnly if \code{TRUE}, returns data instead of AFMdata object
#' @param verbose if \code{TRUE}, output additional information
#' @returns AFMdata object with line data or data frame with data
#'
#' @author Thomas Gredig
#' @seealso \code{\link{AFM.lineProfile}}
#'
#' @examples
#' filename = AFM.getSampleImages()[1]
#' afmd = AFM.import(filename)
#' afmd2 = AFM.getLine(afmd, 50)
#' plot(afmd2, addLines = TRUE)
#' head(AFM.linePlot(afmd2, dataOnly = TRUE))
#'
#'
#' @export
AFM.getLine <- function(obj,
                        yPixel =1,
                        no = 1,
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
    if (verbose) cat("Extracting channel",no," from:",AFMcopy@fullFilename,"\n")
    d = AFM.raster(AFMcopy, no)
    m1 = which(d$y == obj@y.conv*(yPixel-1))
    if (verbose) cat("Data from location y=", obj@y.conv*(yPixel-1)," nm.\n")

    return(d[m1,])
  }


  if (is.null(AFMcopy@data$line)) AFMcopy@data$line = list()
  AFMcopy@data$line = append(AFMcopy@data$line,list(r))
  if (is.null(AFMcopy@data$line.nm)) AFMcopy@data$line.nm = list()
  AFMcopy@data$line.nm = append(AFMcopy@data$line.nm,list(r2))
  AFMcopy
}
