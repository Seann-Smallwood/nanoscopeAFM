#' AFM image class
#'
#' A S4 class to store and manipulate images from Atomic Force Microscopes.
#' It contains the names of channels (exact naming depends on the type/instrument)
#' The class supports multiple images; it can also contain additional information
#' such as lines, etc.
#'
#' @slot data list with objects ($z is a list with images)
#' @slot x.conv conversion factor from pixels to nm
#' @slot y.conv conversion factor from pixels to nm
#' @slot x.pixels number of pixels in x-direction
#' @slot y.pixels number of pixels in y-direction
#' @slot x.nm length of image
#' @slot y.nm height of image
#' @slot z.conv (not used)
#' @slot z.units vector with units for $z (deg, m)
#' @slot channel vector with names of channels
#' @slot instrument name of instrument (Park, Cypher, NanoSurf, Veeco)
#' @slot history history of file changes
#' @slot description AFM image description or note
#' @slot fullFilename name of file
AFMdata<-setClass("AFMdata",
                  slots = c(data="list",
                            x.conv="numeric",
                            y.conv="numeric",
                            x.pixels="numeric",
                            y.pixels="numeric",
                            x.nm = "numeric",
                            y.nm = "numeric",
                            z.conv = "numeric",
                            z.units = "character",
                            channel="character",
                            instrument="character",
                            history="character",
                            description="character",
                            fullFilename="character"
                  ),
                  validity =
                    function(object) {
                      errors <- character()

                      if (!(object@instrument %in% c('Cypher','Park','NanoSurf','Veeco','artificial'))) {
                        msg <- paste('Object has invalid instrument:',object@instrument)
                        errors <- c(errors,msg)
                      }
                      if (length(errors) == 0) TRUE else errors
                    }
                  )


#' Constructor method of AFMImage Class.
#'
#' @param .Object an AFMdata object
#' @param data list with objects (z is a list with images)
#' @param x.conv conversion factor from pixels to nm
#' @param y.conv conversion factor from pixels to nm
#' @param x.pixels number of pixels in x-direction
#' @param y.pixels number of pixels in y-direction
#' @param z.conv (not used)
#' @param z.units vector with units for z (deg, m)
#' @param channel vector with names of channels
#' @param instrument name of instrument (Park, Cypher, NanoSurf, Veeco)
#' @param history history of file changes
#' @param description AFM image description or note
#' @param fullFilename name of file
#' @return initialized AFMdata object
#' @export
#' @importFrom methods setMethod initialize new validObject
setMethod(f="initialize",
          signature="AFMdata",
          definition= function(.Object,
                               data,
                               x.conv,
                               y.conv,
                               x.pixels,
                               y.pixels,
                               z.conv,
                               z.units ,
                               channel,
                               instrument,
                               history,
                               description,
                               fullFilename)
          {
            if (!missing(data)) .Object@data<-data
            if (!missing(x.conv)) {.Object@x.conv<-x.conv; .Object@x.nm=round(x.conv*(x.pixels-1)); }
            if (!missing(y.conv)) {.Object@y.conv<-y.conv; .Object@y.nm=round(y.conv*(y.pixels-1)); }
            if (!missing(x.pixels)) .Object@x.pixels<-x.pixels
            if (!missing(y.pixels)) .Object@y.pixels<-y.pixels
            if (!missing(z.conv)) .Object@z.conv<-z.conv
            if (!missing(z.units)) .Object@z.units<-z.units
            if (!missing(channel)) .Object@channel <-channel
            if (!missing(instrument)) .Object@instrument <-instrument
            if (!missing(history)) .Object@history <-history
            if (!missing(description)) .Object@description <-description
            if (!missing(fullFilename)) .Object@fullFilename<-fullFilename
            validObject(.Object)
            return(.Object)
          })




#' Initialize the AFMdata object
#'
#' @param data list with objects ($z is a list with images)
#' @param x.conv conversion factor from pixels to nm
#' @param y.conv conversion factor from pixels to nm
#' @param x.pixels number of pixels in x-direction
#' @param y.pixels number of pixels in y-direction
#' @param z.conv (not used)
#' @param z.units vector with units for $z (deg, m)
#' @param channel vector with names of channels
#' @param instrument name of instrument (Park, Cypher, NanoSurf, Veeco)
#' @param history history of file changes
#' @param description AFM image description or note
#' @param fullFilename name of file
#' @export
AFMdata <- function(data,
                    x.conv,
                    y.conv,
                    x.pixels,
                    y.pixels,
                    z.conv,
                    z.units ,
                    channel,
                    instrument,
                    history,
                    description="",
                    fullFilename) {
  return(new("AFMdata",
             data,
             x.conv,
             y.conv,
             x.pixels,
             y.pixels,
             z.conv,
             z.units ,
             channel,
             instrument,
             history,
             description,
             fullFilename))
}


#' Imports AFM file
#'
#' Use this function to create an AFMdata object from the filename;
#' four AFM formats (TIFF, NID, IBW, and 000) are supported. Use
#' \code{AFM.raster()} to create a data.frame from this object, or
#' use \code{plot()} to generate an image.
#'
#' @param filename name of AFM filename
#' @param verbose if \code{TRUE}, output additional information during loading of file
#' @return AFMdata object
#' @author Thomas Gredig
#' @importFrom ggplot2 ggplot aes
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw')[1])
#' summary(d)
#' plot(d)
#' @export
AFM.import <- function(filename, verbose=FALSE) {
  if (grepl('ibw$',filename)) obj = read.AR_file.v2(filename)
  else if (grepl('tiff$',filename)) obj = read.Park_file.v2(filename)
  else {
    d = AFM.read(filename)
    z.conv = 1
    if (d$z[1] != 0) z.conv = d$z.nm[1] / d$z[1]
    d1 = list(d$z.nm)
    if (is.null(attr(d,"note"))) attr(d,"note")="none"
    obj = AFMdata(
      data = list(z=d1),
      channel = attr(d,"channel"),
      x.conv = max(d$x.nm)/(max(d$x)-1),
      y.conv = max(d$y.nm)/(max(d$y)-1),
      x.pixels = max(d$x),
      y.pixels = max(d$y),
      z.conv = z.conv,
      z.units = .getChannelUnits(attr(d,"channel")),
      instrument = attr(d,"instrument"),
      history = '',
      description = attr(d,"note"),
      fullFilename = filename
    )
  }
  if (verbose) print(paste("Instrument:", obj@instrument))
  obj
}


#' Print AFMdata object
#'
#' prints essential information about the AFMdata object, which includes
#' the description, channel, image size, history, and filename
#'
#' @param x AFMdata object
#' @param ... other arguments
#' @return text with object information
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw')[1])
#' print(d)
#' @export
print.AFMdata <- function(x, ...) {
  if(AFM.isImage(x)) dataType='Image' else dataType='Force Curve'
  cat("Object:     ",x@instrument,"AFM",dataType,"\n")
  cat("Description:",x@description,'\n')
  cat("Channel:    ",x@channel,'\n')
  cat("            ",x@x.nm,"nm  x ",x@y.nm,'nm \n')
  cat("History:    ",x@history,'\n')
  cat("Filename:   ",x@fullFilename)
}

#' summary of AFMdata object
#'
#' @param object AFMdata object
#' @param ... other summary parameters
#' @return summary of AFMdata object
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw')[1])
#' summary(d)
#' @export
summary.AFMdata <- function(object,...) {
  if (purrr::is_empty(object@description)) object@description=""
  if(AFM.isImage(object)) dataType='Image' else dataType='Force Curve'
  r = data.frame(
    objectect = paste(object@instrument,dataType),
    description = paste(object@description),
    resolution = paste(object@x.pixels,"x",object@y.pixels),
    size = paste(object@x.nm,"x",round(object@y.nm),'nm'),
    channel = paste(object@channel),
    history = paste(object@history)
  )
  for(i in seq_len(length(r$channel))) {
    d = AFM.raster(object,i)
    r$z.min[i]=min(d$z)
    r$z.max[i] = max(d$z)
  }
  r$z.units = paste(object@z.units)
  r
}



#' Raster data frame
#'
#' data frame has  ($x, $y, $z) in units for particular channel, ($x, $y) are
#' always in units of nanometer
#'
#' @param obj AFMdata object
#' @param no channel number
#' @return data.frame with ($x, $y, $z) raster image; ($x,$y) in units of nm
#' @author Thomas Gredig
#' @examples
#' afmd = AFM.import(AFM.getSampleImages(type='ibw')[1])
#' d = AFM.raster(afmd, 1)
#' head(d)
#' @export
AFM.raster <- function(obj,no=1) {
  if(!isS4(obj)) { stop("Not an S4 object, AFMdata object expected.") }
  if (AFM.isImage(obj)) {
    dr = data.frame(
      x = rep(0:(obj@x.pixels-1),obj@y.pixels)*obj@x.conv,
      y = rep(0:(obj@y.pixels-1),each=obj@x.pixels)*obj@y.conv,
      z = obj@data$z[[no]]
    )
  } else {  # could be a spectrum
    dr = data.frame(
      x = (0:(obj@x.pixels-1))*obj@x.conv,
      z = obj@data$z[[no]]
    )
  }
  dr
}

#' Graph of AFMdata object
#'
#' By default, trims 1 percent of the outliers in height data
#'
#' @param x AFMdata object
#' @param no channel number of the image
#' @param mpt midpoint for coloring
#' @param graphType 1 = graph with legend outside, 2 = square graph with line bar, 3 = plain graph
#' @param trimPeaks value from 0 to 1, where 0=trim 0\% and 1=trim 100\% of data points, generally a value less than 0.01 is useful to elevate the contrast of the image
#' @param addLines if \code{TRUE} lines from obj are added to graph, lines can be added with \code{\link{AFM.lineProfile}} for example
#' @param redBlue if \code{TRUE} output red / blue color scheme
#' @param verbose if \code{TRUE} it outputs additional information.
#' @param quiet if \code{TRUE} then no output at all
#' @param ... other arguments
#'
#' @return ggplot graph
#'
#' @author Thomas Gredig
#'
#' @importFrom utils head tail
#' @importFrom ggplot2 ggplot aes geom_raster geom_line theme_bw scale_fill_gradient2 xlab ylab labs scale_y_continuous scale_x_continuous coord_equal geom_text theme element_blank
#' @importFrom ggplot2 scale_fill_viridis_c
#' @seealso \code{\link{AFM.lineProfile}}
#'
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw')[1])
#' plot(d, graphType=2)
#' @export
plot.AFMdata <- function(x, no=1, mpt=NA, graphType=1, trimPeaks=0.01, addLines=FALSE, redBlue = FALSE, verbose=FALSE, quiet=FALSE, fillOption='viridis'...) {
  if (no>length(x@channel)) stop("imageNo out of bounds.")
  if (!quiet) cat("Graphing:",x@channel[no])
  if (verbose) print(paste("History:",x@history))
  d = AFM.raster(x,no)
  zLab = paste0(x@channel[no],' (',x@z.units[no],')')
  zLab = gsub('Retrace|Trace','',zLab)

  xlab <- expression(paste('x (',mu,'m)'))

  if (trimPeaks>0) {
    AFM.histogram(x, no, dataOnly = TRUE) -> qHist
    cumsum(qHist$zDensity) -> csHist
    csHist / max(csHist) -> csHist
    lowerBound = qHist$mids[tail(which(csHist<(trimPeaks/2)),n=1)]
    upperBound = qHist$mids[head(which(csHist>(1-trimPeaks/2)),n=1)]
    d$z[which(d$z<lowerBound)] <- lowerBound
    d$z[which(d$z>upperBound)] <- upperBound
  }

  if (addLines) {
    # check if there are lines
    if (is.null(x@data$line)) { warning("No lines attached.") }
    else {
      if (verbose) print("Adding lines using min. value for color.")
      for(zLine in x@data$line) {
        d$z[zLine] = min(d$z)
      }
    }
  }

  if (is.na(mpt)) mean(d$z) -> mpt

  if (verbose) print(paste("z range: ",min(d$z)," to ",max(d$z)," midpoint",mpt))
  if (redBlue) sFill = scale_fill_gradient2(low='red', mid='white', high='blue', midpoint=mpt)
  else sFill = scale_fill_viridis(option=fillOption)
  
  if (graphType==1) {
    g1 = ggplot(d, aes(x/1000, y/1000, fill = z)) +
      geom_raster() +
      sFill +
      xlab(xlab) +
      ylab(expression(paste('y (',mu,'m)'))) +
      labs(fill=zLab) +
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      coord_equal() +
      theme_bw()
  } else if (graphType==2) {
    # figure out coordinates for line
    bar.length = signif(x@x.nm*0.2,2)  # nm
    bar.x.start = 0.05*x@x.pixels * x@x.conv
    bar.y.start = 0.05*x@y.pixels * x@y.conv
    bar.x.end = bar.x.start + bar.length
    d.line = data.frame(
      x = c(bar.x.start, bar.x.end),
      y = c(bar.y.start, bar.y.start),
      z = 1,
      myLabel = c(paste(bar.length,"nm"),"")
    )
    zLab = x@z.units[no]
    g1 = ggplot(d, aes(x/1000, y/1000, fill = z)) +
      geom_raster() +
      sFill +
      xlab("") +
      ylab("") +
      labs(fill=zLab) +
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      coord_equal() +
      geom_line(data = d.line, aes(x/1000,y/1000), size=4) +
      geom_text(data = d.line, aes(label=myLabel), vjust=-1, hjust=0) +
      theme_bw() +
      theme(legend.position =c(0.99,0.01),
            legend.justification = c(1,0)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())

  } else if (graphType==3) {
    zLab = x@z.units[no]
    g1 = ggplot(d, aes(x/1000, y/1000, fill = z)) +
      geom_raster() +
      sFill +
      xlab("") +
      ylab("") +
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      coord_equal() +
      theme_bw() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())

  } else stop('graphType is not supported.')

  g1
}



# (simple check only at the moment): NEEDS more work

#' checks if the object is an AFM image
#'
#' @param obj AFMdata object
#' @return \code{TRUE} if object is an AFM image
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw')[1])
#' AFM.isImage(d)
#' @export
AFM.isImage <- function(obj) {
  ((obj@x.pixels > 1) & (obj@y.pixels>1))
}




#' Valid AFM file
#'
#' Checks that filename is an AFM data file either Asylum Research
#' Igor Wavefile, Nanoscope Veeco file, Park AFM file, or Nanosurf file
#'
#' @param filename filename with full path
#' @return \code{TRUE} if filename is a supported AFM image
#' @author Thomas Gredig
#' @examples
#' AFM.isFileValid(AFM.getSampleImages()[1])
#' @export
AFM.isFileValid <- function(filename) {
  validFile = FALSE
  if (file.exists(filename)) {
    validFile =  grepl('\\.ibw$',filename, ignore.case = TRUE) |
      grepl('\\.tiff$',filename, ignore.case = TRUE) |
      grepl('\\.nid$',filename, ignore.case = TRUE) |
      grepl('\\.\\d{3}$',filename, ignore.case = TRUE)
  }
  validFile
}

.onAttach <- function(libname, pkgname) {
  if (runif(1) > 0.7) packageStartupMessage(paste("Please cite", pkgname,',see https://doi.org/10.5281/zenodo.5770772'))
}
