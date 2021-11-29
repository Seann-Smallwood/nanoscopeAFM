# =============================================
# http://adv-r.had.co.nz/OO-essentials.html#s4
# =============================================

# make sure AFMdata is valid
check_AFMdata <- function(object) {
  errors <- character()
  if (!(object@instrument %in% c('AR','Park','NanoSurf','Veeco'))) {
    msg <- paste('Object has invalid instrument:',object@instrument)
    errors <- c(errors,msg)
  }

  if (length(errors) == 0) TRUE else errors
}


#' AFM image class
#'
#' A S4 class to store and manipulate images from Atomic Force Microscopes.
#'
#' @slot data ($x,$y,$z): a data.frame storing the coordinates of the sample and the measured heights
#' @slot x.conv maximum width in units of nm
#' @slot y.conv maximum height in units of nm
#' @slot x.pixels number of pixels in x-direction
#' @slot y.pixels number of pixels in y-direction
#' @slot z.conv conversion factor for z to convert to $units
#' @slot z.units units for z (deg, m)
#' @slot channel name of channel
#' @slot instrument name of instrument (Park, AR, NanoSurf, Veeco)
#' @slot history history of file changes
#' @slot description AFM image description or note
#' @slot fullfilename name of filename
AFMdata<-setClass("AFMdata",
                  slots = c(data="data.frame",
                            x.conv="numeric",
                            y.conv="numeric",
                            x.pixels="numeric",
                            y.pixels="numeric",
                            z.conv = "numeric",
                            z.units = "character",
                            channel="character",
                            instrument="character",
                            history="character",
                            description="character",
                            fullfilename="character"
                  ),
                  validity = check_AFMdata)


#' Constructor method of AFMImage Class.
#'
#' @param .Object an AFMdata object
#' @slot data ($x,$y,$z): a data.frame storing the coordinates of the sample and the measured heights
#' @slot x.conv maximum width in units of nm
#' @slot y.conv maximum height in units of nm
#' @slot x.pixels number of pixels in x-direction
#' @slot y.pixels number of pixels in y-direction
#' @slot z.conv conversion factor for z to convert to $units
#' @slot z.units units for z (deg, m)
#' @slot channel name of channel
#' @slot instrument name of instrument (Park, AR, NanoSurf, Veeco)
#' @slot description AFM image description or note
#' @slot history changes to file
#' @slot fullfilename name of filename
#' @export
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
                               fullfilename)
          {
            if (!missing(data)) .Object@data<-data
            if (!missing(x.conv)) .Object@x.conv<-x.conv
            if (!missing(y.conv)) .Object@y.conv<-y.conv
            if (!missing(x.pixels)) .Object@x.pixels<-x.pixels
            if (!missing(y.pixels)) .Object@y.pixels<-y.pixels
            if (!missing(z.conv)) .Object@z.conv<-z.conv
            if (!missing(z.units)) .Object@z.units<-z.units
            if (!missing(channel)) .Object@channel <-channel
            if (!missing(instrument)) .Object@instrument <-instrument
            if (!missing(history)) .Object@history <-history
            if (!missing(description)) .Object@description <-description
            if (!missing(fullfilename)) .Object@fullfilename<-fullfilename
            validObject(.Object)
            return(.Object)
          })

#' Initialize the AFMdata object
#'
#' @param data ($x,$y,$z): a data.frame storing the coordinates of the sample and the measured heights
#' @param x.conv maximum width in units of nm
#' @param y.conv maximum height in units of nm
#' @param x.pixels number of pixels in x-direction
#' @param y.pixels number of pixels in y-direction
#' @param z.conv conversion factor for z to convert to $units
#' @param z.units units for z (deg, m)
#' @param channel name of channel
#' @param instrument name of instrument (Park, AR, NanoSurf, Veeco)
#' @param history history of file changes
#' @param description AFM image description or note
#' @param fullfilename name of filename
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
                    description,
                    fullfilename) {
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
             fullfilename))
}


#' imports AFM file
#'
#' @param filename name of AFM filename
#' @return AFMdata object
#' @author thomasgredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' @export
AFM.import <- function(filename) {
  d = AFM.read(filename)
  z.conv = 1
  if (d$z[1] != 0) z.conv = d$z.nm[1] / d$z[1]
  d1 = data.frame(z=d$z)
  if (is.null(attr(d,"note"))) attr(d,"note")="none"
  AFMdata(
    data = d1,
    channel = attr(d,"channel"),
    x.conv = max(d$x.nm)/max(d$x),
    y.conv = max(d$y.nm)/max(d$y),
    x.pixels = max(d$x),
    y.pixels = max(d$y),
    z.conv = z.conv,
    z.units = .getChannelUnits(attr(d,"channel")),
    instrument = attr(d,"instrument"),
    history = '',
    description = attr(d,"note"),
    fullfilename = filename
  )
}


#' print essential information about the AFMdata object
#'
#' @param obj AFMdata object
#' @return text with object information
#' @author thomasgredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' print(d)
#' @export
print.AFMdata <- function(obj) {
  cat("Object:     ",obj@instrument,"AFM image\n")
  cat("Description:",obj@description,'\n')
  cat("Channel:    ",obj@channel,'\n')
  cat("            ",obj@x.conv*obj@x.pixels,"nm  x ",obj@y.conv*obj@y.pixels,'nm \n')
  cat("History:    ",obj@history,'\n')
  cat("Filename:   ",obj@fullfilename)
}

#' summary of AFMdata object
#'
#' @param obj AFMdata object
#' @return summary of AFMdata object
#' @author thomasgredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' summary(d)
#' @export
summary.AFMdata <- function(obj) {
  d = AFM.raster(obj)
  data.frame(
    object = paste(obj@instrument,"AFM image"),
    description = paste(obj@description),
    resolution = paste(obj@x.pixels,"x",obj@y.pixels),
    size = paste(obj@x.conv*obj@x.pixels,"x",obj@y.conv*obj@y.pixels,'nm'),
    channel = paste(obj@channel),
    z.units = paste(obj@z.units),
    z.min.nm = min(d$z),
    z.max.nm = max(d$z),
    history = paste(obj@history),
    filename = obj@fullfilename
  )
}

#' returns data frame with ($x.nm, $y.nm, $z.nm) in nanometers
#'
#' @param obj AFMdata object
#' @return data.frame with  ($x.nm, $y.nm, $z.nm)
#' @author thomasgredig
#' @export
AFM.raster <- function(obj) {
  if(!isS4(obj)) { stop("not an S4 object") }
  data.frame(
    x = rep(1:obj@x.pixels,obj@y.pixels)*obj@x.conv,
    y = rep(1:obj@y.pixels,each=obj@x.pixels)*obj@y.conv,
    z = obj@data$z * obj@z.conv
  )
}

#' graph of AFMdata object
#'
#' @param obj AFMdata object
#' @param mpt midpoint for coloring
#' @return ggplot graph
#' @author thomasgredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' plot.AFMdata(d)
#' @export
plot.AFMdata <- function(obj,mpt=NA,...) {
  cat("Graphing:",obj@fullfilename)
  d = AFM.raster(obj)
  zLab = paste(obj@channel,obj@z.units)

  if (is.na(mpt)) mean(d$z) -> mpt
  xlab <- expression(paste('x (',mu,'m)'))
  print(
    ggplot(d, aes(x/1000, y/1000, fill = z)) +
      geom_raster() +
      scale_fill_gradient2(low='red', mid='white', high='blue',
                           midpoint=mpt) +
      xlab(xlab) +
      ylab(expression(paste('y (',mu,'m)'))) +
      labs(fill=zLab) +
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      coord_equal() +
      theme_bw()
  )
}


#' graph a histogram for the AFM image
#'
#' @param obj AFMdata object
#' @return draws a ggplot graph
#' @author thomasgredig
#' @examples
#' library(ggplot2)
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' AFM.histogram(d)
#' @export
AFM.histogram <- function(obj) {
  d = AFM.raster(obj)
  print(
    ggplot(d, aes(x=z)) +
      geom_histogram(aes(y=..density..),
                     colour="black", fill="white", bins=200)+
      geom_density(alpha=0.2, fill='red')
  )
}

# (simple check only at the moment): NEEDS more work

#' checks if the object is an AFM image
#'
#' @param obj AFMdata object
#' @return \code{TRUE} if object is an AFM image
#' @author thomasgredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' AFM.isImage(d)
#' @export
AFM.isImage <- function(obj) {
  ((obj@x.pixels > 1) & (obj@y.pixels>1))
}

