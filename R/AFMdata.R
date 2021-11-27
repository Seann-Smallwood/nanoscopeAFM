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
#' @slot x.max.nm maximum width in units of nm
#' @slot y.max.nm maximum height in units of nm
#' @slot x.pixels number of pixels in x-direction
#' @slot y.pixels number of pixels in y-direction
#' @slot z.conv conversion factor for z to convert to $units
#' @slot z.units units for z (deg, m)
#' @slot channel name of channel
#' @slot instrument name of instrument (Park, AR, NanoSurf, Veeco)
#' @slot fullfilename name of filename
AFMdata<-setClass("AFMdata",
                  slots = c(data="data.frame",
                            x.max.nm="numeric",
                            y.max.nm="numeric",
                            x.pixels="numeric",
                            y.pixels="numeric",
                            z.conv = "numeric",
                            z.units = "character",
                            channel="character",
                            instrument="character",
                            fullfilename="character"
                  ),
                  validity = check_AFMdata)


#' Constructor method of AFMImage Class.
#'
#' @param .Object an AFMdata object
#' @slot data ($x,$y,$z): a data.frame storing the coordinates of the sample and the measured heights
#' @slot x.max.nm maximum width in units of nm
#' @slot y.max.nm maximum height in units of nm
#' @slot x.pixels number of pixels in x-direction
#' @slot y.pixels number of pixels in y-direction
#' @slot z.conv conversion factor for z to convert to $units
#' @slot z.units units for z (deg, m)
#' @slot channel name of channel
#' @slot instrument name of instrument (Park, AR, NanoSurf, Veeco)
#' @slot fullfilename name of filename
#' @export
setMethod(f="initialize",
          signature="AFMdata",
          definition= function(.Object,
                               data,
                               x.max.nm,
                               y.max.nm,
                               x.pixels,
                               y.pixels,
                               z.conv,
                               z.units ,
                               channel,
                               instrument,
                               fullfilename)
          {
            if (!missing(data)) .Object@data<-data
            if (!missing(x.max.nm)) .Object@x.max.nm<-x.max.nm
            if (!missing(y.max.nm)) .Object@y.max.nm<-y.max.nm
            if (!missing(x.pixels)) .Object@x.pixels<-x.pixels
            if (!missing(y.pixels)) .Object@y.pixels<-y.pixels
            if (!missing(z.conv)) .Object@z.conv<-z.conv
            if (!missing(z.units)) .Object@z.units<-z.units
            if (!missing(channel)) .Object@channel <-channel
            if (!missing(instrument)) .Object@instrument <-instrument
            if (!missing(fullfilename)) .Object@fullfilename<-fullfilename
            validObject(.Object)
            return(.Object)
          })

#' Initialize the AFMdata object
#'
#' @param data ($x,$y,$z): a data.frame storing the coordinates of the sample and the measured heights
#' @param x.max.nm maximum width in units of nm
#' @param y.max.nm maximum height in units of nm
#' @param x.pixels number of pixels in x-direction
#' @param y.pixels number of pixels in y-direction
#' @param z.conv conversion factor for z to convert to $units
#' @param z.units units for z (deg, m)
#' @param channel name of channel
#' @param instrument name of instrument (Park, AR, NanoSurf, Veeco)
#' @param fullfilename name of filename
#' @export
AFMdata <- function(data,
                    x.max.nm,
                    y.max.nm,
                    x.pixels,
                    y.pixels,
                    z.conv,
                    z.units ,
                    channel,
                    instrument,
                    fullfilename) {
  return(new("AFMdata",
             data,
             x.max.nm,
             y.max.nm,
             x.pixels,
             y.pixels,
             z.conv,
             z.units ,
             channel,
             instrument,
             fullfilename))
}


#' imports AFM file
#'
#' @param filename name of AFM filename
#' @return AFMdata object
#' @author thomasgredig
#' @export
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' @export
AFM.import <- function(filename) {
  d = AFM.read(filename)
  AFMdata(
    data = d,
    channel = attr(d,"channel"),
    x.max.nm = max(d$x.nm),
    y.max.nm = max(d$y.nm),
    x.pixels = max(d$x),
    y.pixels = max(d$y),
    z.conv = d$z.nm[1] / d$z[1],
    z.units = .getChannelUnits(attr(d,"channel")),
    instrument = attr(d,"instrument"),
    fullfilename = filename
  )
}


#' print essential information about the AFMdata object
#'
#' @param obj AFMdata object
#' @return text with object information
#' @author thomasgredig
#' @export
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' print(d)
#' @export
print.AFMdata <- function(obj) {
  cat("Object:",obj@instrument,"AFM image\n")
  cat(max(obj@x.max.nm),"nm  x ",max(obj@y.max.nm),'nm \n')
  cat("Filename:",obj@fullfilename)
}

#' summary of AFMdata object
#'
#' @param obj AFMdata object
#' @return ggplot graph
#' @author thomasgredig
#' @export
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' summary(d)
#' @export
summary.AFMdata <- function(obj) {
  data.frame(
    object = paste(obj@instrument,"AFM image"),
    size = paste(max(obj@x.max.nm),"x",max(obj@y.max.nm),'nm'),
    filename = obj@fullfilename
  )
}

#' graph of AFMdata object
#'
#' @param obj AFMdata object
#' @param mpt midpoint for coloring
#' @return ggplot graph
#' @author thomasgredig
#' @export
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' summary(d)
#' @export
plot.AFMdata <- function(obj,mpt=NA,...) {
  cat("Filename:",obj@fullfilename)
  if (is.na(mpt)) mean(obj@data$z.nm) -> mpt
  xlab <- expression(paste('x (',mu,'m)'))
  print(
    ggplot(obj@data, aes(x.nm, y.nm, fill = z.nm)) +
      geom_raster() +
      scale_fill_gradient2(low='red', mid='white', high='blue',
                           midpoint=mpt) +
      xlab(xlab) +
      ylab(expression(paste('y (',mu,'m)'))) +
      labs(fill='z (nm)') +
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      coord_equal() +
      theme_bw()
  )
}


#' graph a histogram for the AFM image
#'
#' @param AFMdata AFMdata object
#' @return draws a ggplot graph
#' @author thomasgredig
#' @export
#' @examples
#' library(ggplot2)
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' AFM.histogram(d)
#' @export
AFM.histogram <- function(AFMdata) {
  print(
    ggplot(AFMdata@data, aes(x=z.nm)) +
      geom_histogram(aes(y=..density..),
                     colour="black", fill="white", bins=200)+
      geom_density(alpha=0.2, fill='red')
  )

}

# (simple check only at the moment): NEEDS more work

#' checks if the object is an AFM image
#'
#' @param AFMdata AFMdata object
#' @return \code{TRUE} if object is an AFM image
#' @author thomasgredig
#' @export
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' AFM.isImage(d)
#' @export
AFM.isImage <- function(AFMdata) {
  ((AFMdata@x.pixels > 1) & (AFMdata@y.pixels>1))
}

