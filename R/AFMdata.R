# =============================================
# http://adv-r.had.co.nz/OO-essentials.html#s4
# =============================================

# make sure AFMdata is valid
check_AFMdata <- function(object) {
  errors <- character()

  if (!(object@instrument %in% c('Cypher','Park','NanoSurf','Veeco'))) {
    msg <- paste('Object has invalid instrument:',object@instrument)
    errors <- c(errors,msg)
  }
  if (length(errors) == 0) TRUE else errors
}


#' AFM image class
#'
#' A S4 class to store and manipulate images from Atomic Force Microscopes.
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
#' @slot fullfilename name of file
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
                            fullfilename="character"
                  ),
                  validity = check_AFMdata)


#' Constructor method of AFMImage Class.
#'
#' @param .Object an AFMdata object
#' @slot data list with objects ($z is a list with images)
#' @slot x.conv conversion factor from pixels to nm
#' @slot y.conv conversion factor from pixels to nm
#' @slot x.pixels number of pixels in x-direction
#' @slot y.pixels number of pixels in y-direction
#' @slot z.conv (not used)
#' @slot z.units vector with units for $z (deg, m)
#' @slot channel vector with names of channels
#' @slot instrument name of instrument (Park, Cypher, NanoSurf, Veeco)
#' @slot history history of file changes
#' @slot description AFM image description or note
#' @slot fullfilename name of file
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
            if (!missing(x.conv)) {.Object@x.conv<-x.conv; .Object@x.nm=x.conv*(x.pixels-1); }
            if (!missing(y.conv)) {.Object@y.conv<-y.conv; .Object@y.nm=y.conv*(y.pixels-1); }
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
#' @param fullfilename name of file
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


#' \code{AFM.import} imports AFM file
#'
#' @param filename name of AFM filename
#' @return AFMdata object
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' summary(d)
#' plot(d)
#' @export
AFM.import <- function(filename) {
  if (grepl('ibw$',filename)) obj = read.AR_file.v2(filename)
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
      fullfilename = filename
    )
  }
  obj
}


#' print essential information about the AFMdata object
#'
#' @param obj AFMdata object
#' @return text with object information
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' print(d)
#' @export
print.AFMdata <- function(obj) {
  cat("Object:     ",obj@instrument,"AFM image\n")
  cat("Description:",obj@description,'\n')
  cat("Channel:    ",obj@channel,'\n')
  cat("            ",obj@x.nm,"nm  x ",obj@y.nm,'nm \n')
  cat("History:    ",obj@history,'\n')
  cat("Filename:   ",obj@fullfilename)
}

#' summary of AFMdata object
#'
#' @param obj AFMdata object
#' @return summary of AFMdata object
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' summary(d)
#' @export
summary.AFMdata <- function(obj) {
  if (purrr::is_empty(obj@description)) obj@description=""
  r = data.frame(
    object = paste(obj@instrument,"AFM image"),
    description = paste(obj@description),
    resolution = paste(obj@x.pixels,"x",obj@y.pixels),
    size = paste(obj@x.nm,"x",round(obj@y.nm),'nm'),
    channel = paste(obj@channel),
    history = paste(obj@history)
  )
  for(i in 1:length(r$channel)) {
    d = AFM.raster(obj,i)
    r$z.min[i]=min(d$z)
    r$z.max[i] = max(d$z)
  }
  r$z.units = paste(obj@z.units)
  r
}



#' returns data frame with ($x.nm, $y.nm, $z.nm) in nanometers
#'
#' @param obj AFMdata object
#' @param no image number
#' @return data.frame with  ($x.nm, $y.nm, $z.nm)
#' @author Thomas Gredig
#' @export
AFM.raster <- function(obj,no=1) {
  if(!isS4(obj)) { stop("not an S4 object") }
  data.frame(
    x = rep(0:(obj@x.pixels-1),obj@y.pixels)*obj@x.conv,
    y = rep(0:(obj@y.pixels-1),each=obj@x.pixels)*obj@y.conv,
    z = obj@data$z[[no]]
  )
}

#' graph of AFMdata object
#'
#' @param obj AFMdata object
#' @param mpt midpoint for coloring
#' @param imageNo number of the image
#' @return ggplot graph
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' plot.AFMdata(d)
#' @export
plot.AFMdata <- function(obj,mpt=NA,imageNo=1,...) {
  if (imageNo>length(obj@channel)) stop("imageNo out of bounds.")
  cat("Graphing:",obj@channel[imageNo])
  d = AFM.raster(obj,imageNo)
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
#' @author Thomas Gredig
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
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' AFM.isImage(d)
#' @export
AFM.isImage <- function(obj) {
  ((obj@x.pixels > 1) & (obj@y.pixels>1))
}

