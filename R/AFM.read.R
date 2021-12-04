#' loads AFM image file
#'
#' \code{AFM.read} reads data files from different instruments
#' and stores 6 rows (x,y,z) and converted units, when possible
#' (xc,yc,zc), use the attributes to find the units and
#' channel name
#'
#' @param filename filename of (Veeco, Park, AR, NanoSurf) AFM image including path
#' @param no channel number (for Veeco, NanoSurf, AR)
#' @return AFM image with attributes
#' @author thomasgredig
#' @examples
#' filename = system.file("extdata","Park_20210916_034.tiff",package="nanoscopeAFM")
#' d = AFM.read(filename)
#' @export
AFM.read <- function(filename, no=1) {
  # does file exist?
  df = data.frame()
  if (file.exists(filename)) {
    # get file extension
    fext = tolower(tools::file_ext(filename))
    if (fext=='ibw') {
      df = read.AR_file(filename,no)
      attr(df,"instrument")='AR'
    } else if (fext=='nid') {
      df = read.NanoSurf_file(filename,no)
      attr(df,"instrument")='NanoSurf'
      h1 = AFMinfo(filename)
      attr(df,"channel") = AFMinfo.item(h1, 'Op. mode')
    } else if (fext=='tiff') {
      df = read.Park_file(filename)
      attr(df,"instrument")='Park'
      h1 = AFMinfo(filename)
      attr(df,"channel") = AFMinfo.item(h1, 'sourceName')
    } else {
      df = read.Nanoscope_file(filename,no)
      attr(df,"instrument")='Veeco'
    }
  }  else { warning(paste("File does not exist:",filename)) }
  df
}





.getChannelName <- function(h1,no) {
  # from h1=read.AR_eofHeader.V2(filename)
  gsub('(.*)[RT][er].*$','\\1',strsplit(h1$DataTypeList,",")[[1]][no])
}

.getChannelDirection <- function(h1,no) {
  # from h1=read.AR_eofHeader.V2(filename)
  gsub('.*?([RT][er].*)$','\\1',strsplit(h1$DataTypeList,",")[[1]][no])
}

.getChannelUnits <- function(channelName) {
  units = "m"  # default units
  if (nchar(channelName)>0) {
    if(grepl('Phase',channelName)) units="deg"
  } else { units="" }
  units
}
