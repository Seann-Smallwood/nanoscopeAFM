#' AFMinfo object
#'
#' heater information about the AFM image, includes scan rate, widths, etc.
#'
#' @param filename full filename of instrument file
#' @return AFMinfo class object
#' @author Thomas Gredig
#' @examples
#' filename = system.file("extdata","Veeco_20160622.003",package="nanoscopeAFM")
#' h = AFMinfo(filename)
#' summary(h)
#' @export
AFMinfo <- function(filename) {
  fext = tolower(tools::file_ext(filename))
  if (fext=='ibw') {
    data = read.AR_header.v2(filename)
    type = 'Cypher'
    widthPixel = data$value[grep('PointsLines',data$name)[1]]
    heightPixel = data$value[grep('ScanLines',data$name)[1]]
    scanAngle = data$value[grep('ScanAngle',data$name)[1]]
    scanRate.Hz = data$value[grep('ScanRate',data$name)[1]]
    note = data$value[grep('ImageNote',data$name)[1]]
  } else if (fext=='tiff') {
    data = read.Park_header.v2(filename)
    type = 'Park'
    widthPixel = data$value[grep('nWidth',data$name)[1]]
    heightPixel = data$value[grep('nHeight',data$name)[1]]
    scanAngle = data$value[grep('dfAngle',data$name)[1]]
    scanRate.Hz = data$value[grep('dfScanRateHz',data$name)[1]]
    note = data$value[grep('ImageNote',data$name)[1]]
  } else if (fext=='nid') {
    data = read.NanoSurf_header.v2(filename)
    type = 'NanoSurf'
    widthPixel = data$value[grep('Points',data$name)[1]]
    heightPixel = data$value[grep('Lines',data$name)[1]]
    scanAngle = data$value[grep('dfAngle',data$name)[1]]
    scanRate.Hz = 1/as.numeric(gsub('(\\d+).*','\\1',data$value[grep('Time/Line',data$name)[1]]))
    note = ""
  } else {  # Veeco
    data = read.Nanoscope_file(filename, headerOnly=TRUE)
    type = 'Veeco'
    widthPixel = as.numeric(data$value[grep('Valid data len X',data$name)[1]])
    heightPixel = as.numeric(data$value[grep('Valid data len Y',data$name)[1]])
    scanAngle = data$value[grep('Feature scan angle',data$name)[1]]
    scanRate.Hz = data$value[grep('Scan rate',data$name)[1]]
    note = data$value[grep('Note',data$name)[1]]
  }
  structure(
    list(
      data = data,
      type = type,
      filename = filename,
      widthPixel = widthPixel,
      heightPixel = heightPixel,
      scanRate.Hz = scanRate.Hz,
      note = note
    ),
    class = 'AFMinfo'
  )
}

#' returns an item from the AFMinfo object
#'
#' @param obj AFMinfo object
#' @param itemName name to retrieve (ScanRate, ScanAngle, ...)
#' @return string or number
#' @author Thomas Gredig
#' @examples
#' filename = system.file("extdata","Veeco_20160622.003",package="nanoscopeAFM")
#' h = AFMinfo(filename)
#' AFMinfo.item(h,"Description")
#' AFMinfo.item(h,"Operating mode")
#' @export
AFMinfo.item <- function(obj, itemName) {
  q = grep(itemName,obj$data$name)
  itemValue = ''
  if (length(q)>0) {
    itemValue = obj$data$value[q[1]]
  }
  itemValue
}


#' summary for AFMinfo object
#'
#' @param obj AFMinfo object
#' @return quick summary
#' @author Thomas Gredig
#' @examples
#' h1 = AFMinfo(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' summary(h1)
#' @export
summary.AFMinfo <- function(obj) {
  cat('AFM image type:  ', obj$type, "\n")
  cat('Resolution:      ', obj$widthPixel, "x", obj$heightPixel,'\n')
  cat('Scan rate (Hz):  ', obj$scanRate.Hz,'\n')
  cat('Scan Angle (deg):', obj$scanAngle,'\n')
  cat('Notes:           ', obj$note,'\n')
  cat('Data Items:      ', nrow(obj$data),'\n')
}
