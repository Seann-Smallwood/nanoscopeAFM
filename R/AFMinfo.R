#' AFMinfo object
#'
#' header information about the AFM image, includes scan rate, widths, etc.
#'
#' @param filename full file name of instrument AFM file
#' @return AFMinfo class object
#' @author Thomas Gredig
#' @examples
#' filename = AFM.getSampleImages(type='003')
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
    noChannels = 8 - sum(data$value[grep('^Channel\\d',data$name)]==" None")
  } else if (fext=='tiff') {
    data = read.Park_header.v2(filename)
    type = 'Park'
    widthPixel = data$value[grep('nWidth',data$name)[1]]
    heightPixel = data$value[grep('nHeight',data$name)[1]]
    scanAngle = data$value[grep('dfAngle',data$name)[1]]
    scanRate.Hz = data$value[grep('dfScanRateHz',data$name)[1]]
    note = data$value[grep('ImageNote',data$name)[1]]
    noChannels = 1
  } else if (fext=='nid') {
    data = read.NanoSurf_header.v2(filename)
    type = 'NanoSurf'
    widthPixel = data$value[grep('Points',data$name)[1]]
    heightPixel = data$value[grep('Lines',data$name)[1]]
    scanAngle = data$value[grep('dfAngle',data$name)[1]]
    scanRate.Hz = 1/as.numeric(gsub('(\\d+).*','\\1',data$value[grep('Time/Line',data$name)[1]]))
    vibrationFreq.Hz = as.numeric(gsub('kHz','',data$value[grep('Vibration freq',data$name)[1]]))*1000
    note = ""
    noChannels = NA
  } else {  # Veeco
    data = read.Nanoscope_file(filename, headerOnly=TRUE)
    type = 'Veeco'
    widthPixel = as.numeric(data$value[grep('Valid data len X',data$name)[1]])
    heightPixel = as.numeric(data$value[grep('Valid data len Y',data$name)[1]])
    scanAngle = data$value[grep('Feature scan angle',data$name)[1]]
    scanRate.Hz = data$value[grep('Scan rate',data$name)[1]]
    note = data$value[grep('Note',data$name)[1]]
    noChannels = NA
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

#' Specific information about AFM image
#'
#' Retrieve a specific information piece from the AFM image,
#' such as "ScanRate", "Description", etc. The item names depend
#' on the instrument; to find possible item names, leave
#' \code{itemName} empty.
#'
#' @param obj AFMinfo object
#' @param itemName name to retrieve (ScanRate, ScanAngle, ...)
#' @return if \code{itemName} is empty, returns list of names, otherwise the value for the specific data item
#' @author Thomas Gredig
#' @examples
#' filename = AFM.getSampleImages(type='003')
#' h = AFMinfo(filename)
#' allNames = AFMinfo.item(h)
#' AFMinfo.item(h,"Description")
#' AFMinfo.item(h,"Operating mode")
#' print(allNames)
#' @export
AFMinfo.item <- function(obj, itemName="") {
  q = grep(itemName,obj$data$name)
  if(itemName=="") { # return item names
    itemValue = obj$data$name
  } else { # return value of item
    itemValue = ''
    if (length(q)>0) {
      itemValue = obj$data$value[q[1]]
    }
  }
  itemValue
}


#' summary for AFMinfo object
#'
#' @param obj AFMinfo object
#' @return quick summary
#' @author Thomas Gredig
#' @examples
#' h1 = AFMinfo(AFM.getSampleImages(type='ibw')[1])
#' summary(h1)
#' @export
summary.AFMinfo <- function(obj) {
  cat('AFM image type:  ', obj$type, "\n")
  cat('Resolution:      ', obj$widthPixel, "x", obj$heightPixel,'\n')
  cat('Channels:        ', obj$noChannels,'\n')
  cat('Scan rate (Hz):  ', obj$scanRate.Hz,'\n')
  cat('Scan Angle (deg):', obj$scanAngle,'\n')
  cat('Notes:           ', obj$note,'\n')
  cat('Data Items:      ', nrow(obj$data),'\n')
}
