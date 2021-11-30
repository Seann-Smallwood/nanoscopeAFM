#' AFMinfo object with information about the AFM image
#'
#' @param filename filename of (Veeco, Park, AR, NanoSurf) AFM image including path
#' @return AFMinfo object
#' @author Thomas Gredig
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
#' @param AFMinfo object
#' @param itemName name to retrieve (ScanRate, ScanAngle, ...)
#' @return string or number
#' @author Thomas Gredig
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
#' @export
summary.AFMinfo <- function(obj) {
  cat('AFM image type:', obj$type, "\n")
  cat('Resolution:    ', obj$widthPixel, "x", obj$heightPixel,'\n')
  cat('Scan rate (Hz):', obj$scanRate.Hz,'\n')
  cat('Notes:         ', obj$note,'\n')
  cat('Data Items:    ', nrow(obj$data),'\n')
}

#' find AFM image information, much of the information is specific to the
#' AFM instrument; common pieces are included with the tag INFO
#'
#' @param filename filename of (Veeco, Park, AR, NanoSurf) AFM image including path
#' @param no channel number (for Veeco, NanoSurf, AR)
#' @param fullInfo if \code{TRUE}, return all full header info
#' @return returns information about the AFM image as an associative vector
#' @author Thomas Gredig
#' @examples
#' h1 = AFM.info(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' @export
AFM.info <- function(filename, no=1, fullInfo=FALSE) {
  r = c()  # associative array, all common tags start with INFO (mandatory), others are optional
  if (file.exists(filename)) {
    r=c(r,INFO.noImages=1)
    # get file extension
    fext = tolower(tools::file_ext(filename))
    if (fext=='ibw') {
      # ASYLUM RESEARCH
      # ===============
      r=c(r,INFO.type='AR')
      r=c(r, read.AR_header(filename, no))
      scanRate=gsub('(.*)@.*','\\1',r['ScanRate'])

      # mandatory tags:
      r=c(r,INFO.widthPixel=as.numeric(r['PointsLines']))
      r=c(r,INFO.heightPixel=as.numeric(r['ScanLines']))
      r=c(r,INFO.width.nm=as.numeric(r['FastScanSize']))
      r=c(r,INFO.height.nm=as.numeric(r['SlowScanSize']))
      r=c(r,INFO.scanRate.Hz=scanRate)
      r=c(r,INFO.vibrationFreq.Hz=as.numeric(r['ResFreq1']))
      r=c(r,INFO.note=as.character(r['ImageNote']))

      # ===============
    } else if (fext=='nid') {
      # NANOSURF
      # ===============
      r=c(r,INFO.type='NanoSurf')
      r=c(r, read.NanoSurf_header(filename, no))
      gsub('µm','000', r['Image size']) -> width.nm
      gsub('nm','', r['Image size']) -> width.nm

      # mandatory tags:
      r=c(r,INFO.widthPixel=as.numeric(r['Points']))
      r=c(r,INFO.heightPixel=as.numeric(r['Lines']))
      r=c(r,INFO.width.nm=width.nm)
      r=c(r,INFO.height.nm=width.nm)
      r=c(r,INFO.scanRate.Hz=1/as.numeric(gsub('s','',r['Time/Line'])))
      r=c(r,INFO.vibrationFreq.Hz=as.numeric(r['Vibration freq']))
      r=c(r,INFO.note='')

      # ===============
    } else if (fext=='tiff') {
      # PARK
      # ===============
      r=c(r,INFO.type='Park')
      tiffTags = tagReader(filename)
      afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
      params = get.ParkAFM.header(afm.params)
      r = c(r,params)

      # mandatory tags:
      r=c(r,INFO.widthPixel=as.numeric(r['nWidth']))
      r=c(r,INFO.heightPixel=as.numeric(r['nHeight']))
      r=c(r,INFO.width.nm=as.numeric(r['dfXScanSizeum'])*1000)
      r=c(r,INFO.height.nm=as.numeric(r['dfYScanSizeum'])*1000)
      r=c(r,INFO.scanRate.Hz=as.numeric(r['dfScanRateHz']))
      r=c(r,INFO.vibrationFreq.Hz=NA)
      r=c(r,INFO.note='')

      # ===============
    } else {
      # VEECO
      # ===============
      r=c(r,INFO.type='Veeco')
      r = c(r, read.Nanoscope_file(filename, no, headerOnly=TRUE) )
      wh = strsplit(gsub('\\s*(.*)\\s.*','\\1', r['Scan size'])," ")[[1]]

      # mandatory tags:
      r=c(r,INFO.widthPixel=as.numeric(r['Samps/line2']))
      r=c(r,INFO.heightPixel=as.numeric(r['Number of lines']))
      r=c(r,INFO.width.nm=as.numeric(wh[1])*1000)
      r=c(r,INFO.height.nm=as.numeric(wh[2])*1000)
      r=c(r,INFO.scanRate.Hz=as.numeric(r['Scan rate']))
      r=c(r,INFO.vibrationFreq.Hz=NA)
      r=c(r,INFO.note='')

      # ===============
    }
  } # else { warning(paste("File does not exist:",filename)) }
  r
}
