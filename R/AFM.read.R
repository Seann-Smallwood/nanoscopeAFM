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
#' d = AFM.read(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
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
      h = AFM.info(filename)
      attr(df,"channel") =h['Op. mode']
    } else if (fext=='tiff') {
      df = read.Park_file(filename)
      AFM.info(filename) -> h1
      attr(df,"channel")=h1$sourceName
      attr(df,"instrument")='Park'
    } else {
      df = read.Nanoscope_file(filename,no)
      attr(df,"instrument")='Veeco'
    }
  }  else { warning(paste("File does not exist:",filename)) }
  df
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
