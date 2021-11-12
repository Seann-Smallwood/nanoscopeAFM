#' find AFM image information
#'
#' @param filename filename of (Veeco, Park, AR, NanoSurf) AFM image including path
#' @param no channel number (for Veeco, NanoSurf, AR)
#' @return returns gerenal information about the AFM image
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = AFM.info(filename)
#' @export
AFM.info <- function(filename, no=1) {
  r = c()  # associative array, all common tags start with INFO (mandatory), others are optional
  if (file.exists(filename)) {
    r=c(r,INFO.noImages=1)
    # get file extension
    fext = tolower(tools::file_ext(filename))
    if (fext=='ibw') {
      # ASYLUM RESEARCH
      # ===============
      r=c(r,INFO.type='AR')
    } else if (fext=='nid') {
      # NANOSURF
      # ===============
      r=c(r,INFO.type='NanoSurf')
    } else if (fext=='tiff') {
      # PARK
      # ===============
      r=c(r,INFO.type='Park')

      # read TIFF tags
      tiffTags = tagReader(filename)
      afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
      params = get.ParkAFM.header(afm.params)
      r = c(r,params)

      # mandatory tags:
      r=c(r,INFO.widthPixel=as.numeric(r['nWidth']))
      r=c(r,INFO.heightPixel=as.numeric(r['nHeight']))
      r=c(r,INFO.widthNanometer=as.numeric(r['dfXScanSizeum'])*1000)
      r=c(r,INFO.heightNanometer=as.numeric(r['dfYScanSizeum'])*1000)
      r=c(r,INFO.scanRateHz=as.numeric(r['dfScanRateHz']))
    } else {
      # VEECO
      # ===============
      r=c(r,INFO.type='Veeco')
    }
  } # else { warning(paste("File does not exist:",filename)) }
  r
}
