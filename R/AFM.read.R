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
      r = AFM.info(filename)
      attr(df,"instrument")='Veeco'
      attr(df,"channelDirection") = r['Frame direction']
      attr(df,"note")=r['Note']
      attr(df,"channel")= r['Data type']   # not sure
    }
  }  else { warning(paste("File does not exist:",filename)) }
  df
}
