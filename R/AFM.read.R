#' loads AFM image files from different instruments and stores 6 rows (x,y,z) and
#' converted units, when possible (xc,yc,zc), use the attributes to find the units and
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
    } else if (fext=='nid') {
      df = read.NanoSurf_file(filename,no)
    } else if (fext=='tiff') {
      df = read.Park_file(filename)
    } else {
      df = read.Nanoscope_file(filename,no)
    }
    attr(df,'filename')=basename(filename)
  }  else { warning(paste("File does not exist:",filename)) }
  df
}
