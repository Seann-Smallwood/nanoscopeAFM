#' loads AFM image file
#'
#' @param filename filename of (Veeco, Park, AR, NanoSurf) AFM image including path
#' @param no channel number (for Veeco, NanoSurf, AR)
#' @return AFM image with attributes
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = AFM.read(filename)
#' @export
AFM.read <- function(filename, no=1) {
  # does file exist?
  df = NULL
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
    # df$x.nm = attr(df, "convFactor") * (df$x-1) * 1e9
    # df$y.nm = attr(df, "convFactor") * (df$y-1) * 1e9
  } # else { warning(paste("File does not exist:",filename)) }
  df
}
