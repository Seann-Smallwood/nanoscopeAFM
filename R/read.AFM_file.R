#' (DEPRACATED): loads AFM image file
#'
#' @param filename filename of (Veeco, Park, AR, NanoSurf) AFM image including path
#' @param no channel number (for Veeco, NanoSurf, AR)
#' @return AFM image with attributes
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = read.AFM_file(filename)
#' @export
read.AFM_file <- function(filename, no=1) {
  warning('Depracated function: use AFM.read()')
  # does file exist?
  AFM.read(filename, no)
}
