#' loads image file
#'
#' @param filename filename of AFM image raw data including path
#' @param no number of the channel
#' @return AFM image with attributes
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = read.AFM_file(filename)
#' @export
read.AFM_file <- function(filename, no=1) {
  # does file exist?
  if (!file.exists(filename)) {
    warning(paste("File does not exist:",filename))
    df = NULL
  }

  # get file extension
  fext = tolower(tools::file_ext(filename))
  if (fext=='ibw') {
    df = read.IBW_file(filename,no)
  } else if (fext=='nid') {
    df = read.NID_file(filename)
  } else {
    df = read.Nanoscope_file(filename)
  }
  attr(df,'filename')=basename(filename)
  df$x.nm = attr(df, "convFactor") * df$x * 1e6
  df$y.nm = attr(df, "convFactor") * df$y * 1e6
  df
}
