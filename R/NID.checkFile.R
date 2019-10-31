#' checks AFM NID file length
#'
#' @param filename filename including path
#' @return mismatch in image size + header with file size (should be 0)
#' @examples
#' filename = dir(pattern='nid$', recursive=TRUE)[1]
#' d = NID.checkFile(filename)
#' @export
NID.checkFile <- function(filename) {
  # does file exist?
  if (!file.exists(filename)) return -1
  # length of file in bytes
  file.len = file.info(filename)$size

  # read header
  h = read.NID_header(filename)
  # get header length in bytes
  header.length = h[[1]]

  # get number of images and size of images
  q = get.NID_imageInfo(h[[2]])

  # compare file length with images + header +
  # 2 bytes for #! character, which is the beginning
  # of the images
  file.len - sum(q*q)*2 - header.length - 2
}


