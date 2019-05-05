#' checks AFM NID file length
#'
#' @param filename filename including path
#' @return true or false
#' @examples
#' d = check.NID_file('example.nid')
#' @export
check.NID_file <- function(filename) {
  # does file exist?
  if (!file.exists(filename)) FALSE
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


