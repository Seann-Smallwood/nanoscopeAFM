#' loads images of AFM Nanoscope file (use Nanoscope.loadImage whenever)
#'
#' @param filename filename including path
#' @return list with header, file ID, and images
#' @examples
#' filename = dir(pattern='000$', recursive=TRUE)[1]
#' d = read.Nanoscope_file(filename)
#' @export
read.Nanoscope_file <- function(filename) {
  # read the header of the AFM file
  q = read.Nanoscope_header(filename)
  # retrieve header length + file image lengths
  dlen.num = as.numeric(q$value[grepl('Data length',q$name)])
  # load components
  con <- file(fname,"rb")
  bin.header <- readBin(con, integer(),  n = dlen.num[1], size=1, endian = "little")
  imageNo = 1
  r = list()
  while(imageNo < length(dlen.num)) {
    bin.data   <- readBin(con, integer(),  n = dlen.num[imageNo+1]/2, size=2, endian = "little")
    r[[imageNo]] = bin.data
    imageNo = imageNo+1
  }
  close(con)
  r
}
