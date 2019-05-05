# ######################################
# read NID file, AFM file
#
# Date: 2019-02-10
# Author: Thomas Gredig
#
# ######################################
#
# # Nanosurf image data format (NID format)
# # from easyscan AFM
#
#
# # read the header of the NID AFM files
# # seems header file ends with "" (empty) or
# # with #!F
# ######################################

#' loads header of AFM NID file
#'
#' @param filename filename including path
#' @return list with length in bytes and header as text
#' @examples
#' d = read.NID_header('example.nid')
#' @export
read.NID_header <- function(filename) {
  Sys.setlocale('LC_ALL','en_US')
  con <- file(filename,"rb")
  rline = ''
  i=0
  dlen.header = 0
  while( TRUE ) {
    rline = readLines(con,n=1)
    if (substr(rline,1,2) == "#!" ) break
    i = i + 1
    dlen.header = dlen.header + nchar(rline, type="bytes") + 2
  }
  close(con)

  con <- file(filename,"rb")
  header = readLines(con, n=(i-1))
  close(con)

  list(header.len = dlen.header, header = header)

  # dlen.header
  # sum(nchar(header, type='bytes')) + 2*length(header)
  #
  #
  # # read header, followed by image
  #
  # dlen.header=dlen.header+2
  # con <- file(fname,"rb")
  # bin.header <- readBin(con, integer(),  n = dlen.header, size=1, endian = "little")
  # bin.data   <- readBin(con, integer(),  n = 256*256, size=2, endian = "little")
  # close(con)
  # as.hexmode(bin.header[1:4])
  # # make a graph of the image
  # library(raster)
  # m1 = matrix(bin.data[1:(128*128)], ncol=128)
  # str(m1)
  # r1 = raster(t(m1))
  # plot(r1)
}

# read header, followed by image
# con <- file(fname,"rb")
# bin.header <- readBin(con, integer(),  n = dlen.header, size=1, endian = "little")
# bin.data   <- readBin(con, integer(),  n = 256*256, size=2, endian = "little")
# close(con)
#


#' loads images of AFM NID file
#'
#' @param filename filename including path
#' @return images
#' @examples
#' d = read.NID_header('example.nid')
#' @export
read.NID_file <- function(filename) {
  if (check.NID_file(filename) == 0) {
    h = read.NID_header(filename)
    q = get.NID_imageInfo(h)

    header.length = h[[1]]
    con <- file(fname,"rb")
    bin.header <- readBin(con, integer(),  n = header.length, size=1, endian = "little")
    bin.ID = readBin(con, integer(),  n = 2, size=1, endian = "little")
    r = list(bin.header, bin.ID)
    for(i in 1:length(q)) {
      bin.data <- readBin(con, integer(),  n = q[i]*q[i], size=2, endian = "little")
      r = c(r, bin.data)
    }
    close(con)
  }
  r
}
