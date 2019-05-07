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

empty.header = list(header.len = 0, header = c(''))

#' loads header of AFM NID file
#'
#' @param filename filename including path
#' @return list with length in bytes and header as text
#' @examples
#' filename = dir(pattern='nid$', recursive=TRUE)[1]
#' read.NID_header(filename)
#' @export
read.NID_header <- function(filename) {
  if (!file.exists(filename)) { return(empty.header) }
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
}


