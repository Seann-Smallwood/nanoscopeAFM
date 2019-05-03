###########################
# read NID file, AFM file
#
# Date: 2019-02-10
# Version: 0.1
# Author: Thomas Gredig
#
#########################


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
#' @return data frame with header
#' @examples
#' d = read.NID_header('example.nid')
#' @export
read.NID_header <- function(filename) {
  Sys.setlocale('LC_ALL','en_US')
  con <- file(fname,"rb")
  rline = ''
  i=0
  while(substr(rline,1,2) != "#!" ) {
    rline = readLines(con,n=1)
    i = i+1
  }
  close(con)
  con <- file(fname,"rb")
  header = readLines(con, n=(i-1))
  close(con)

  header
}





