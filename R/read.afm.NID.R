###########################
# read NID file, AFM file
#
# Date: 2019-02-10
# Version: 0.1
# Author: Thomas Gredig
#
#########################

Sys.setlocale('LC_ALL','en_US')


# define the path and search for AFM files
##########################################
# source.path = './data'
# dir(source.path)
# my.pattern = '.*nid$'
# file.list = dir(source.path, pattern=my.pattern, recursive=TRUE)
# print(paste("Found",length(file.list),"AFM files."))
#
#
# # Nanosurf image data format (NID format)
# # from easyscan AFM
#
#
# # read the header of the NID AFM files
# # seems header file ends with "" (empty) or
# # with #!F
# ######################################
#
# f = file.path(source.path, file.list[1])
#

#' loads header of AFM NID file
#'
#' @param filename filename including path
#' @return data frame with header
#' @examples
#' d = read.afm.NID('example.nid')
#' @export
read.afm.NID <- function(filename) {
  dlen.header = 0
  fname = filename
  first_line = ''
  header = c()
  con <- file(fname,"rb")
  i=0
  #while(substr(first_line,1,3) != "#!F" ) {
  while(i<2 || first_line != "") {
    first_line <- readLines(con,n=1)
    header=c(header, first_line)
    i=i+1
    # include \n as +1
    first_line = gsub('\xb5','mu',first_line)
    first_line = gsub('\xb0','o',first_line)
    dlen.header = dlen.header + nchar(first_line) + 2
  }

  #print(paste('header lines:',i))
  header
}

# header = read.afm.NID(f)



