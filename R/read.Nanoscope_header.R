#' Returns the header of a NanoScope AFM file
#'
#' @param filename filename including path
#' @return data frame with name / value columns
#' @examples
#' filename = dir(pattern='000$', recursive=TRUE)[1]
#' h = read.Nanoscope_header(filename)
#' @export
read.Nanoscope_header <- function(filename) {
  if (!file.exists(filename)) { warning(paste("File",filename,"does NOT exist.")) }
  first_line = ''
  header = c()
  con <- file(filename,"rb")
  i=0
  while(first_line != "\\*File list end" ) {
    first_line <- readLines(con,n=1)
    header=c(header, first_line)
    i=i+1
  }
  close(con)
  header = gsub('\\\\','',header)
  header[grep('^\\*',header)] = paste0(header[grep('^\\*',header)],":NEW SECTION")
  p1 = strsplit(header,':')
  data.frame(
    name = unlist(lapply(p1,"[[",1)),
    value = unlist(lapply(p1,"[[",2)),
    stringsAsFactors = FALSE
  )
}
