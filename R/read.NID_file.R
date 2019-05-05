#' loads images of AFM NID file
#'
#' @param filename filename including path
#' @return list with header, file ID, and images
#' @examples
#' d = read.NID_file('example.nid')
#' @export
read.NID_file <- function(filename) {
  if (check.NID_file(filename) == 0) {
    h = read.NID_header(filename)
    q = get.NID_imageInfo(h[[2]])

    header.length = h[[1]]
    con <- file(filename,"rb")
    bin.header <- readBin(con, integer(),  n = header.length, size=1, endian = "little")
    bin.ID = readBin(con, integer(),  n = 2, size=1, endian = "little")
    r = list(header = bin.header,
             ID = bin.ID)

    j = 3
    if (sum(bin.ID) == sum(c(35,33))) {
      if(length(q)>0) {
        for(i in 1:length(q)) {
          bin.data <- readBin(con, integer(),  n = q[i]*q[i], size=2, endian = "little")
          r[[j]] = bin.data
          j = j + 1
        }
      }
    }
    close(con)
  }
  r
}
