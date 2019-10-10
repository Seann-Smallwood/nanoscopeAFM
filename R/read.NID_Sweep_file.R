#' loads frequency sweep data from NID file
#'
#' @param filename filename including path
#' @return list with data frames that have freq vs. ampl.
#' @examples
#' filename = dir(pattern='nid$', recursive=TRUE)[1]
#' d = read.NID_Sweep_file(filename)
#' @export
read.NID_Sweep_file <- function(filename) {
  # read header
  h = read.NID_header(filename)
  k1= h[[2]]
  # find lengths of frequency sweep data
  k2 = k1[grep('Points=', k1)]
  q = as.numeric(gsub('\\D','\\1',k2))
  k3 = k1[grep('Dim0Min=', k1)]
  freq.min = as.numeric(gsub('\\D','\\1',k3))
  k3 = k1[grep('Dim0Range=', k1)]
  freq.range = as.numeric(gsub('\\D','\\1',k3))


  header.length = h[[1]]
  con <- file(filename,"rb")
  bin.header <- readBin(con, integer(),  n = header.length, size=1, endian = "little")
  bin.ID = readBin(con, integer(),  n = 2, size=1, endian = "little")
  #r = list(header = bin.header, ID = bin.ID)
  r = list()

  if (sum(bin.ID) == sum(c(35,33))) {
    if(length(q)>0) {
      for(i in 1:length(q)) {
        if(q[i]>0) {
          bin.data <- readBin(con, integer(),  n = q[i], size=2, endian = "little")
          r[[i]] = data.frame(x = seq(from=freq.min[i], to=freq.min[i] + freq.range[i], length.out=q[i]),
                              y = bin.data)
        }
      }
    }
  }
  close(con)
  r
}
