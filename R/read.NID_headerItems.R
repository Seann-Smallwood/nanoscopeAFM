#' loads header of NID file and separates into items
#'
#' @param filename filename including path
#' @return list
#' @examples
#' filename = dir(pattern='nid$', recursive=TRUE)[1]
#' read.NID_headerItems(filename)
#' @export
read.NID_headerItems <- function(filename) {
  # read the NID header
  k1 = read.NID_header(filename)[[2]]
  # separate groups
  from = grep("^\\[.*\\]$",k1) + 1
  to = c(from[-1]-1, length(k1))
  itemslist <- mapply(
    function(x, y) return(k1[x:y]),
    x = from, y = to - 1,
    SIMPLIFY = FALSE
  )
  # add list with titles
  c(list(c('',k1[from-1])),itemslist)
}
