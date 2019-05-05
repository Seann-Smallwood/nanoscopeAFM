#' returns number of lines for each image
#'
#' @param header.string header string of NID file (use read.NID_header)
#' @return vector with line numbers for each image
#' @examples
#' lines = check.NID_file(header.string)
#' @export
get.NID_imageInfo <- function(header.string) {
  # split data sets
  from = grep('\\[DataSet-',header.string)
  to = c(from[-1]-1, length(header.string))

  itemslist <- mapply(
    function(x, y) return(header.string[x:y]),
    x = from, y = to,
    SIMPLIFY = FALSE
  )
  itemslist[[1]] <- NULL

  image.Lines <- lapply(itemslist,
                   function(x) {
                     x[grep('Lines',x)]
                   }
  )

  as.numeric(
    unlist(lapply(image.Lines, function(x) { sapply(strsplit(x,"="),'[[',2) }))
  )
}
