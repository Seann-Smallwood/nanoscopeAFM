#' sample AFM images
#'
#' @description
#' returns sample AFM images from library
#' @return vector with path/filename to AFM sample images
#' @author Thomas Gredig
#' @examples
#' file.list = AFM.getSampleImages()
#' print(file.list)
#' @export
AFM.getSampleImages <- function() {
  pfad = system.file("extdata",package="nanoscopeAFM")
  file.list = dir(system.file("extdata",package="nanoscopeAFM"))
  file.path(pfad, file.list)
}
