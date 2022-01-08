#' sample AFM images
#'
#' @description
#' returns sample AFM images from library
#' @param type could be "ibw" for Asylum, "nid" for NanoSurf, "tiff" for Park, if empty or "*", all files will be returned
#' @return vector with path/filename to AFM sample images
#' @author Thomas Gredig
#' @examples
#' file.list = AFM.getSampleImages()
#' print(paste("Found",length(file.list),"sample files."))
#' @export
AFM.getSampleImages <- function(type='*') {
  pfad = system.file("extdata",package="nanoscopeAFM")
  file.list = dir(system.file("extdata",package="nanoscopeAFM"), pattern=paste0(type,"$"))
  file.path(pfad, file.list)
}
