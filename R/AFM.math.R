#' Various computed AFM image parameters
#'
#' @param obj AFMdata class
#' @return structure with various computed AFM image parameters, such
#' as roughness
#' @examples
#' filename = AFM.getSampleImages(type='ibw')
#' AFM.math.params(AFM.import(filename))
#' @export
AFM.math.params <- function(obj) {
  Ra = get.Ra(obj@data$z[[1]])
  Rq = get.Rq(obj@data$z[[1]])
  structure(
    list(
      basename = basename(obj@fullFilename),
      Ra = Ra,
      Rq = Rq
    ),
    class = 'AFMmath'
  )
}

#' Summary of computed AFM image parameters
#'
#' @param object AFMmath object
#' @param ... other arguments
#' @return prints a summary
#' @examples
#' filename = AFM.getSampleImages(type='ibw')
#' summary(AFM.math.params(AFM.import(filename)))
#' @export
summary.AFMmath <- function(object, ...) {
  cat("Basename:      ", object$basename,"\n")
  cat("Roughness Ra = ", object$Ra," nm \n")
  cat("Roughness Rq = ", object$Rq," nm \n")
}


#
NULL

# computes the average roughness Ra
get.Ra <- function(z) { sum(abs(z-mean(z)))/length(z) }
# computes the root mean square roughness Rq
get.Rq <- function(z) { sqrt(sum((z-mean(z))^2)/length(z)) }
