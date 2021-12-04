#' Various computed AFM image parameters
#'
#' @param obj AFMdata class
#' @return structure with various computed AFM image parameters, such
#' as roughness
#' @examples
#' filename = system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM")
#' AFM.math.params(AFM.import(filename))
#' @export
AFM.math.params <- function(obj) {
  Ra = get.Ra(obj@data$z[[1]])
  Rq = get.Rq(obj@data$z[[1]])
  structure(
    list(
      basename = basename(obj@fullfilename),
      Ra = Ra,
      Rq = Rq
    ),
    class = 'AFMmath'
  )
}

#' Summary of computed AFM image parameters
#'
#' @param obj AFMmath object
#' @return prints a summary
#' @examples
#' filename = system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM")
#' summary(AFM.math.params(AFM.import(filename)))
#' @export
summary.AFMmath <- function(obj) {
  cat("Basename:      ", obj$basename,"\n")
  cat("Roughness Ra = ", obj$Ra," nm \n")
  cat("Roughness Rq = ", obj$Rq," nm \n")
}


#
NULL

# computes the average roughness Ra
get.Ra <- function(z) { sum(abs(z-mean(z)))/length(z) }
# computes the root mean square roughness Rq
get.Rq <- function(z) { sqrt(sum((z-mean(z))^2)/length(z)) }
