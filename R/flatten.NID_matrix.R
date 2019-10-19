#' flattens a NID AFM image using a plane fit
#'
#' @param m matrix with AFM image
#' @return flattened matrix with AFM image
#' @examples
#' filename = dir(pattern='nid$', recursive=TRUE)[1]
#' d = read.NID_file(filename)
#' m = matrix(d[[1]],nrow=256, ncol=256)
#' flatten.NID_matrix(m)
#' @export
flatten.NID_matrix <- function(m) {
  z1 = as.vector(m)
  x1 = rep(1:nrow(m), each=ncol(m))
  y1 = rep(1:ncol(m), nrow(m))

  b = c(sum(x1*z1), sum(y1*z1), sum(z1))
  a = matrix(data = c(sum(x1*x1), sum(x1*y1), sum(x1),
                      sum(x1*y1), sum(y1*y1), sum(y1),
                      sum(x1), sum(y1), length(m)),
             nrow=3)
  x = solve(a,b)
  matrix(x1*x[1] + y1*x[2] + x[3] - z1, nrow=nrow(m))
}
