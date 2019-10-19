#' returns the scaled image + flattend image
#'
#' @param filename filename including path
#' @param imageNo a number of the image, 1,2,3,4
#' @return data.frame with four components, x and y-axes
#'         and the z-axis (original) and flattened
#' @examples
#' filename = dir(pattern='nid$', recursive=TRUE)[1]
#' d = NID.loadImage(filename)
#' ggplot(d, aes(x,y,fill=z.flatten)) + geom_raster()
#' @export
NID.loadImage <- function(filename,imageNo) {
  # read header information
  h = read.NID_headerItems(filename)
  # read all images
  d = read.NID_file(filename)

  # get scaling for image
  s1 = get.NIDchannel.Scale(h,imageNo)

  # create the rastering sequences for x-, y-axes
  # and convert pixels from z-axis into scale (m or V)
  seq1 = seq(from=s1$from[1], to=s1$to[1], length.out = s1$length[1])
  seq2 = seq(from=s1$from[2], to=s1$to[2], length.out = s1$length[2])
  range.z = s1$to[3]-s1$from[3]

  # create a data frame with the AFM image
  dr = data.frame(x=rep(seq1,each=s1$length[2]),
                  y=rep(seq2,times=s1$length[1]),
                  z=d[[imageNo]]/s1$length[3]*range.z+s1$from[3])

  # render image as follows:
  # ggplot(dr, aes(x,y, fill=z)) +  geom_raster()

  m = matrix(dr$z,nrow=s1$length[1])
  m2 = flatten.NID_matrix(m)
  dr$z.flatten = as.vector(m2)
  dr$z.flatten = dr$z.flatten-min(dr$z.flatten)

  dr
}
