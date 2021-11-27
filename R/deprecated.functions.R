# DEPRECATED functions, kept for backwards compatibility
# =====================================================-

#' DEPRECATEDD: use AFM.flatten() instead:
#' flattens a NID AFM image using a plane fit
#'
#' @param m matrix with AFM image
#' @return flattened matrix with AFM image
#' @author thomasgredig
#' @examples
#' filename = system.file("extdata","NanoSurf_20160301.nid",package="nanoscopeAFM")
#' d = read.NID_file(filename)
#' m = matrix(d[[1]],nrow=256, ncol=256)
#' flatten.NID_matrix(m)
#' @export
flatten.NID_matrix <- function(m) {
  warning('Deprecated: use AFM.flatten()')
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


#' DEPRACATED: returns the scaled image + flattend image
#'
#' @param filename filename including path
#' @param imageNo a number of the image, 1,2,3,4
#' @return data.frame with four components, x and y-axes
#'         and the z-axis (original) and flattened
#' @examples
#' filename = system.file("extdata","NanoSurf_20160301.nid",package="nanoscopeAFM")
#' d = NID.loadImage(filename,1)
#' ggplot(d, aes(x,y,fill=z.flatten)) + geom_raster()
#' @export
NID.loadImage <- function(filename,imageNo=1) {
  warning('Deprecated: use AFM.read()')
  if (!file.exists(filename)) warning(paste("File",filename,"does NOT exist."))
  # read header information
  h = read.NID_headerItems(filename)
  # read all images
  d = read.NID_file(filename)

  # get scaling for image
  s1 = NID.getChannelScale(h,imageNo)

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



#' DEPRECATED: use AFM.read()
#' loads AFM image file
#'
#' @param filename filename of (Veeco, Park, AR, NanoSurf) AFM image including path
#' @param no channel number (for Veeco, NanoSurf, AR)
#' @return AFM image with attributes
#' @examples
#' d = read.AFM_file(system.file("extdata","NanoSurf_20160301.nid",package="nanoscopeAFM"))
#' @export
read.AFM_file <- function(filename, no=1) {
  warning('Depracated function: use AFM.read()')
  # does file exist?
  AFM.read(filename, no)
}


#' DEPRECATED: use read.AR_file()
#' loads Asylum Research Igor Wave AFM files
#'
#' @param filename filename including path
#' @param no number of the channel
#' @return image with attributes
#' @examples
#' d = read.IBW_file(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' @export
read.IBW_file <- function(filename, no=1) {
  warning("DEPRECATED: use read.AR_file() instead")
  d = IgorR::read.ibw(filename)
  q2 = attr(d, "WaveHeader")
  x = 1:dim(d)[1]
  y = 1:dim(d)[2]
  z = 1:(length(x)*length(y)) + (no-1)*(length(x)*length(y))
  dr = data.frame(
    x = rep(x, length(y)),
    y = rep(y, each=length(x)),
    z.nm = d[z]*1e9
  )
  dr$z.nm=dr$z.nm-min(dr$z.nm)
  attr(dr,"imageDim") <- q2$nDim[1:2]
  attr(dr,"noChannels") <- q2$nDim[3]
  sfA = q2['sfA']$sfA
  attr(dr,"convFactor") <- sfA[no]
  attr(dr,"Units") <- q2$dimUnits[no]
  dr
}

#' DEPRECATED: use AFM.read()
#' loads images of AFM NID file (use NID.loadImage whenever)
#'
#' @param filename filename including path
#' @return list with header, file ID, and images
#' @examples
#' d = read.NID_file(system.file("extdata","NanoSurf_20160301.nid",package="nanoscopeAFM"))
#' @export
read.NID_file <- function(filename) {
  warning('DEPREACTED: use read.NanoSurf_file()')
  if (NID.checkFile(filename) == 0) {
    h = read.NID_header(filename)
    q = get.NID_imageInfo(h[[2]])

    header.length = h[[1]]
    con <- file(filename,"rb")
    bin.header <- readBin(con, integer(),  n = header.length, size=1, endian = "little")
    bin.ID = readBin(con, integer(),  n = 2, size=1, endian = "little")
    #r = list(header = bin.header, ID = bin.ID)
    r = list()

    if (sum(bin.ID) == sum(c(35,33))) {
      if(length(q)>0) {
        for(i in 1:length(q)) {
          bin.data <- readBin(con, integer(),  n = q[i]*q[i], size=2, endian = "little")
          r[[i]] = bin.data
        }
      }
    }
    close(con)
  }
  r
}

