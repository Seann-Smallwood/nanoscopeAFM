#' loads images
#'
#' @param filename Park AFM filename including path
#' @return image with attributes
#' @examples
#' d = read.Park_file(filename)
#' @export
read.Park_file <- function(filename) {
  # read TIFF tags
  tiffTags = tagReader(filename)
  afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
  params = get.ParkAFM.header(afm.params)

  # check that the file can be displayed
  if (!tiff.isPaletteColorImage(tiffTags)) stop("Not a palette color image.")
  if (!tiff.getValue(tiffTags,'BitsPerSample') ==  8) stop("Not an 3 x 8-bit image.")

  # read data
  dataStart = tiffTags[which(tiffTags$tag==50434),]$value
  dataLen = tiffTags[which(tiffTags$tag==50434),]$count
  # warning(paste("length:",dataLen))
  df = loadBinaryAFMDatafromTIFF(filename, dataStart, dataLen, params$nDataType)

  # create image
  imWidth = tiff.getValue(tiffTags, 'ImageWidth')
  imHeight = tiff.getValue(tiffTags, 'ImageLength')
  if (imHeight != imWidth) {
    warning("Image is not square.")
    imHeight=imWidth
  }
  if (length(df) != imHeight*imWidth) {
    imHeight = sqrt(length(df))
    imWidth = imHeight
  }
  x=rep(1:imWidth,imHeight)
  y=rep(seq(from=1, to=imHeight),each=imWidth)
  # warning(paste("width:",imWidth," pixels"))
  d1 = data.frame(
    x,
    y,
    z = df
  )
  d1$x.nm = params$dfXScanSizeum * d1$x / max(d1$x)*1000
  d1$y.nm = params$dfYScanSizeum * d1$y / max(d1$y)*1000
  d1$z.nm = (d1$z * params$dfDataGain) *  units2nanometer(params$UnitZ)

  d1
}