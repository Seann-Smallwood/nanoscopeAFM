#' Returns NanoScope AFM image with Scaling
#'
#' @param filename filename including path
#' @return data frame with x,y,z data in units of nm
#' @examples
#' filename = dir(pattern='000$', recursive=TRUE)[1]
#' h = read.Nanoscope_file_scaled(filename)
#' ggplot() + geom_raster(data = d , aes(x = x, y = y, fill = z)) +
#' coord_equal(expand=FALSE) +
#' xlab('x (nm)') +
#' ylab('y (nm)') +
#' scale_fill_continuous(name='z (nm)')
#' @export
read.Nanoscope_file_scaled <- function(filename) {
  d = data.frame()
  if (file.exists(filename)==FALSE) {
    warning(paste("Nanoscope Load Error: File",filename,"does not exist."))
    return(d)
  }
  # read header information
  h = read.Nanoscope_header(filename)
  if (nrow(h)==0) {
    warning(paste("Nanoscope Load Error: File",filename,"has no header information."))
  } else {
    line.num = as.numeric(h$value[grep('Number of lines',h$name)])
    lines = line.num[1]
    line.sam = as.numeric(h$value[grep('Samps/line',h$name)[-1]])
    zScale = h$value[grep('Sens. Zscan',h$name)]
    zSens = h$value[grep('Z center',h$name)]
    image.size = h$value[grep('Scan size',h$name)[-1]]
    Scale.nm.V = as.numeric(gsub(' nm/V','',str_extract(zScale, pattern = '\\d+\\.\\d+ nm/V$')))
    Sens.V = as.numeric(gsub(' V','',str_extract(zSens, pattern = '\\d+\\.\\d+ V$')))
    zConversion = Scale.nm.V*Sens.V/(2^16)
    qq = strsplit(image.size[1],' ')
    width.nm = as.numeric(sapply(qq,'[[',2))
    height.nm = as.numeric(sapply(qq,'[[',3))
    units = sapply(qq,'[[',4)
    # convert to nm
    if (units=='~m') {
      width.nm = width.nm*1000
      height.nm = height.nm *1000
    }

    step.nm = height.nm / lines

    # load image
    x = rep(seq(from=0, to=width.nm, length=lines), lines)
    y = rep(seq(from=0, to=height.nm, length=lines), each=lines)
    d = read.Nanoscope_file(filename)
    z = d[[1]]  #chose first image
    d = data.frame(x,y,z=z*zConversion)
  }
  d
}
