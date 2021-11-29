#' Returns Veeco NanoScope AFM image with Scaling
#'
#' @param filename filename including path
#' @param no image number
#' @param headerOnly if \code{TRUE}, returns header only and no image
#' @return image or header information
#' @examples
#' filename = system.file("extdata","Veeco_20160622.003",package="nanoscopeAFM")
#' h = read.Nanoscope_file(filename)
#' @export
read.Nanoscope_file <- function(filename, no=1, headerOnly = FALSE) {
  err.msg = c()
  # load header
  header = c()
  con <- file(filename,"rb")
  first_line = ''
  while(first_line != "\\*File list end" ) {
    first_line <- readLines(con,n=1)
    if (length(first_line)==0) { err.msg = c(err.msg,'Header error'); break; }
    header=c(header, first_line)
  }

  # analyze header
  header = gsub('\\\\','',header)
  header[grep('^\\*',header)] = paste0(header[grep('^\\*',header)],":NEW SECTION")
  HEADER.INFO = data.frame(
    name = gsub("(.*?):.*","\\1",header),
    value = gsub(".*?:(.*)","\\1",header),
    stringsAsFactors = FALSE
  )
  Volt2nanometer = HEADER.INFO$value[grep('Sens. Zscan', HEADER.INFO$name)]
  Volt2nanometer = as.numeric(gsub(' V(.*)nm/V','\\1',Volt2nanometer))
  #write.csv(HEADER.INFO, file='VEECO-Header.csv')

  # parse out parameters that are relevant to image number no
  sections = grep('NEW SECTION', HEADER.INFO$value)
  fileNo = grep('File list$', HEADER.INFO$name[sections])
  scanNo = grep('Scanner list$', HEADER.INFO$name[sections])
  ciaoNo = grep('Ciao scan list$', HEADER.INFO$name[sections])
  eqipNo = grep('Equipment list', HEADER.INFO$name[sections])
  totalNumberImages = length(grep('Ciao image list$',HEADER.INFO$name[sections]))
  imNo = grep('Ciao image list$',HEADER.INFO$name[sections])

  # Image Header Online
  imageHeader = HEADER.INFO[sections[imNo[no]]:(sections[imNo[no]+1]-1),]
  getHeaderNumeric <- function(name) {as.numeric(imageHeader$value[grep(name,imageHeader$name)])  }
  getHeaderStr <- function(name) {imageHeader$value[grep(name,imageHeader$name)]  }
  getHeaderStrVal <- function(name) {imageHeader$value[grep(name,imageHeader$value)]  }

  im.channelName = gsub('.*\\[(.*)\\].*','\\1',imageHeader$value[grep('Image Data:',imageHeader$value)])
  im.line.num = getHeaderNumeric('Number of lines')
  im.line.sam = getHeaderNumeric('Samps/line')
  im.zMagnify = getHeaderStr('magnify')
  im.zMagnifyFac = as.numeric(gsub('.*\\](.*)','\\1',im.zMagnify))
  im.image.size = getHeaderStr('Scan size')
  im.bytes.pixel = getHeaderNumeric('Bytes/pixel')
  im.scanDirection = getHeaderStr('Line direction')
  im.Zscale = getHeaderStrVal('Z scale:')
  im.ZscaleFac = as.numeric(gsub('.*?\\)(.*)V','\\1',im.Zscale))
  im.Zoffset = getHeaderStrVal('Z offset:')
  im.dataOffset = getHeaderNumeric('Data offset')
  im.dataLength = getHeaderNumeric('Data length')

  im.units='nm'  # default units
  if (im.channelName=='Amplitude') im.units='V'
  if (im.units=='V') Volt2nanometer=1
  zConversion =  im.ZscaleFac / (2^(im.bytes.pixel*8)) * Volt2nanometer

  wh = strsplit(gsub('\\s*(.*)\\s.*','\\1', im.image.size)," ")[[1]]
  im.width.nm = as.numeric(wh[1])
  im.height.nm = as.numeric(wh[2])
  # convert um to nm
  if (length(grep('~m',im.image.size))) {
    im.width.nm = im.width.nm*1000
    im.height.nm = im.height.nm *1000
  }

  step.nm = im.height.nm / im.line.num

  # load image
  x.nm = rep(seq(from=0, to=im.width.nm, length=im.line.num), im.line.num)
  y.nm = rep(seq(from=0, to=im.height.nm, length=im.line.num), each=im.line.num)

  #print(paste("Loading at offset",im.dataOffset," (",im.dataLength,"bytes)"))
  seek(con, where  = im.dataOffset) # skip header
  z   <- readBin(con, integer(),  n = im.dataLength/im.bytes.pixel, size=im.bytes.pixel, endian = "little")
  close(con)

  if (length(err.msg)>0) warning(err.msg)

  df = data.frame(x = rep(1:im.line.num, im.line.num),
                 y = rep(1:im.line.num, each=im.line.num),
                 z,
                 x.nm,
                 y.nm,
                 z.nm = z*zConversion)

  attr(df,"channelDirection") = getHeaderStrVal('Line direction')
  attr(df,"note")       = getHeaderStrVal('Note')
  attr(df,"channel")    = im.channelName
  attr(df,"units")      = im.units
  attr(df,"date")       = HEADER.INFO$value[grep('^Date', HEADER.INFO$name)]
  if (headerOnly) {
    df = imageHeader
  }
  df
}
