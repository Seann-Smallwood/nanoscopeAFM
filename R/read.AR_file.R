#' loads Asylum Research Igor Wave AFM header information
#'
#' @param filename filename including path
#' @return header information
#' @examples
#' filename = system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM")
#' d = read.AR_header.v2(filename)
#' @export
read.AR_header.v2 <- function(filename) {
  suppressWarnings({
    d = IgorR::read.ibw(filename)
    # to get notes, need to read all, not just "header"
  })
  qNote = attr(d, "Note")
  notes = strsplit(qNote,'\r')[[1]]
  df = data.frame(
    name = gsub('(.*?):.*','\\1',notes),
    value = gsub('(.*?):(.*)','\\2',notes)
  )
}



#' loads Asylum Research Igor AFM wavefile
#'
#' loads all images contained in Igor wavefile and
#' returns an AFMdata object
#'
#' @param h1 filename including path
#' @return \code{AFMdata} object
#' @export
read.AR_file.v2 <- function(filename) {
  cat('loading')
  h1 = read.AR_eofHeader.V2(filename)
  channels = strsplit(h1$DataTypeList,',')[[1]]
  units = rep('nm', length(channels))
  units[grep('^Phase',channels)] = 'deg'
  cat('image')
  suppressWarnings({
    d = IgorR::read.ibw(filename)
  })
  q2 = attr(d, "WaveHeader")
  imageDim <- q2$nDim[1:2]
  noChannels <- q2$nDim[3]
  dr = data.frame()
  x.conv = q2$sfA[1]
  y.conv = q2$sfA[2]
  im.size = imageDim[1]*imageDim[2]
  z.data=list()
  for(i in 1:noChannels) {
    z.conv = 1; if (units[i]=='nm') z.conv=1e9
    z.data[[i]] = d[(im.size*(i-1)+1):(im.size*i)]*z.conv
  }

  # return AFMdata object
  AFMdata(
    data = list(z=z.data),
    channel = channels,
    x.conv = x.conv*1e9,
    y.conv = y.conv*1e9,
    x.pixels = imageDim[1],
    y.pixels = imageDim[2],
    z.conv = 1,
    z.units = units,
    instrument = 'Cypher',
    history = '',
    description = gsub('.*ImageNote:(.*?)\r.*','\\1',attr(d,'Note')),
    fullfilename = filename
  )
}




#' returns the version 2 header information tagged
#' at the end of the AR file, which includes several tags
#' including the $DataTypeList and $NumberOfFiles
#' ex: "HeightRetrace,AmplitudeRetrace,PhaseRetrace,ZSensorRetrace,"
#'
#' @param wavefile filename including path
#' @param Verbose if true, returns additional information
#' @return list of channels and additional information
#' @examples
#' filename = system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM")
#' afmV2header = read.AR_eofHeader.V2(filename)
#' afmV2header$DataTypeList
#' @export
read.AR_eofHeader.V2 <- function(wavefile, Verbose = FALSE) {
  con <- file(wavefile, "rb",encoding="macintosh")

  # check Igor Wavefile is version 5
  bytes=readBin(con,"integer",2,size=1)
  if(bytes[1]==0){ endian="big"; version=bytes[2] } else { endian="little"; version=bytes[1] }
  if(Verbose) cat("version = ",version,"endian = ",endian,"\n")
  if(version != 5) { warning("Not sure how to read IBW")}

  # find the end of the file
  fsize = file.info(wavefile)$size
  seek(con, where=fsize-10)
  # read header size
  rawc = readBin(con, what="raw", 10)    # "0466 MFP3D"
  s=readBin(rawc, what="character")
  headerSize = as.numeric(gsub('^(\\d+).*','\\1',s))
  if(Verbose) cat("headerSize = ",headerSize,"\n")

  # read complete header version 2
  s3=list()
  if (headerSize>10 & headerSize<1200) {
    seek(con, where=fsize-headerSize)
    rawc = readBin(con, what="raw", headerSize)
    s=readBin(rawc, what="character")
    p2= strsplit(strsplit(s,';')[[1]],":")
    p3 = p2[which(sapply(p2, length)==2)]

    s3[sapply(p3,'[[',1)] =  sapply(p3,'[[',2)
  } else { warning("Header size incorrect.") }
  close(con)

  s3
}


