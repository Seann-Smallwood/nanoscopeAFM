#' loads Asylum Research Igor Wave AFM files
#'
#' @param filename filename including path
#' @param no number of the channel
#' @return image with attributes
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = read.AR_file(filename)
#' @export
read.AR_file <- function(filename, no=1) {
  suppressWarnings({
    d = IgorR::read.ibw(filename)
  })
  q2 = attr(d, "WaveHeader")
  imageDim <- q2$nDim[1:2]
  noChannels <- q2$nDim[3]
  dr = data.frame()

  if(no <= noChannels) {
    x = 1:dim(d)[1]
    y = 1:dim(d)[2]
    zp = 1:(length(x)*length(y)) + (no-1)*(length(x)*length(y))
    dr = data.frame(
      x = rep(x, length(y)),
      y = rep(y, each=length(x)),
      z = d[zp]
    )

    sfA = q2['sfA']$sfA
    convFactor <- sfA[no]
    Units <- q2$dimUnits[no]

    dr$x.nm = convFactor*1e9 * dr$x
    dr$y.nm = convFactor*1e9 * dr$y
    dr$z.nm = 1e9 * dr$z
    dr$z.nm = dr$z.nm - min(dr$z.nm)
  }
  dr
}

#' loads Asylum Research Igor Wave AFM files
#'
#' @param filename filename including path
#' @param no number of the channel
#' @return image with attributes
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' d = read.AR_header(filename)
#' @export
read.AR_header <- function(filename, no=1) {
  suppressWarnings({
    d = IgorR::read.ibw(filename)
    # to get notes, need to read all, not just "header"
  })
  qHead = attr(d, "WaveHeader")
  p = c(version = attr(d, "BinHeader")$version,
        creationDate = format(qHead$creationDate),
        modDate = format(qHead$modDate),
        npts = qHead$npts,
        name = qHead$WaveName,
        numImages = length(qHead$nDim),
        nDim = qHead$nDim[no],
        sfA = qHead$sfA[no],
        sfB = qHead$sfB[no],
        dataUnits = qHead$dataUnits,
        dimUnits = qHead$dimUnits[no],
        platform = qHead$platform)
  qNote = attr(d, "Note")
  notes = strsplit(qNote,'\r')[[1]]
  pspl = strsplit(notes, ":")
  n4 = which(sapply(pspl,length) < 2)
  for(i in 1:length(n4)) { pspl[[n4[i]-i+1]] <- NULL }
  n4 = which(sapply(pspl,length) > 2)
  for(i in 1:length(n4)) { pspl[[n4[i]]][2] = paste(pspl[[n4[i]]][-1],collapse=':') }

  p2=c()
  p2[sapply(pspl,'[[',1)] = sapply(pspl,'[[',2)
  p2
}


#' returns names of AR channels
#'
#' @param filename filename including path
#' @return string with channel names ("Height", "Amplitude")
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' s1 = read.AR_channelNames(filename)
read.AR_channelNames <- function(filename) {
  read.AR_header(filename) -> r1
  # find Channel Names for AR:
  channelNames = c(paste0(r1[grep('^Channel',attr(r1,'names'))]))
  NapMode = as.numeric(r1[grep('NapMode',attr(r1,'names'))])
  if (NapMode==1) {
    channelNames = c(channelNames,paste0('Nap',r1[grep('^FastMap\\d',attr(r1,'names'))]))
  }
  channelNames = gsub('\\s+','',channelNames)
  channelNames[-which(channelNames=='None')]
}

#' returns names of AR channel names
#'
#' @param wavefile filename including path
#' @param Verbose if true, returns additional information
#' @return list of channels and additional information
#' @examples
#' filename = dir(pattern='ibw$', recursive=TRUE)[1]
#' afmHeadInfo = read.AR_eofHeader.V2(filename)
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
  if (headerSize>10 & headerSize<1000) {
    seek(con, where=fsize-headerSize)
    rawc = readBin(con, what="raw", headerSize)
    s=readBin(rawc, what="character")
    p2= strsplit(strsplit(s,';')[[1]],":")
    p3 = p2[which(sapply(p2, length)==2)]

    s3[sapply(p3,'[[',1)] =  sapply(p3,'[[',2)
  } else { warning("Header size incorrect.") }
  s3
}
