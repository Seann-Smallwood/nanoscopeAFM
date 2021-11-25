#' loads Asylum Research Igor AFM wavefile
#' which can contain multiple images, returns channel
#' name and units in attributes
#'
#' @param filename filename including path
#' @param no number of the channel
#' @return image with attributes
#' @author thomasgredig
#' @examples
#' d = read.AR_file(system.file("extdata","AR_20211011.ibw",package="nanoscopeAFM"))
#' attr(d, 'channel')
#' @export
read.AR_file <- function(filename, no=1) {
  h1 = read.AR_eofHeader.V2(filename)
  if (no > as.numeric(h1$NumberOfFiles)) warning("Requested image no is out of range.")
  channelName = .getChannelName(h1, no)
  units = .getChannelUnits(channelName)

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

    attr(dr, "channel") = channelName
    attr(dr, "units") = units

    sfA = q2['sfA']$sfA
    convFactor <- sfA[1]  # all images should have same dimensions
    # wfUnits <- q2$dimUnits[no]

    if (units=='m') {
      dr$x.nm = convFactor*1e9 * (dr$x-1)
      dr$y.nm = convFactor*1e9 * (dr$y-1)
      dr$z.nm = 1e9 * dr$z
      dr$z.nm = dr$z.nm - min(dr$z.nm)
    } else {
      dr$x.nm = convFactor*1e9 * (dr$x-1)
      dr$y.nm = convFactor*1e9 * (dr$y-1)
      dr$z.nm = dr$z
    }

    attr(dr, "convFactor") = sfA[no]
    attr(dr, "channelDirection") = .getChannelDirection(h1, no)
    attr(dr, "noImages") = h1$NumberOfFiles
  }
  dr
}

#' loads Asylum Research Igor Wave AFM files
#'
#' @param filename filename including path
#' @param no number of the channel
#' @return image with attributes
#' @examples
#' d = read.AR_header(system.file("AR_20211011.ibw",package="nanoscopeAFM"))
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


# private files
NULL

.getChannelName <- function(h1,no) {
  # from h1=read.AR_eofHeader.V2(filename)
  gsub('(.*)[RT][er].*$','\\1',strsplit(h1$DataTypeList,",")[[1]][no])
}

.getChannelDirection <- function(h1,no) {
  # from h1=read.AR_eofHeader.V2(filename)
  gsub('.*?([RT][er].*)$','\\1',strsplit(h1$DataTypeList,",")[[1]][no])
}

.getChannelUnits <- function(channelName) {
  units = "m"  # default units
  if(grepl('Phase',channelName)) units="deg"
  units
}
