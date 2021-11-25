#' Returns Veeco NanoScope AFM image with Scaling
#'
#' @param filename filename including path
#' @param no image number
#' @return image
#' @examples
#' filename = system.file("extdata","Veeco_20160622.003",package="nanoscopeAFM")
#' h = read.Nanoscope_file(filename)
#' @export
read.Nanoscope_file <- function(filename, no=1) {
  d = data.frame()
  if (file.exists(filename)==FALSE) {
    warning(paste("Nanoscope Load Error: File",filename,"does not exist."))
    return(d)
  }
  # read header information
  #h = read.Nanoscope_header(filename)
  h = read.Nanoscope_params(filename, no)
  if (length(h)==0) {
    warning(paste("Nanoscope Load Error: File",filename,"has no header information."))
  } else {
    line.num = as.numeric(h['Number of lines'])
    line.sam = as.numeric(h['Samps/line'])
    zScale = h['AT-Sens. Zscan']
    zSens = h['AT-Z center']
    image.size = h['Scan size']
    Scale.nm.V = as.numeric(gsub(' nm/V','',stringr::str_extract(zScale, pattern = '\\d+\\.\\d+ nm/V$')))
    Sens.V = as.numeric(gsub(' V','',stringr::str_extract(zSens, pattern = '\\d+\\.\\d+ V$')))
    zConversion = Scale.nm.V*Sens.V/(2^16)
    wh = strsplit(gsub('\\s*(.*)\\s.*','\\1', h['Scan size2'])," ")[[1]]
    width.nm = as.numeric(wh[1])
    height.nm = as.numeric(wh[2])
    # convert to nm
    if (length(grep('~m',h['Scan size2']))) {
      width.nm = width.nm*1000
      height.nm = height.nm *1000
    } else if (!(units=='nm')) {
      warning(paste('Nanoscope units are neither nm nor um, but ',units))
    }

    step.nm = height.nm / line.num

    # load image
    x.nm = rep(seq(from=0, to=width.nm, length=line.num), line.num)
    y.nm = rep(seq(from=0, to=height.nm, length=line.num), each=line.num)
    z = read.Nanoscope_BIN(filename, no)
    d = data.frame(x = rep(1:line.num, line.num),
                   y =  rep(1:line.num, each=line.num),
                   z,
                   x.nm,
                   y.nm,
                   z.nm = z*zConversion)
  }
  d
}

# private functions
# ==
NULL

#' loads the binary data from the Veeco file
read.Nanoscope_BIN <- function(filename, no=1) {
  if(!file.exists(filename)) { warning(paste("File",filename," does NOT exist !")) }
  # read the header of the AFM file
  q = read.Nanoscope_header(filename)
  # retrieve header length + file image lengths
  dlen.num = as.numeric(q$value[grepl('Data length',q$name)])
  # load components
  con <- file(filename,"rb")
  bin.header <- readBin(con, integer(),  n = dlen.num[1], size=1, endian = "little")
  imageNo = 1
  r = list()
  while(imageNo < length(dlen.num)) {
    bin.data   <- readBin(con, integer(),  n = dlen.num[imageNo+1]/2, size=2, endian = "little")
    r[[imageNo]] = bin.data
    imageNo = imageNo+1
  }
  close(con)
  r[[no]]
}


# returns header information
read.Nanoscope_header <- function(filename) {
  if (!file.exists(filename)) { warning(paste("File",filename,"does NOT exist.")) }
  if (file.info(filename)$size<20000) { warning(paste("File",filename,"is too small. Data may be missing."))}
  first_line = ''
  header = c()
  con <- file(filename,"rb")
  i=0
  err = FALSE
  while(first_line != "\\*File list end" ) {
    first_line <- readLines(con,n=1)
    if (length(first_line)==0) { err = TRUE; break; }
    header=c(header, first_line)
    i=i+1
  }
  close(con)
  if (err==TRUE) {
    data.frame(
      name='Reading error',
      value='TRUE'
    )
  } else {
    header = gsub('\\\\','',header)
    header[grep('^\\*',header)] = paste0(header[grep('^\\*',header)],":NEW SECTION")
    p1 = strsplit(header,':')
    data.frame(
      name = unlist(lapply(p1,"[[",1)),
      value = unlist(lapply(p1,"[[",2)),
      stringsAsFactors = FALSE
    )
  }
}

# > h$name[sections]
# [1] "*File list"       "*Equipment list"  "*Scanner list"    "*Ciao scan list"  "*Fast Scan list"  "*Ciao image list"
# [7] "*Ciao image list" "*File list end"
# returns header information for particular image number only
read.Nanoscope_params <- function(filename, no=1) {
  h = read.Nanoscope_header(filename)
  # parse out parameters that are relevant to image number no
  sections = grep('NEW SECTION', h$value)
  fileNo = grep('File list$', h$name[sections])
  scanNo = grep('Scanner list$', h$name[sections])
  ciaoNo = grep('Ciao scan list$', h$name[sections])
  eqipNo = grep('Equipment list', h$name[sections])
  imNo = grep('Ciao image list$',h$name[sections])[no]

  h = h[c(sections[fileNo]:(sections[fileNo+1]-1),
      sections[eqipNo]:(sections[eqipNo+1]-1),
      sections[scanNo]:(sections[scanNo+1]-1),
      sections[ciaoNo]:(sections[ciaoNo+1]-1),
      sections[imNo]:(sections[imNo+1]-1)),]

  # tidy up some of the names
  gsub("^\\*", 'SECTION-' , h$name) -> h$name
  winNum = grep("^@\\d", h$name)
  nl = unique(h$name[winNum])
  for(nm in nl) { n1 = which(h$name==nm); v1 = paste(h$value[n1], collapse=';;'); h=h[-n1,]; h=rbind(h,data.frame(name=nm,value=v1)); }
  gsub('^@','AT-',h$name) -> h$name
  nl = h$name[duplicated(h$name)==TRUE]
  for(nm in nl) { j=1; for(i in which(h$name==nm)) { if(j>1) {h$name[i] = paste0(h$name[i],j)}; j=j+1; } }

  p2=c()
  p2[h$name] = h$value
  p2
}




# ######################################
# read NID file, AFM file
#
# Date: 2019-02-10
# Author: Thomas Gredig
#
# ######################################
#
# # Nanosurf image data format (NID format)
# # from easyscan AFM
#
#
# # read the header of the NID AFM files
# # seems header file ends with "" (empty) or
# # with #!F
# ######################################

#empty.header = list(header.len = 0, header = c(''))

#' loads header of AFM NID file
#'
#' @param filename filename including path
#' @return list with length in bytes and header as text
#' @examples
#' filename = system.file("extdata","Veeco_20160622.003",package="nanoscopeAFM")
#' read.NID_header(filename)
#' @export
read.NID_header <- function(filename) {
  if (!file.exists(filename)) { return(empty.header) }
  Sys.setlocale('LC_ALL','en_US')
  con <- file(filename,"rb")
  rline = ''
  i=0
  dlen.header = 0
  while( TRUE ) {
    rline = readLines(con,n=1)
    if (substr(rline,1,2) == "#!" ) break
    i = i + 1
    dlen.header = dlen.header + nchar(rline, type="bytes") + 2
  }
  close(con)

  con <- file(filename,"rb")
  header = readLines(con, n=(i-1))
  close(con)

  list(header.len = dlen.header, header = header)
}




#' returns number of lines for each image
#'
#' @param header.string header string of NID file (use read.NID_header)
#' @return vector with line numbers for each image
#' @examples
#' hdr = read.NID_header(system.file("extdata","Veeco_20160622.003",package="nanoscopeAFM"))
#' get.NID_imageInfo(hdr[[2]])
#' @export
get.NID_imageInfo <- function(header.string) {
  # split data sets
  from = grep('\\[DataSet-',header.string)
  to = c(from[-1]-1, length(header.string))

  itemslist <- mapply(
    function(x, y) return(header.string[x:y]),
    x = from, y = to,
    SIMPLIFY = FALSE
  )
  itemslist[[1]] <- NULL

  image.Lines <- lapply(itemslist,
                        function(x) {
                          x[grep('Lines',x)]
                        }
  )

  as.numeric(
    unlist(lapply(image.Lines, function(x) { sapply(strsplit(x,"="),'[[',2) }))
  )
}

