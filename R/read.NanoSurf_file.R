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


#' loads images of AFM NID file (use NID.loadImage whenever)
#'
#' @param filename filename including path
#' @param imageNo number of the image file
#' @return AFM image
#' @examples
#' filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
#' d = read.NanoSurf_file(filename)
#' @export
read.NanoSurf_file<- function(filename, imageNo=1) {
  if (!file.exists(filename)) warning(paste("File",filename,"does NOT exist."))
  # read header information
  hItems = read.NID_headerItems(filename)
  # read all images
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
  d = r

  # get scaling for image
  s1 = NID.getChannelScale(hItems,imageNo)

  # create the rastering sequences for x-, y-axes
  # and convert pixels from z-axis into scale (m or V)
  seq1 = seq(from=s1$from[1], to=s1$to[1], length.out = s1$length[1])
  seq2 = seq(from=s1$from[2], to=s1$to[2], length.out = s1$length[2])
  range.z = s1$to[3]-s1$from[3]

  # create a data frame with the AFM image
  data.frame(x =rep(1:s1$length[1], each = s1$length[1]),
             y = rep(1:s1$length[1], times = s1$length[1]),
             z = d[[imageNo]],
             x.nm=rep(seq1,each=s1$length[2])*1e9,
             y.nm=rep(seq2,times=s1$length[1])*1e9,
             z.nm=(d[[imageNo]]* (range.z/65536) )*1e9)
}



get.NIDitem <- function(item, name) {
  n0 = grep(paste0(name,'='),item)
  gsub(paste0(name,'='),'',item[n0])
}

get.NIDitem.numeric <- function(item, name) {
  n0 = grep(paste0(name,'='),item)
  as.numeric(gsub(paste0(name,'='),'',item[n0]))
}

# returns the scales of a particular channel / image
# headerList header list as obtained from read.NID_headerItems
# imageNo 1,2,3,4 denoting the number of the image
NID.getChannelScale <- function(headerList, imageNo = 1) {
  c1 = switch(imageNo, "Gr0-Ch1","Gr0-Ch2","Gr1-Ch1","Gr1-Ch2",
              "Gr2-Ch1","Gr2-Ch2","Gr3-Ch1","Gr3-Ch2")
  d.set = get.NIDitem(headerList[[2]],c1)
  k.set = grep(d.set,headerList[[1]])
  h = headerList[[k.set]]

  ax=data.frame(axis='x',units = get.NIDitem(h,'Dim0Unit'),
                from=get.NIDitem.numeric(h,'Dim0Min'),
                to=get.NIDitem.numeric(h,'Dim0Min')+get.NIDitem.numeric(h,'Dim0Range'),
                length=get.NIDitem.numeric(h,'Points'))
  ay=data.frame(axis='y',units = get.NIDitem(h,'Dim1Unit'),
                from=get.NIDitem.numeric(h,'Dim1Min'),
                to=get.NIDitem.numeric(h,'Dim1Min')+get.NIDitem.numeric(h,'Dim1Range'),
                length=get.NIDitem.numeric(h,'Lines'))
  az=data.frame(axis='z',units = get.NIDitem(h,'Dim2Unit'),
                from=get.NIDitem.numeric(h,'Dim2Min'),
                to=get.NIDitem.numeric(h,'Dim2Min')+get.NIDitem.numeric(h,'Dim2Range'),
                length=2**get.NIDitem.numeric(h,'SaveBits'))
  rbind(ax,ay,az)
}



#' loads header of NanoSurf AFM NID file and returns
#' parameters for particular image
#'
#' @param filename filename including path
#' @param imageNo image number to get data on
#' @return list
#' @examples
#' filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
#' read.NanoSurf_header(filename, 1)
#' @export
read.NanoSurf_header <- function(filename, imageNo=1) {
  # read the NID header
  k1 = read.NID_header(filename)[[2]]
  k1 = enc2utf8(k1)
  gsub('<b5>','\u00b5',k1) -> k1
  gsub('<b0>','\u00b0',k1) -> k1
  # k1[grep('<[0-9a-z][0-9a-z]>',k1)]   # <- find any extended ASCII
  # separate groups
  from = grep("^\\[.*\\]$",k1)
  to = c(from[-1]-1, length(k1))
  itemslist <- mapply(
    function(x, y) return(k1[x:y]),
    x = from, y = to - 1,
    SIMPLIFY = FALSE
  )
  # add list with titles
  allParams = c(list(c('HEADERS',k1[from])),itemslist)
  p = c()

  # extract specific information
  dataSet = grep('\\[DataSet\\]',allParams[[1]])
  p = c(p, allParams[[dataSet]][c(2,3)])

  dataSetInfo = grep('\\[DataSet-Info\\]',allParams[[1]])
  p = c(p,allParams[[dataSetInfo]][grep('^--\\s',allParams[[dataSetInfo]], invert=TRUE)[-1]])

  # convert to associative array
  pspl = strsplit(p, '=', useBytes=TRUE)
  p2=c()
  p2[sapply(pspl,'[[',1)] = sapply(pspl,'[[',2)
  p2
}


# > k1[from]
# [1] "[DataSet]"                           "[DataSet-Info]"                      "[DataSet\\DataSetInfos]"
# [4] "[DataSet\\DataSetInfos\\Scan]"       "[DataSet\\DataSetInfos\\Feedback]"   "[DataSet\\DataSetInfos\\Global]"
# [7] "[DataSet\\DataSetInfos\\Module]"     "[DataSet\\Calibration]"              "[DataSet\\Calibration\\Scanhead]"
# [10] "[DataSet\\Calibration\\Cantilever]"  "[DataSet\\Parameters]"               "[DataSet\\Parameters\\Approach]"
# [13] "[DataSet\\Parameters\\ZFeedback]"    "[DataSet\\Parameters\\Lithography]"  "[DataSet\\Parameters\\Imaging]"
# [16] "[DataSet\\Parameters\\SignalIO]"     "[DataSet\\Parameters\\Spectroscopy]" "[DataSet\\Parameters\\SPMSystem]"
# [19] "[DataSet-0:1]"                       "[DataSet-0:2]"                       "[DataSet-1:1]"
# [22] "[DataSet-1:2]"                       "[SetView]"                           "[SetView-View0]"
# [25] "[SetView-View1]"                     "[SetView-View2]"                     "[SetView-View3]"

# ######################################
# read NID file, AFM file
#
# Date: 2019-02-10
# Author: Thomas Gredig
#
# ######################################

# loads header of NanoSurf NID file as text vector
#
# str(t)
# List of 2
# $ header.len: num 12917
# $ header    : chr [1:626] "[DataSet]" "Version=2" "GroupCount=2" "Gr0-Name=Scan forward" ...
# > head(t[[2]])
# [1] "[DataSet]"             "Version=2"             "GroupCount=2"          "Gr0-Name=Scan forward"
# [5] "Gr0-ID=0"              "Gr0-Count=21"
read.NID_header <- function(filename) {
  if (!file.exists(filename)) { return(NULL) }
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


#' loads header of NID file and separates into items
#' first item in list has the titles of the others
#'
#' @param filename filename including path
#' @return list
#' @examples
#' filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
#' read.NID_headerItems(filename)
#' @export
read.NID_headerItems <- function(filename) {
  # read the NID header
  k1 = read.NID_header(filename)[[2]]
  # separate groups
  from = grep("^\\[.*\\]$",k1)
  to = c(from[-1]-1, length(k1))
  itemslist <- mapply(
    function(x, y) return(k1[x:y]),
    x = from, y = to - 1,
    SIMPLIFY = FALSE
  )
  # add list with titles
  c(list(c('HEADERS',k1[from])),itemslist)
}

#' checks AFM NID file length
#'
#' @param filename filename including path
#' @return mismatch in image size + header with file size (should be 0)
NID.checkFile <- function(filename) {
  # does file exist?
  if (!file.exists(filename)) return -1
  # length of file in bytes
  file.len = file.info(filename)$size

  # read header
  h = read.NID_header(filename)
  # get header length in bytes
  header.length = h[[1]]

  # get number of images and size of images
  q = get.NID_imageInfo(h[[2]])

  # compare file length with images + header +
  # 2 bytes for #! character, which is the beginning
  # of the images
  file.len - sum(q*q)*2 - header.length - 2
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




#' loads header of NanoSurf AFM NID file and returns
#' parameters for particular image
#'
#' @param filename filename including path
#' @return list
#' @examples
#' filename = system.file("extdata", "NanoSurf_20160301.nid",package="nanoscopeAFM")
#' h = read.NanoSurf_header.v2(filename)
#' @export
read.NanoSurf_header.v2 <- function(filename) {
  # read the NID header
  k1 = read.NID_header(filename)[[2]]
  k1 = enc2utf8(k1)
  gsub('<b5>','\u00b5',k1) -> k1
  gsub('<b0>','\u00b0',k1) -> k1

  q = grep("=",k1)
  data.frame(
    name = gsub('(.*?)=.*','\\1',k1[q]),
    value = gsub('(.*?)=(.*)','\\2',k1[q])
  )
}
