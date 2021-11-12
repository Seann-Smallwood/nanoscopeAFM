#' loads images of AFM NID file (use NID.loadImage whenever)
#'
#' @param filename filename including path
#' @param imageNo number of the image file
#' @return AFM image
#' @examples
#' filename = dir(pattern='nid$', recursive=TRUE)[1]
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
             x.nm=rep(seq1,each=s1$length[2]),
             y.nm=rep(seq2,times=s1$length[1]),
             z.nm=d[[imageNo]]/s1$length[3]*range.z+s1$from[3])
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
