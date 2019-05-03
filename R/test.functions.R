library(nanoscopeAFM)
source.path = './data'
dir(source.path)
my.pattern = '.*nid$'
file.list = dir(source.path, pattern=my.pattern, recursive=TRUE)
print(paste("Found",length(file.list),"AFM files."))

fname = file.path(source.path,file.list[1])


h = read.NID_header(fname)


#
while(i<2 || first_line != "") {
  first_line <-
  header=c(header, first_line)
  i=i+1
  # include \n as +1
  first_line = gsub('\xb5','mu',first_line)
  first_line = gsub('\xb0','o',first_line)
  dlen.header = dlen.header + nchar(first_line) + 2
}
close(con)
