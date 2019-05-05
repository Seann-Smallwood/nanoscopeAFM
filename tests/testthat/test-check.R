fname = dir(pattern='nid$', recursive = TRUE)
check.NID_file(fname[1]) == 0

read.NID_file(fname[1]) -> r1

library(raster)
m1 = matrix(r1[[3]], nrow = 128)
plot(raster(m1))
