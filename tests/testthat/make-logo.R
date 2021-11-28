# make LOGO
library(nanoscopeAFM)
library(ggplot2)
library(scales)

source('R/read.AR_file.R')
da = AFM.import(system.file("extdata", "Park_20210916_034.tiff",package="nanoscopeAFM"))
print(da)
plot(da)
ggsave('man/figures/logo.png', width=3,height=3)
