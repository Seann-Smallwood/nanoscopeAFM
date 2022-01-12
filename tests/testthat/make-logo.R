# make LOGO
library(nanoscopeAFM)
library(ggplot2)
da = AFM.import(AFM.getSampleImages(type='tiff'))
plot(da, graphType=3)
ggsave('man/figures/logo.png', width=3,height=3)
