###################################################
#'
#' Height-Height Correlation Function
#'
#' @description
#' Computes g(r) correlation function for an AFM
#' image with height information. The height information
#' must be encoded in a BMP file with a linear scale
#' Publication: http://iopscience.iop.org/article/10.1088/1742-6596/417/1/012069
#
#' Title: Height-Height Correlation Function to Determine Grain
#'        Size in Iron Phthalocyanine Thin Films
#' Authors: Thomas Gredig, Evan A. Silverstein, Matthew P Byrne
#' Journal: J of Phys: Conf. Ser. Vol 417, p. 012069 (2013).
#'
#' @author Thomas Gredig
#' @param obj AFMdata object
#' @param no Channel number
#' @param degRes resolution of angle, the higher the more, should be >100, 1000 is also good
#' @param addFit if \code{TRUE} a fit is added to the data
#' @param numIterations Number of iterations (must be > 1000), but 1e6 recommended
#' @param dataOnly if \code{TRUE} only return data frame, otherwise returns a graph
#' @param verbose output time if \code{TRUE}
#'
#' @importFrom ggplot2 ggplot geom_point geom_path scale_x_log10 scale_y_log10 theme_bw geom_label theme
#'
#' @return graph or data frame with g(r) and $num indicating number of computations used for r
#'
#' @examples
#' filename = AFM.getSampleImages(type='tiff')
#' a = AFM.import(filename)
#' a = AFM.flatten(a)
#' r = AFM.hhcf(a, dataOnly = TRUE)
#' head(r)
#'
##################################################
#' @export
AFM.hhcf <- function(obj, no=1,
                     degRes = 100,
                     numIterations=1000,
                     addFit = TRUE,
                     dataOnly = FALSE,
                     verbose=FALSE) {
  if (!(class(obj)=="AFMdata")) return(NULL)
  if (obj@x.conv != obj@y.conv) warning('AFM image is distorted in x- and y-direction; HHCF is not correct.')
  dimx = obj@x.pixels
  dimy = obj@y.pixels
  q = obj@data$z[[no]]
  if (numIterations<1000) numIterations=1000
  # generate random numbers to pick starting positions
  # faster than computing all possible locations
  px1 = round(runif(numIterations, min=1, max=dimx))
  py1 = round(runif(numIterations, min=1, max=dimy))
  theta = round(runif(numIterations, min=0, max=2*pi*degRes))/degRes

  lg = c()
  lq = c()
  t.start = as.numeric(Sys.time())
  maxR = round(dimx*0.8)
  for(r in 1:maxR) {
    # compute second point
    px2 = round(px1+r*cos(theta))
    py2 = round(py1+r*sin(theta))
    qq = which(px2>0 & px2 <= dimx & py2>0 & py2 <= dimy)

    p1 = px1[qq]+(py1[qq]-1)*dimx
    p2 = px2[qq]+(py2[qq]-1)*dimy

    # compute height-height difference
    g = sum((q[p1]-q[p2])^2) / length(qq)
    lg = c(lg, g)
    lq = c(lq, length(qq))
  }
  t.end = as.numeric(Sys.time())
  if (verbose) print(paste('Time used: ',round(t.end-t.start,1), ' seconds'))

  r = data.frame(
    r.nm = (1:maxR)*obj@x.conv,
    g = lg,
    num = lq
  )
  if (dataOnly) return(r)

  if (addFit) {
    # starting fit parameters
    AFM.math.params(obj) -> m1
    m1$Rq^2*2 -> ss
    xi = r$r.nm[min(which(r$g>0.7*ss))]

    # fit the data using
    # 2*sigma^2 = ss, sigma = roughness
    # xi = correlation length
    # H = 2*Hurst parameter
    fit <- NULL
    try({
      nls(data=r,
          g ~ ss * (1 - exp(-(r.nm/xi)^H)),
          start = list(ss = ss, xi = xi,
                       H=2)) -> fit
    })

    # fit was successful
    if (!is.null(fit)) {
      fitRnm = seq(from=round(min(r$r.nm)*0.9), to=max(r$r.nm), by=1)
      dFit = data.frame(
        r.nm = fitRnm,
        g = predict(fit, list(r.nm=fitRnm))
      )
      fitNames = c('sigma', 'xi','H')
      fitNamesUnits = c('nm','nm','')
      fitParams = coef(fit)
      fitParams[1]=sqrt(fitParams[1]/2)
      fitParams[3]=fitParams[3]/2
      dFitLabels = data.frame(
        r.nm = r$r.nm[1:3],
        g = r$g[1:3],
        label = paste(fitNames,'=',signif(fitParams,4),fitNamesUnits)
      )
    }
  }

  g = ggplot(r, aes(r.nm, g)) +
    geom_point(col='blue', size=2) +
    scale_x_log10() +
    scale_y_log10() + ylab('g(r)') + xlab('r (nm)') +
    theme_bw()
  if (addFit) g = g +
    geom_path(data=dFit, col='red') +
    geom_label(data = dFitLabels,
               aes(fill = 'white',label=label), colour = "white",
               fontface = "bold", hjust=-0.1) +
    theme(legend.position = 'none')

  g
}


