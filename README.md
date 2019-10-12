# nanoscopeAFM

Analyzes Atomic Force Microsocpy (AFM) images from nanosurf (.nid) or nanoscope devices.


## Installation

```R
# install.packages("devtools")
devtools::install_github("thomasgredig/nanoscopeAFM")
```

## Description

The main functions from this library are:

- **check.NID_file**: should return 0
- **read.NID_header**: reads the header of a NID file
- **read.NID_file**: read the images from a NID file
- **flatten.NID_matrix**: plane fit to remove background
- **read.NID_Sweep_file**: Frequency Sweep NID file


## Example

The image can be loaded into memory using `read.NID_file` as the command, so here is an example:


```R
library(nanoscopeAFM)
fname = dir(pattern='nid$', recursive = TRUE)
d <- read.NID_file(fname[1])
```

Then, `d` will be a list with several images corresponding to the channels. In order to display the image use the raster library. 

```R
library(raster)
# display the first image, which is 256 x 256
m = matrix(d[[1]], nrow=256)
plot(raster(m))
```

![sample output from code above](images/Calibration-NID-File.png)

You may need to perform additional image analysis, for example you may want to remove the background. This can be performed with this code:

```R
m1 = flatten.NID_matrix(m)
plot(raster(m1))
```
![sample output from code above](images/Calibration-NID-File.Flattened.png)


## Frequency Sweep

If the NID file is a frequency sweep, you can display the data using the function `read.NID_Sweep_file` which will return a list that contains data frames with the frequency vs. amplitude data.

```R
q = read.NID_Sweep_file(fname[1])
plot(q[[1]],xlab='f (Hz)', ylab='A')
```

![sample output for frequency sweep](images/Frequency-Sweep.png)

The units for amplitude are stored in the header of the file and can be modified accordingly.
