# nanoscopeAFM

Analyzes Atomic Force Microsocpy (AFM) images from nanosurf (.nid) or nanoscope devices.


## Installation

```R
# install.packages("devtools")
devtools::install_github("thomasgredig/nanoscopeAFM")
```


## Example:

```R
library(nanoscopeAFM)
fname = dir(pattern='nid$', recursive = TRUE)
d <- read.NID_file(fname[1])
```
