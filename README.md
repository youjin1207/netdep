[![Build Status](https://travis-ci.org/youjin1207/netdep.svg?branch=master)](https://travis-ci.org/youjin1207/netdep)

# netdep
R package for testing network dependence.

- Version: 0.1.0
- Author : Youjin Lee (<ylee160@jhu.edu>) and Elizabeth Ogburn (<eogburn@jhu.edu>)
- Maintainer : Youjin Lee (<ylee160@jhu.edu>)
- Imports : igraph, igraphdata, MASS, mvrtn

You can download the package by:
```
install.packages("devtools")
library(devtools)
install_github("youjin1207/netdep")
```
[Here](https://github.com/youjin1207/netdep/blob/master/vignettes/nettest.Rmd) is a R vignettes for guidance. Or you can access to vignettes via:

```
install_github("youjin1207/netdep", build_vignettes = TRUE)
vignette("nettest", package = "netdep")
```
