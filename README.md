[![Build Status](https://travis-ci.org/youjin1207/netdep.svg?branch=master)](https://travis-ci.org/youjin1207/netdep)

## Overview

netdep is for testing for network dependence. Dependence due to social network connections increases variance and engenders confounding that can lead to biased estimates. Therefore testing for network dependence should precede any statistical inference upon observations sampled from a single or small number of social networks.
We also provide two data generative process for generating network dependent outcomes--network dependence due to (1) direct transmission and (2) latent variable dependence.

## Package information

- Version: 0.1.0
- Author : Youjin Lee (<ylee160@jhu.edu>) and Elizabeth Ogburn (<eogburn@jhu.edu>)
- Maintainer : Youjin Lee (<ylee160@jhu.edu>)
- Imports : stats, igraph, igraphdata, MASS, mvrtn

## Installation

You can download the package by:

```
install.packages("netdep")

# or you can directly download the development version from author's Github 
install.packages("devtools")
library(devtools)
install_github("youjin1207/netdep")
```


## Usage

[Here](https://github.com/youjin1207/netdep/blob/master/vignettes/nettest.Rmd) is a R vignettes for guidance. Or you can access to vignettes via:

```
vignette("nettest", package = "netdep")
```


