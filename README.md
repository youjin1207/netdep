[![Build Status](https://travis-ci.org/youjin1207/netdep.svg?branch=master)](https://travis-ci.org/youjin1207/netdep)
![![Downloads badge](http://cranlogs.r-pkg.org/badges/netdep)](http://cranlogs.r-pkg.org/badges/netdep?color=red)
 [![](http://cranlogs.r-pkg.org/badges/grand-total/netdep?color=yellow)](https://CRAN.R-project.org/package=netdep)
[![arXiv shield](https://img.shields.io/badge/arXiv-1710.03296-blue.svg?style=flat)](https://arxiv.org/abs/1710.03296)


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

### Example

```
library(netdep)

# generate network
G = latent.netdep(n.node = 200, rho = 0.2, dep.factor = -1)
A = as.matrix(get.adjacency(G))
outcomes = peer.process(A, max.time = 3, mprob = 0.6, epsilon = 0.1)
names(outcomes)
result3 = make.permute.moran(A, outcomes$time3, np = 100)
```

```
# generate latent variable dependent observations
G = latent.netdep(n.node = 200, rho = 0.4, dep.factor = -1)
subG = snowball.sampling(G, 100)$subG
A = as.matrix(get.adjacency(subG))

# transform continuous observations to categorical observations
conti.Y = V(subG)$outcome 
cate.Y = ifelse(conti.Y < quantile(conti.Y, 0.25), 1, 4)
cate.Y = ifelse(conti.Y < quantile(conti.Y, 0.60) & conti.Y >= quantile(conti.Y, 0.25), 2, cate.Y)
cate.Y = ifelse(conti.Y < quantile(conti.Y, 0.80) & conti.Y >= quantile(conti.Y, 0.60), 3, cate.Y)
table(cate.Y)

# apply network dependence for categorical variable
result = make.permute.Phi(A, cate.Y, 100)
print(result$phi)
print(result$pval.permute)
```
