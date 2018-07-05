#' Snowball sampling
#'
#' Sampling from one graph to its induced subgraph depending on network structure.
#'
#' @param G an \code{igraph} object.
#' @param samn is a size of snowball sample that will be samples from \code{G}.
#'
#' @importFrom igraph V induced.subgraph vcount neighbors V<-
#'
#' @return
#' \item{\code{subG}}{an induced subgraph of \code{G} sampled using snowball sampling.}
#' \item{\code{ind}}{a set of index of samples.}
#'
#' @export
#'
#' @examples
#' library(netdep)
#' library(igraph)
#' G = latent.netdep(n.node = 200, rho = 0.2, dep.factor = -3, dep.range = c(-10, 0))
#' subG = snowball.sampling(G, 100)
#'
snowball.sampling = function(G, samn){

  if (vcount(G) < samn){
    # exit if the population number is less than the sample size
    return("Population size is not enough for snowball sampling")
  }

  ind = c()
  V(G)$name = c(1:length(V(G)))
  starter = sample(1:length(V(G)),1)
  current = c()
  current[1] = V(G)$name[starter]
  count = 1
  ind[1] = current[1]
  while (count< samn){
    nnode = length(current) # the number of subjects in the current stage
    for (i in 1:nnode){
      ngh = neighbors(G, current[i]) # vertex index
      ind = c(ind, V(G)$name[ngh])
      ind = unique(ind)
    }
    tmp_sample = ind[(count+1):length(ind)]

    if (samn < length(ind)){ # if we reach more than the targeted sample size
      need = samn - count # number of subjects needed
      tmp_sample = sample(tmp_sample, need)
      ind[(count+1):samn] = tmp_sample
      ind = ind[-c((samn+1):length(ind))]
    }
    current = tmp_sample
    count = length(ind)
  }

  if(count == samn){
    subG = induced.subgraph(G, ind)
    return(list(subG = subG, ind = ind))
  }else{
    return("somthing goes wrong.")
  }
}
