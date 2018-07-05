#' Generate latent variable dependent network.
#'
#' @param n.node The number of nodes in network.
#' @param rho correlation coefficient between continuous observations and latent factor .
#' @param dep.factor multiplicative factor applied to.
#' \itemize{
#'    \item{If \code{dep.factor} < 0 }{Then \eqn{A_ij} \eqn{~} Bern (logistic ( \code{dep.factor}*| X_i - X_j |)) }
#'    \item{If \code{dep.factor} \eqn{\ge} 0}{Then \eqn{A_ij} \eqn{~} Bern (logistic ( \code{dep.factor} / | X_i - X_j |)) }
#'  }
#' @param dep.range a vector specifying lower bound and upper bound for \code{dep.factor}*| X_i - X_j |. Defaults to \code{c(-5, 5)}.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats rbinom
#' @importFrom igraph graph.adjacency V V<-
#'
#' @return an undirected and binary \code{igraph} object \code{G} having both \eqn{Y} and \eqn{U} as nodal attributes.
#' \item{\code{V(G)$outcome}}{one-dimensional continuous observations.}
#' \item{\code{V(G)$latent}}{one-dimensional continuous latent variable dependent on \code{V(G)$Y} through \code{rho}.}
#' @export
#'
#' @examples
#' library(netdep)
#' library(MASS)
#' library(mvrtn)
#' library(igraph)
#' G = latent.netdep(n.node = 100, rho = 0.5, dep.factor = 1)
#'
#'
latent.netdep = function(n.node, rho = 0.3, dep.factor = 1, dep.range = c(-5, 5)){

  Sigma = matrix(c(1, rho, rho, 1),2,2)
  sample = mvrnorm(n = n.node, mu = c(0,0), Sigma)
  X  = sample[,1]; Y = sample[,2]
  prob = matrix(0, n.node, n.node)
  A = matrix(0, n.node, n.node)
  for (i in 1:(n.node-1) ) {
    for (j in (i+1):(n.node)) {
      dif = abs(X[i] - X[j])
      if(dep.factor > 0 ){
        tmp = min(dep.factor/dif, dep.range[2])
      }else{
        tmp = max(dep.factor*dif, dep.range[1])
      }
      prob[i,j] = exp(tmp) / (1 + exp(tmp))
      A[i,j] = rbinom(1, 1, prob[i,j])
      A[j,i] = A[i,j]
    }
  }
  G = graph.adjacency(A, "undirected")
  V(G)$outcome = Y
  V(G)$latent = X
  return(G)
}
