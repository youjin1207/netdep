#' Generate direct transmission process
#'
#' Generate time-evolving outcomes where outcomes at time \eqn{t} of \eqn{i} depends on outcomes of \eqn{i}'s adjacent peers at time \eqn{t-1}.
#'
#' @param A \code{[n x n]} adjacency matrix.
#' @param max.time the maximum discrete time that direct transmission occurs.
#' @param mprob the maximum susceptibility probability, i.e. maximum probability that \eqn{i}'s outcome at time \eqn{t} depends on \eqn{i}'s peers at time \eqn{t-1}.
#' @param epsilon standard deviation of error process. This adds uncertainties in outcomes.
#'
#' For t=1,2, ... \code{max.time} :
#' \deqn{p ~ Unif(0, mprob)}
#' \deqn{Y^{t}_{i} = Y^t_i=  (1 - p)Y^{t-1}_i + p \sum\limits{j} A_{ij} Y^{t-1}_j / \sum_{j} A_{ij}  +  N(0, \epsilon) }{Y^t_i=  (1 - p)Y^{t-1}_i + p \sum_{j} A_ij Y^{t-1}_j / \sum_{j} A_ij  +  N(0, \epsilon) }
#'
#' @return a list of time-evolving outcomes from \code{time0} to \code{time(max.time)}.
#'
#' @importFrom stats rnorm runif
#' @export
#'
#' @examples
#' library(netdep)
#' library(igraph)
#' library(stats)
#' G = latent.netdep(n.node = 100, rho = 0.2)
#' A = as.matrix(get.adjacency(G))
#' outcomes = peer.process(A, max.time = 3, mprob = 0.3, epsilon = 0.5)
#'
#'
peer.process = function(A, max.time = 3, mprob = 0.5, epsilon = 0.3){
  popn = nrow(A)
  # Generate the initial popn's outcomes (at t = 0)
  outcome = matrix(0, ncol = popn, nrow = (max.time+1) )
  outcome[1,] = rnorm(popn, 0, 1)
  # outcome t=1,2,...,max.time
  for (t in 2:(max.time + 1)){
    for (i in 1:popn){
      p = runif(1,0, mprob) # Generate a new susceptibility probability
      deg = rowSums(A)[i] # the number of friends of subject i
      if (deg != 0){
        # the average of all of subject i's neighbor's outcomes at time (t-1)
        z = sum(A[1:popn,i]*outcome[t-1,1:popn]) / deg
      } else
        z = outcome[t-1, i]
      outcome[t,i] = (1 - p)*outcome[t-1,i] + p*z + rnorm(1, 0, epsilon)
    }
  }
  outcomes = list()
  for(t in 1:(max.time + 1)){
    outcomes[[t]] = as.numeric(outcome[t,])
  }
  names(outcomes) = as.character(paste("time" ,c(0:max.time), sep = ""))

  return(outcomes)
}
