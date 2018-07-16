#' Moran's I statistic
#'
#' This function calculates Moran's I statistic given adjacency matrix and outcome.
#' The statistic can be used for testing network dependence.
#'
#' @param A \code{[n x n]} adjacency matrix or general relational weight matrix of which \eqn{A_ij} indicates relationship from \eqn{i} to \eqn{j}.
#' @param Y a vector of \eqn{n} continuous or binary, one-dimensional observations.
#'
#' @return
#'  \item{\code{moran}}{a standardized Moran's \eqn{I} statistic.}
#' @export
#'
#' @author Youjin Lee
#'
MoranI = function(A, Y){

  m = length(Y)

  s0 = sum(A + t(A)) / 2
  s1 = sum((A + t(A))^2) / 2
  s2 = sum((apply(A,1,sum) + apply(A, 2, sum))^2)

  b2 = sum((Y - mean(Y))^2) / m
  b4 = sum((Y - mean(Y))^4) / m

  II = t(Y - mean(Y)) %*% A %*% (Y - mean(Y)) / (s0*b2)
  II.mean = -1/(m-1)
  II.meansq = s1*(m*b2^2-b4)/(s0^2*b2^2*(m-1)) +
    (s2-2*s1)*(2*b4-m*b2^2) / (s0^2*b2^2*(m-1)*(m-2)) +
    (s0^2 - s2 + s1) * (3*m*b2^2-6*b4) / (s0^2*b2^2*(m-1)*(m-2)*(m-3))
  II.var = II.meansq - II.mean^2
  II.std = (II - II.mean) / sqrt(II.var)
  return(moran = II.std)
}

#' Permutation Test of Moran's I
#'
#' @param A \code{[n x n]} adjacency matrix or general relational weight matrix of which \eqn{A_ij} indicates relationship from \eqn{i} to \eqn{j}.
#' @param Y a vector of \eqn{n} continuous or binary, one-dimensional observations.
#' @param np the number of permutation samples.
#'
#' @return
#' \item{\code{moran}}{a standardized Moran's \eqn{I} statistic.}
#' \item{\code{pval.z}}{p-value of a standardized Moran's \eqn{I} statistic assuming asymptotic normality.}
#' \item{\code{pval.permute}}{p-value of a standardized Moran's \eqn{I} statistic using \code{np} independent permutation samples.}
#' @export
#'
#' @import igraphdata
#' @importFrom igraph graph.adjacency get.adjacency
#' @importFrom stats pnorm
#'
#'
#' @author Youjin Lee
#'
#' @examples
#' library(netdep)
#' library(igraph)
#' library(igraphdata)
#' data(karate)
#' A = as.matrix(get.adjacency(karate, attr= "weight", sparse = TRUE)) # weighted adjacency matrix
#' Y = V(karate)$Faction
#' result = make.permute.moran(A, Y, np = 100)
#'
#'
make.permute.moran = function(A, Y, np = 100){
  popn = length(Y)
  pval = 1
  moran = MoranI(A, Y)
  for(q in 2:np){
    newA = matrix(NA, ncol = popn, nrow = popn)
    s = sample(c(1:popn), popn, replace =FALSE)
    # construct the new adjacency matrix
    newA[s, s] = A
    pval = ifelse(MoranI(newA, Y) >= moran , pval+1, pval)
  }
  pval = pval / np
  return(list(moran = moran, pval.z = pnorm(moran), pval.permute = pval))
}


#' Calculate \eqn{\Phi} statistic
#'
#' This is an auxiliary function to calculate non-standardized \eqn{\Phi} statistic and its first and second moment.
#'
#' @param A \code{[n x n]} adjacency matrix or general relational weight matrix of which \eqn{A_ij} indicates relationship from \eqn{i} to \eqn{j}.
#' @param Y a vector of \eqn{n} nominal or binary, one-dimensional observations.
#'
#' @return
#' \item{\code{rawphi}}{Non-standardized \eqn{\Phi} statistic.}
#' \item{\code{m1.rawphi}}{the first (permutation) moment of \code{rawphi}.}
#' \item{\code{m2.rawphi}}{the second (permutation) moment of \code{rawphi}.}
#' @export
#'
#' @author Youjin Lee
#'
phi.moment = function(A, Y){

  A = as.matrix(A)
  n = length(Y)
  pvec = as.vector(table(Y)) / n
  k = length(pvec)

  B = matrix(0, nrow = nrow(A), ncol = ncol(A))
  for(i in 1:n){
    prob1 = pvec[which(Y[i] == names(table(Y)))]
    B[i,] = A[i,] / prob1
  }
  for(j in 1:n){
    prob2 = pvec[which(Y[j] == names(table(Y)))]
    B[,j] = B[,j] / prob2
  }
  rawphi = 0
  for(i in 1:n){
    for(j in 1:n){
      rawphi = rawphi + B[i,j]*(2*(Y[i] == Y[j]) - 1)
    }
  }


  s0 = sum(A + t(A)) / 2
  s1 = sum((A + t(A))^2) / 2
  s2 = sum((apply(A,1,sum) + apply(A, 2, sum))^2)
  m1.rawphi =  (s0/(n*(n-1)))*(n^2*k*(2-k) - n*sum(1/pvec))

  Q1 = sum(1/pvec)
  Q2 = sum(1/pvec^2)
  Q3 = sum(1/pvec^3)
  Q22 = sum((1/pvec)%*%t(1/pvec))
  E1 = (n^2*Q22 - n*Q3)/(n*(n-1))
  E2 = 4*n^3*Q1 - 4*n^3*k*Q1 + n^3*k^2*Q1 - 2*( 2*n^2*Q2 - n^2*k*Q2) + 2*n*Q3 - n^2*Q22
  E2 = E2/(n*(n-1)*(n-2))

  A1 = 4*n^4*k^2 - 4*n^4*k^3 + n^4*k^4 - (2*n^3*k*Q1 - n^3*k^2*Q1)
  A2 = 4*n^3*Q1 - 4*n^3*k*Q1 + n^3*k^2*Q1 - (2*n^2*Q2 - n^2*k*Q2)
  Apart = A1 - 2*A2

  B1 = 4*n^3*Q1 - 4*n^3*k*Q1 + n^3*k^2*Q1 - (2*n^2*Q2 - n^2*k*Q2)
  B2 = 2*n^2*Q2 - n^2*k*Q2 - n*Q3
  B3 = n^2*Q22 - n*Q3
  Bpart = B1 - B2 - B3

  C1 = 2*n^3*k*Q1 - n^3*k^2*Q1 - n^2*Q22
  C2 = 2*n^2*Q2 - n^2*k*Q2 - n*Q3
  Cpart = C1 - 2*C2

  E3 = (Apart - 2*Bpart - Cpart) / (n*(n-1)*(n-2)*(n-3))

  m2.rawphi = s1*E1 + (s2 - 2*s1)*E2 + (s0^2 - s2 + s1)*E3

  return(list(rawphi = rawphi, m1.rawphi = m1.rawphi, m2.rawphi = m2.rawphi))
}


#' Standardized \eqn{\Phi} statistic
#'
#' A test statistic of \eqn{\Phi} is to test network dependence in categorical observations.
#'
#' @param A \code{[n x n]} adjacency matrix or general relational weight matrix of which \eqn{A_ij} indicates relationship from \eqn{i} to \eqn{j}.
#' @param Y a vector of \eqn{n} nominal or binary, one-dimensional observations.
#'
#' @return
#' \item{\code{phi}}{a standardized \eqn{\Phi} statistic.}
#'
#' @author Youjin Lee
#' @export
#'
phi.stat = function(A, Y){

  phi.result = phi.moment(A, Y)
  rawphi = phi.result$rawphi

  mean.rawphi = phi.result$m1.rawphi
  var.rawphi = phi.result$m2.rawphi - mean.rawphi^2
  std.phi = (rawphi - mean.rawphi) / sqrt(var.rawphi)
  return(phi = std.phi)
}


#' Permutation Test of \eqn{\Phi}
#'
#' This function prints out the network dependence test results for categorical observations.
#'
#' @param A \code{[n x n]} adjacency matrix or general relational weight matrix of which \eqn{A_ij} indicates relationship from \eqn{i} to \eqn{j}.
#' @param Y a vector of \eqn{n} continuous or binary, one-dimensional observations.
#' @param np  the number of permutation samples.
#'
#' @return
#' \item{\code{phi}}{a standardized \eqn{\Phi} statistic.}
#' \item{\code{pval.z}}{p-value of a standardized \eqn{\Phi} statistic assuming asymptotic normality.}
#' \item{\code{pval.permute}}{p-value of a standardized \eqn{\Phi} statistic using \code{np} independent permutation samples.}
#'
#' @importFrom stats pnorm
#' @export
#' @author Youjin Lee
#'
#'
#' @examples
#' library(netdep)
#' library(igraph)
#' library(igraphdata)
#' data(UKfaculty)
#' A = as.matrix(get.adjacency(UKfaculty, attr= "weight", sparse = TRUE)) # weighted adjacency matrix
#' Y = V(UKfaculty)$Group
#' result = make.permute.Phi(A, Y, np = 50)
#'
#'
make.permute.Phi = function(A, Y, np){
  popn = length(Y)
  pval = 1
  phi = phi.stat(A, Y)
  for(q in 2:np){
    newA = matrix(NA, ncol = popn, nrow = popn)
    s = sample(c(1:popn), popn, replace =FALSE)
    # construct the new adjacency matrix
    newA[s, s] = A
    pval = ifelse(phi.stat(newA,Y) >= phi , pval+1, pval)
  }
  pval = pval / np
  return(list(phi = phi, pval.z = 1-pnorm(phi), pval.permute = pval))
}


