library(mvtnorm)
library(trialr)
library(pdist)


# synthetic experiments
# INPUT:
# m: number of features
# ni: number of instances
# nc: number of configurations
# sigma: standard deviation for configuration sampling
# k: number of instance groups
# OUTPUT:
# fmat: feature matrix
# cmat: parameter matrix
# pmat: performance matrix
synthmeta <- function(m = 4, ni = 180, nc = 180, sigma = 0.15, k = 3) {
  stopifnot(nc %% k == 0)
  P <- rlkjcorr(1, m) # sample a covariance matrix P
  fmat <- rmvnorm(ni, rep(0, m), sigma = P)
  fmat <- scale(fmat)  # feature matrix
  attr(fmat, "scaled:center") <- NULL; attr(fmat, "scaled:scale") <- NULL
  # get instance group memberships
  centroids_idxs <- sample(nrow(fmat), k, replace = FALSE)
  centroids <- fmat[centroids_idxs, ]
  distances <- matrix(pdist(fmat, centroids)@dist, ncol = nrow(centroids), byrow = T)
  membership <- apply(distances, 1, which.min)

  # set up ground truth
  beta <- rmvnorm(k, rep(0, m))  # ground truth regression coefficients for each cluster
  yg <- fmat %*% t(beta)
  yg <- yg[cbind(seq_len(nrow(fmat)), membership)]  # ground truth regression labels

  # generate nc/k configurations around each ground truth coefficient vector
  configs_list <- list()
  sources_C <- c()
  for (r in seq_len(nrow(beta))) {
    configs_list[[length(configs_list)+1]] <- rmvnorm(nc/k, mean = beta[r, ],
                                                      sigma = sigma^2 * diag(m))
    sources_C <- c(sources_C, rep(r, nc/k))
  }
  cmat <- do.call(rbind, configs_list)  # parameter matrix
  
  pmat <- abs(sweep(fmat %*% t(cmat), 1, yg, "-"))  # absolute errors
  # set column and row names
  colnames(fmat) <- paste0("F", 1:ncol(fmat)); rownames(fmat) <- paste0("I", 1:nrow(fmat))
  colnames(cmat) <- paste0("P", 1:ncol(cmat)); rownames(cmat) <- paste0("C", 1:nrow(cmat))
  rownames(pmat) <- rownames(fmat); colnames(pmat) <- rownames(cmat)
  
  # categorise instances/configurations by the 'beta' coefficients of their associated centroid
  source_names <- unlist(lapply(seq_len(k),
                function(i) {
                  paste0(paste0(
                    round(beta,2)[i, ],
                    paste0(" ", colnames(fmat))), collapse = " + ")
                }))
  
  sources_I <- source_names[membership]
  sources_I <- data.frame(source = factor(sources_I))
  row.names(sources_I) <- row.names(fmat)
  
  sources_C <- source_names[sources_C]
  sources_C <- data.frame(source = factor(sources_C))
  row.names(sources_C) <- row.names(cmat)
  
  return(list(fd = data.frame(fmat), cd = data.frame(cmat), pmat = pmat, sources_I = sources_I, sources_C = sources_C,
              centroids = centroids, beta = beta))
}


# function to get performance matrix 'pmat' for new data
pmat_meta <- function(fmat, cmat, centroids, beta) {
  distances <- matrix(pdist(fmat, centroids)@dist, ncol = nrow(centroids), byrow = T)
  membership <- apply(distances, 1, which.min)
  yg <- fmat %*% t(beta)
  yg <- yg[cbind(seq_len(nrow(fmat)), membership)]  # ground truth regression labels
  pmat <- abs(sweep(fmat %*% t(cmat), 1, yg, "-"))  # absolute errors
  return(pmat)
}
