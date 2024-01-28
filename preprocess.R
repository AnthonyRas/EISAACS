library(tidyr)
library(matrixStats)
library(caret)

# NOTE: have an option for additional preprocessing (e.g. box-cox transformation etc.)

# preprocess the feature matrix
preproc_features <- function(fd, initial = T, do_read = T, rl = list()) {
  if (initial) {
    # initial preprocess
    f_dvs <- caret::dummyVars("~.", data = fd)
    fmat <- scale(predict(f_dvs, newdata = fd))
    stopifnot(!file.exists("Data/fmat_dvs.RData"))
    save(f_dvs, file = "Data/fmat_dvs.RData")
    write.csv(t(as.matrix(attr(fmat, "scaled:center"))),
              "Data/fmat_center.csv", row.names = F)
    write.csv(t(as.matrix(attr(fmat, "scaled:scale"))),
              "Data/fmat_scale.csv", row.names = F)
  } else if (do_read) {
    # preprocess using saved preprocessing parameters
    load("Data/fmat_dvs.RData")
    centers <- unlist(read.csv("Data/fmat_center.csv"))
    scales <- unlist(read.csv("Data/fmat_scale.csv"))
    fmat <- scale(predict(f_dvs, newdata = fd), center = centers, scale = scales)
  } else {
    # preprocess using saved preprocessing parameters, using an input list instead
    fmat <- scale(predict(rl$f_dvs, newdata = fd), center = rl$centers, scale = rl$scales)
  }
  attr(fmat, "scaled:center") <- NULL; attr(fmat, "scaled:scale") <- NULL
  return(fmat)
}


# preprocess the parameter matrix
preproc_configs <- function(cd, initial = T, do_read = T, rl = list()) {
  if (initial) {
    # initial preprocess
    c_dvs <- caret::dummyVars("~.", data = cd)
    cmat <- scale(predict(c_dvs, newdata = cd))
    stopifnot(!file.exists("Data/cmat_dvs.RData"))
    save(c_dvs, file = "Data/cmat_dvs.RData")
    write.csv(t(as.matrix(attr(cmat, "scaled:center"))),
              "Data/cmat_center.csv", row.names = F)
    write.csv(t(as.matrix(attr(cmat, "scaled:scale"))),
              "Data/cmat_scale.csv", row.names = F)
  } else if (do_read) {
    # preprocess using saved preprocessing parameters
    load("Data/cmat_dvs.RData")
    centers <- unlist(read.csv("Data/cmat_center.csv"))
    scales <- unlist(read.csv("Data/cmat_scale.csv"))
    cmat <- scale(predict(c_dvs, newdata = cd), center = centers, scale = scales)
  } else {
    # preprocess using saved preprocessing parameters, using an input list instead
    cmat <- scale(predict(rl$c_dvs, newdata = cd), center = rl$centers, scale = rl$scales)
  }
  attr(cmat, "scaled:center") <- NULL; attr(cmat, "scaled:scale") <- NULL
  return(cmat)
}


# preprocess the performance matrix
preproc_perfs <- function(pmat, direction) {
  if (direction == "min") {
    pmat <- -pmat
  } else {
    stopifnot(direction == "max")
  }
  # instance-wise normalisation
  rmins <- rowMins(pmat)
  rmaxs <- rowMaxs(pmat)
  pmat <- (pmat - rmins)/(rmaxs - rmins)
  return(pmat)
}


# get long metadata dataframe with row per evaluation of a config on an instance
# INPUT:
# list of fd (dataframe), cd (dataframe), pmat (matrix)
# settings (list)
# OUTPUT:
# list of long_metadata,
# fmat, cmat, pmat (preprocessed)
# fd, cd, pmat (raw)
get_long_metadata <- function(datalist, settings, initial = T) {
  fd <- datalist$fd; cd <- datalist$cd; raw_pmat <- datalist$pmat
  n_inst <- nrow(fd); n_config <- nrow(cd)

  fmat <- preproc_features(fd, initial)
  cmat <- preproc_configs(cd, initial)
  pmat <- preproc_perfs(raw_pmat, settings$direction)

  # convert from wide to long format
  wide_metadata <- data.frame("instances" = row.names(pmat), pmat)
  cnm <- colnames(wide_metadata)
  ncl <- ncol(wide_metadata)
  colnames(wide_metadata)[2:ncl] <- colnames(pmat)
  
  long_metadata <- pivot_longer(wide_metadata, cols = 2:ncol(wide_metadata),
                                names_to = "config", values_to = "value")
  long_metadata <- cbind(long_metadata, cmat[long_metadata$config, ])
  long_metadata <- cbind(long_metadata, fmat[long_metadata$instances, ])
  
  return(list(long_metadata = long_metadata,
              fmat = fmat, cmat = cmat, pmat = pmat,
              fd = fd, cd = cd, raw_pmat = raw_pmat))
}


# preprocesses the data for training the projection model
# INPUT:
# list of fmat, cmat, pmat
# settings list
# OUTPUT:
# list of long_metadata,
# fmat, cmat, pmat (preprocessed)
# fmat, cmat, pmat (raw)
preprocess <- function(datalist, settings, initial = T) {
  lmet <- get_long_metadata(datalist, settings, initial)
  
  long_metadata <- lmet$long_metadata
  fmat <- lmet$fmat; cmat <- lmet$cmat; pmat <- lmet$pmat
  params <- colnames(cmat)
  features <- colnames(fmat)
  
  long_metadata <- long_metadata[, c("instances", "config", "value", params, features)]
  attributes(long_metadata)$features <- features
  attributes(long_metadata)$params <- params
  
  return(list(long_metadata = long_metadata,
              fmat = fmat, cmat = cmat, pmat = pmat,
              fd = lmet$fd, cd = lmet$cd, raw_pmat = lmet$raw_pmat))
}
