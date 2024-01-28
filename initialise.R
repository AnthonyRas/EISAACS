library(jsonlite)
library(scales)
library(viridis)

source("preprocess.R")
source("keras_model.R")
source("get_hoverinfo.R")


read <- function() {
  # read data matrices
  pmat <- as.matrix(read.csv("Data/performance_matrix.csv", row.names = 1, check.names = F))
  fd <- read.csv("Data/feature_matrix.csv", row.names = 1, check.names = F)
  cd <- read.csv("Data/parameter_matrix.csv", row.names = 1, check.names = F)
  sources_I <- read.csv("Data/sources_I.csv", row.names = 1, check.names = F)
  sources_C <- read.csv("Data/sources_C.csv", row.names = 1, check.names = F)
  # read settings file, which should not contain any null values
  settings <- fromJSON("settings.json")
  stopifnot("settings.json should not contain any null values." = !any(sapply(settings, is.null)))
  list(datalist = list(pmat = pmat, fd = fd, cd = cd, sources_I = sources_I, sources_C = sources_C), settings = settings)
}


init <- function(read_out = NULL, models = NULL, initial = T, seed = 0L) {
  set.seed(seed = seed)
  set_random_seed(seed = seed)
  if (is.null(read_out)) {
    read_out <- read()
  }
  preproc_out <- preprocess(read_out$datalist, read_out$settings, initial = initial)
  settings <- read_out$settings
  sources_I <- read_out$datalist$sources_I
  sources_C <- read_out$datalist$sources_C

  if (is.null(models)) {
    models <- projection_model(preproc_out$long_metadata,
                               settings$train$layer_size,
                               settings$train$batch_size,
                               settings$train$epochs,
                               as.list(settings$train$loss_weights))
  }
  # get performance data
  pmat <- preproc_out$pmat
  # get points in instance space
  fmat <- preproc_out$fmat
  Z <- as.matrix(models$projector_features_model(fmat))
  Z <- data.frame(Z1 = Z[, 1], Z2 = Z[, 2])
  row.names(Z) <- row.names(fmat)
  # get points in configuration space
  cmat <- preproc_out$cmat
  alpha <- as.matrix(models$projector_params_model(cmat))
  alpha <- data.frame(C1 = alpha[, 1], C2 = alpha[, 2])
  row.names(alpha) <- row.names(cmat)
  # get raw features and raw parameters corresponding to 'fmat', 'cmat'
  
  
  # NOTE: if using feature/paraameter selection redo preprocess
  
  
  fd <- read_out$datalist$fd[row.names(fmat), ]
  cd <- read_out$datalist$cd[row.names(cmat), ]
  pmat_raw <- read_out$datalist$pmat[row.names(fmat), row.names(cmat)]
  # formatting feature names
  nf <- !grepl("feature_", colnames(fmat))
  colnames(fmat)[nf] <- paste0("feature_", colnames(fmat)[nf])
  nf <- !grepl("feature_", colnames(fd))
  colnames(fd)[nf] <- paste0("feature_", colnames(fd)[nf])
  # formatting parameter names
  np <- !grepl("parameter_", colnames(cmat))
  colnames(cmat)[np] <- paste0("parameter_", colnames(cmat)[np])
  np <- !grepl("parameter_", colnames(cd))
  colnames(cd)[np] <- paste0("parameter_", colnames(cd)[np])
  # get hovertext over instance space and configuration space, both raw and processed features/parameters
  hovertextI_raw <- get_hoverinfo(data.frame(Z, fd), "I")
  hovertextI_proc <- get_hoverinfo(data.frame(Z, fmat), "I")
  hovertextC_raw <- get_hoverinfo(data.frame(alpha, cd), "C")
  hovertextC_proc <- get_hoverinfo(data.frame(alpha, cmat), "C")
  # initialise data for plotting instances and configurations
  plotdata_Z <- data.frame(Z1 = Z$Z1, Z2 = Z$Z2, opacity = 1, val = 0, selected = T)
  plotdata_alpha <- data.frame(Z1 = alpha$C1, Z2 = alpha$C2, opacity = 1, val = 0, selected = T)
  
  # save objects to RWorkspace
  save_model_tf(models$prediction_model, "Data/prediction_model")
  save_model_tf(models$projector_features_model, "Data/projector_features_model")
  save_model_tf(models$projector_params_model, "Data/projector_params_model")
  save(fmat, file = "Data/fmat.RData")
  save(fd, file = "Data/fd.RData")
  save(cmat, file = "Data/cmat.RData")
  save(cd, file = "Data/cd.RData")
  save(pmat, file = "Data/pmat.RData")
  save(pmat_raw, file = "Data/pmat_raw.RData")
  save(sources_I, file = "Data/sources_I.RData")
  save(sources_C, file = "Data/sources_C.RData")
  save(hovertextI_raw, file = "Data/hovertextI_raw.RData")
  save(hovertextI_proc, file = "Data/hovertextI_proc.RData")
  save(hovertextC_raw, file = "Data/hovertextC_raw.RData")
  save(hovertextC_proc, file = "Data/hovertextC_proc.RData")
  save(plotdata_Z, file = "Data/plotdata_Z.RData")
  save(plotdata_alpha, file = "Data/plotdata_alpha.RData")
}
