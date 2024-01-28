library(GA)
library(tensorflow)
library(keras)
library(matrixStats)

source("preprocess.R")
source("generate_synthetic_experiments.R")
source("initialise.R")
attach("Data/cd.RData")
attach("Data/fd.RData")

# load tensorflow models
projector_params_model <- load_model_tf("Data/projector_params_model")
projector_features_model <- load_model_tf("Data/projector_features_model")
prediction_model <- load_model_tf("Data/prediction_model")

set.seed(0)

target <- c(0.5, -0.5)

# get lower and upper bounds of parameter values for genetic algorithm
stopifnot("For generating new configurations, all parameters are assumed for now to be numeric." = all(unlist(lapply(cd, is.numeric))))
c_lb <- colMins(as.matrix(cd))
c_ub <- colMaxs(as.matrix(cd))

# for preprocessing of parameter values in fitness function
load("Data/cmat_dvs.RData")
c_centers <- unlist(read.csv("Data/cmat_center.csv"))
c_scales <- unlist(read.csv("Data/cmat_scale.csv"))
c_rl <- list(c_dvs = c_dvs, centers = c_centers, scales = c_scales)

c_fitness <- function(x, eps = 1e-8) {
  x <- setNames(data.frame(as.list(x)), c_rl$c_dvs$vars)
  x <- preproc_configs(x, initial = F, do_read = F, rl = c_rl)
  v <- projector_params_model(matrix(x, nrow=1, byrow=T))
  obj <- as.numeric(1/(eps + sum((v - target)^2)))
  return(obj)
}

c_gares <- ga(type = "real-valued", fitness = c_fitness, lower = c_lb, upper = c_ub, pmutation = 0.4)

# get population and preprocessed population
X <- c_gares@population
colnames(X) <- c_rl$c_dvs$vars
Xprime <- preproc_configs(X, initial = F, do_read = F, rl = c_rl)

# Genetic Algorithm Trials
## NOTE: Uncomment to generate plot to verify consistency of genetic algorithm
# ga_repeats <- function(num_trials = 30L) {
#   trial_solutions <- c_gares@solution  # include first solution
#   colnames(trial_solutions) <- c_rl$c_dvs$vars
#   
#   # 30 trials in total, including first above
#   for (trial in 1L:(num_trials-1L)) {
#     set.seed(trial)
#     trial_res <- ga(type = "real-valued", fitness = c_fitness, lower = c_lb, upper = c_ub, pmutation = 0.4)
#     trial_solutions <- rbind(trial_solutions, trial_res@solution)
#   }
#   
#   # project trial solutions to configuration space
#   project_population <- function(population) {
#     colnames(population) <- c_rl$c_dvs$vars
#     preprocessed_population <- preproc_configs(population, initial = F, do_read = F, rl = c_rl)
#     V <- projector_params_model(as.matrix(preprocessed_population))
#     V <- as.matrix(V)
#     # NOTE: same format as Data/plotdata_alpha.RData
#     project_pop_df <- data.frame(Z1 = V[, 1], Z2 = V[, 2], opacity = 1, val = 0, selected = TRUE)
#     return(project_pop_df)
#   }
#   
#   project_pop_df <- project_population(trial_solutions)
#   
#   # Add best of each trial to original points
#   load("Data/plotdata_alpha.RData")
#   project_orig_df <- plotdata_alpha
#   project_orig_df$val <- "Original"
#   project_pop_df$val <- "Best of Each Trial"
#   
#   d_pop <- rbind(project_orig_df, project_pop_df)
#   
#   title.text <- '<b>Configuration Space</b>'
#   x.title <- '<b>C1</b>'
#   y.title <- '<b>C2</b>'
#   
#   d_pop$val <- factor(d_pop$val)
#   
#   # Plot best of each trial + original points
#   p <- plot_ly()
#   
#   cats <- levels(d_pop$val)
#   catcols <- setNames(pal_brewer(palette = "Set2")(length(cats)), cats)  # NOTE: this assumes that the number of categories is no more than 8
#   for (cat in cats) {
#     whichcat <- which(d_pop$val == cat)
#     p <- p %>% add_trace(data = d_pop[whichcat, ], x = ~Z1, y = ~Z2, 
#                          type = 'scatter', mode = 'markers',
#                          showlegend = T,
#                          marker = list(color = catcols[cat], opacity = ~opacity, symbol = 'triangle-up-open', line = list(width = 0)),
#                          size = 1, legendgroup = "C", name = cat)
#   }
#   
#   p <- p %>% add_trace(x = target[1], y = target[2], type = 'scatter', mode = 'markers', showlegend = T,
#                        marker = list(color = 'red', symbol = 'x-open-dot', line = list(width = 0), size = 20), name = 'Target')
#   
#   
#   p <- p %>% layout(title = list(text = title.text, x = 0.05), 
#                     plot_bgcolor = "rgb(255, 255, 255)", paper_bgcolor = "rgb(255, 255, 255)",
#                     xaxis = list(title = x.title),
#                     yaxis = list(title = y.title),
#                     showlegend = T, margin = list(t = 55, b = 60, pad = 10),
#                     legend = list(x = 0.5, y = 0.8))
#   return(list(p=p, d_pop=d_pop, project_pop_df=project_pop_df, trial_solutions=trial_solutions))
# }
# 
# # NOTE: Uncomment to generate plot to verify consistency of genetic algorithm
# ga_repeats_res <- ga_repeats()

# add population to configuration dataframe
cdnew <- data.frame(rbind(as.matrix(cd), X))
colnames(cdnew) <- colnames(cd)
rownames(cdnew) <- paste0("C", seq_len(nrow(cdnew)))

# update performance matrix
centroids <- read.csv("Data/centroids.csv", row.names = 1)
beta <- read.csv("Data/beta.csv", row.names = 1)
new_pmat_raw <- pmat_meta(as.matrix(fd), as.matrix(cdnew), centroids, beta)

# for compatibility with 'init' function
fd_copy <- fd
colnames(fd_copy) <- sub("feature_", "", colnames(fd_copy))
colnames(cdnew) <- sub("parameter_", "", colnames(cdnew))

# get instance sources as before
sources_I <- read.csv("Data/sources_I.csv", row.names = 1)
# in sources, differentiate between the original configurations and the new configurations
sources_C <- factor(c(rep("Original", nrow(cd)), rep("New", nrow(X))))
sources_C <- data.frame(source = sources_C)
row.names(sources_C) <- row.names(cdnew)
settings <- fromJSON("settings.json")

read_out <- list(datalist = list(pmat = new_pmat_raw, fd = fd_copy, cd = cdnew, sources_I = sources_I, sources_C = sources_C), settings = settings)

models <- list(projector_params_model = projector_params_model,
               projector_features_model = projector_features_model,
               prediction_model = prediction_model)

init(read_out = read_out, models = models, initial = F)
