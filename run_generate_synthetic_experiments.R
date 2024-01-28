library(jsonlite)

source("generate_synthetic_experiments.R")

# NOTE: ensure direction is 'min' in settings.json
settings <- fromJSON("settings.json")
stopifnot(settings$direction == 'min')

set.seed(0)
s <- synthmeta()

# add linebreaks to source names
source_name_linebreaks <- function(sources) {
  stopifnot(length(sources) == 1L)
  snames <- as.character(sources[[1]])
  snames_split <- strsplit(snames, "\\+")
  for (sn_idx in seq_along(snames_split)) {
    snames_split[[sn_idx]][2] <- paste0(snames_split[[sn_idx]][2], '<br>')
    snames_split[[sn_idx]] <- paste0(snames_split[[sn_idx]], collapse = '+')
  }
  snames_split <- unlist(snames_split)
  sources[[1]] <- snames_split
  return(sources)
}

s$sources_I <- source_name_linebreaks(s$sources_I)
s$sources_C <- source_name_linebreaks(s$sources_C)

# save to .csv files
write.csv(s$fd, "Data/feature_matrix.csv", row.names = T)
write.csv(s$cd, "Data/parameter_matrix.csv", row.names = T)
write.csv(s$pmat, "Data/performance_matrix.csv", row.names = T)
write.csv(s$sources_I, "Data/sources_I.csv", row.names = T)
write.csv(s$sources_C, "Data/sources_C.csv", row.names = T)
write.csv(s$centroids, "Data/centroids.csv", row.names = T)
write.csv(s$beta, "Data/beta.csv", row.names = T)
