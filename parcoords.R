library(plotly)


parcoords_init <- function(d, y, pmat_raw) {
  isnum <- unlist(lapply(d, is.numeric))  # is numeric?
  stopifnot(is.numeric(y))
  isnum <- c(y = TRUE, isnum)  # NOTE: assume numeric performance metric 
  
  # convert all factors to numeric, for the parcoords plot
  dnum <- data.frame(lapply(d, as.numeric))
  dnum <- cbind(y=y, dnum)
  row.names(dnum) <- row.names(d)
  
  nm <- names(isnum)  # dimension names
  nm2 <- gsub("feature_", "", gsub("parameter_", "", nm))  # shorter dimension names
  flevs <- lapply(d, levels)  # factor levels
  # for 'dimensions' argument of parcoords plot
  dimensions <- list()
  for (dIdx in seq_along(isnum)) {
    if (isnum[dIdx]) {
      if (nm[dIdx] != "y") {
        lb <- min(d[[nm[dIdx]]])
        ub <- max(d[[nm[dIdx]]])
      } else {
        lb <- min(pmat_raw)  # NOTE: what if there are outliers?
        ub <- max(pmat_raw)
      }
      dimensions[[length(dimensions) + 1]] <- 
        list(range = c(lb, ub),
             label = nm2[dIdx], values = as.formula(sprintf("~%s", nm[dIdx])))
    } else {
      dimensions[[length(dimensions) + 1]] <- 
        list(tickvals = seq_along(flevs[[nm[dIdx]]]),
             ticktext = flevs[[nm[dIdx]]],
             label = nm2[dIdx],
             values = as.formula(sprintf("~%s", nm[dIdx]))
             )
    }
  }
  
  # NOTE: jitter for categorical variables
  # dnum[, !isnum] <- runif(nrow(dnum), 0.15, 0.30) + dnum[, !isnum]
  
  return(list(dnum=dnum, dimensions=dimensions))
}
