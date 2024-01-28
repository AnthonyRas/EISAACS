get_hoverinfo <- function(df, type) {
  # function to map values to colours
  if (type == "I") {
    col_fun <- col_numeric("viridis", domain = c(0, 1))
  } else {
    col_fun <- col_numeric(rocket(100), domain = c(0, 1))
  }
  # normalised 'df' for input to 'col_fun'
  df_norm <- predict(caret::preProcess(df, method = "range"), df)
  # output a vector of type 'character', containing hover text for each row of 'df'
  sapply(seq_len(nrow(df)), function(i) {
    # get hover text for row i
    text_i <- sapply(colnames(df), function(nm) {
      if (is.numeric(df[i, nm])) {
        if (grepl("feature_", nm) || grepl("parameter_", nm)) {
          text_colour <- col_fun(df_norm[i, nm])
          sprintf("<span style='color:%s'><b>%s</b>: %.3g</span>", text_colour, nm, signif(df[i, nm], 3))
        } else {
          sprintf("<b>%s</b>: %.3g", nm, signif(df[i, nm], 3))
        }
      } else {
        sprintf("<b>%s</b>: %s", nm, df[i, nm])
      }
    })
    # get text associated with features or parameters
    attrs_i <- text_i[grepl("(feature_)|(parameter_)", names(text_i))]
    attrs_i <- gsub("(feature_)|(parameter_)", "", attrs_i)
    # format hover text for row i
    parts <- vector("character")
    if (type == "I") {
      parts[1] <- "<b>Instance Space</b><br>"
      s1 <- "<b>Instance</b>: "
      s2 <- "<br><b>Features</b><br>"
    } else {
      stopifnot(type == "C")
      parts[1] <- "<b>Configuration Space</b><br>"
      s1 <- "<b>Configuration</b>: "
      s2 <- "<br><b>Parameters</b><br>"
    }
    parts[2] <- paste0(paste(text_i[1:2], collapse = "<br>"), "<br>")
    parts[3] <- paste0(paste0(s1, row.names(df)[i]), "<br>")
    parts[4] <- s2
    parts[5] <- paste(attrs_i, collapse = "<br>")
    paste0(parts, collapse = "")
  })
}