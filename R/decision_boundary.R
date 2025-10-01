# library(remotes)
# install_github('andrewheiss/qatarcars')
# install_github('matloff/qeML')
library(qeML)

scale_features <- function(df, target_col) {
  cols_to_scale <- names(df) != target_col
  df[cols_to_scale] <- lapply(df[cols_to_scale], function(col) {
    as.numeric(scale(col))
  })
  df[[target_col]] <- as.numeric(scale(df[[target_col]], center=TRUE, scale=FALSE))
  df
}

draw_boundary <- function(df, target_col, target_val, graph_at_once = TRUE) {
  df <- factorsToDummies(df, omitLast=TRUE, dfOut=TRUE)
  
  target_val <- target_val - mean(df[[target_col]])
  df <- scale_features(df, target_col)
  linear_boundary <- function(x1, x2, ...) {
    modC <- lm(df[[target_col]] ~ x1 + x2-1, data = df)
    coefC <- coef(modC)
    
    if (length(coefC) >= 2 && !any(is.na(coefC))) {
      Biso <- (target_val - coefC[1] * x1) / coefC[2]
      
      lines(x1, Biso, col="blue", lwd = 2)
    }
  }
  
  if (graph_at_once) {
    pairs(df,
          lower.panel = linear_boundary,     
          upper.panel = linear_boundary, 
          pch = 19,)
  } else {
    all_pairs <- combn(names(df), 2, simplify=FALSE)
    for (pair in all_pairs) {
      x1 <- df[[pair[1]]]
      x2 <- df[[pair[2]]]
      
      plot.new()
      plot.window(xlim = range(x1, na.rm = TRUE), ylim = range(x2, na.rm = TRUE))
      axis(1)
      axis(2)
      title(main = paste(pair[1], "vs", pair[2]), 
            xlab = pair[1], 
            ylab = pair[2])
      linear_boundary(x1, x2)
    }
  }
}

draw_boundary(mlb1, 'Weight', 200)
