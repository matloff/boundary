# install.packages('FNN')
library(FNN)
library(dplyr)
library(qeML)

get_points_with_target <- function(k, df, target_col, target_min, target_max) {
  nn_indices <- get.knn(df, k)$nn.index
  target_vals <- df[[target_col]]
  neighbor_target_vals <- matrix(target_vals[nn_indices], nrow=nrow(df))
  df$est_mean <- rowMeans(neighbor_target_vals)
  filter(df, between(est_mean, target_min, target_max))[, colnames(df) != 'est_mean']
}

scale_features <- function(df, target_col) {
  cols_to_scale <- names(df) != target_col
  df[cols_to_scale] <- lapply(df[cols_to_scale], function(col) {
    as.numeric(scale(col))
  })
  df[[target_col]] <- as.numeric(scale(df[[target_col]], center=TRUE, scale=FALSE))
  df
}

draw_boundary <- function(k, df, target_col, target_min, target_max, graph_at_once = TRUE) {
  df <- factorsToDummies(df, omitLast=TRUE, dfOut=TRUE)
  
  target_min <- target_min - mean(df[[target_col]])
  target_max <- target_max - mean(df[[target_col]])
  df <- scale_features(df, target_col)
  df <- get_points_with_target(k, df, target_col, target_min, target_max)
  
  knn_boundary <- function(x1, x2, ...) {
    lines(loess(x2 ~ x1, data = df), col = "blue", lwd = 2)
  }
  
  if (graph_at_once) {
    pairs(df,
          lower.panel = knn_boundary,     
          upper.panel = NULL,
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
      knn_boundary(x1, x2)
    }
  }
}

draw_boundary(3, mlb1, 'Weight', 190, 210)