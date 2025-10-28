library(dplyr)
library(qeML)

scale_features <- function(df, target_col) {
  cols_to_scale <- names(df) != target_col
  df[cols_to_scale] <- lapply(df[cols_to_scale], function(col) {
    as.numeric(scale(col))
  })
  df
}

draw_boundary <- function(df, target_col, target_val, threshold=0.5,
                          include_factors=FALSE, graph_at_once=TRUE) {
  if (include_factors) {
    df <- factorsToDummies(df, omitLast=TRUE, dfOut=TRUE)
  } else {
    numeric_cols <- sapply(df, is.numeric)
    df <- df[, numeric_cols]
  }
  
  df <- scale_features(df, target_col)
  logit_boundary <- function(x1, x2, ...) {
    local_df <- data.frame(x1 = x1, x2 = x2, y = df[[target_col]])
    modC <- glm(y ~ x1 + x2 - 1, data = local_df, family = binomial())
    if (is.null(modC)) return()
    coefC <- coef(modC)
    if (length(coefC) < 2 || any(is.na(coefC))) return()
    b1 <- as.numeric(coefC[[1]])
    b2 <- as.numeric(coefC[[2]])
    abline(a = qlogis(threshold), b = -b1 / b2, col = "blue", lwd = 2)
  }
  if (graph_at_once) {
    pairs(df,
          lower.panel = logit_boundary,     
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
      logit_boundary(x1, x2)
    }
  }
}

draw_boundary(mtcars, 'vs', 200)
