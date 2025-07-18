draw_boundaries <- function(df, target_col, target_val, graph_at_once = TRUE) {
  numeric_cols <- sapply(df, is.numeric)
  df <- df[, numeric_cols]
  
  draw_boundary <- function(x, y, ...) {
    points(x, y, pch = 19, col = rgb(0, 0, 0, alpha = 0.7))
    
    modC <- lm(df[[target_col]] ~ x + y, data = df)
    coefC <- coef(modC)
    
    if (length(coefC) >= 3 && !any(is.na(coefC))) {
      Biso <- (target_val - coefC[1] - coefC[2] * x) / coefC[3]
      
      lines(x, Biso, col="blue", lwd = 2)
    }
  }
  
  if (graph_at_once) {
    pairs(df,
          lower.panel = draw_boundary,     
          upper.panel = panel.smooth, 
          pch = 19,)
  } else {
    all_pairs <- combn(names(df), 2, simplify=FALSE)
    for (pair in all_pairs) {
      x <- df[[pair[1]]]
      y <- df[[pair[2]]]
      
      plot.new()
      plot.window(xlim = range(x, na.rm = TRUE), ylim = range(y, na.rm = TRUE))
      axis(1)
      axis(2)
      title(main = paste(pair[1], "vs", pair[2]), 
            xlab = pair[1], 
            ylab = pair[2])
      draw_boundary(x, y)
    }
  }
}

