# Line plot with adaptive x-axis labeling
lineplot <- function(line_data, feature_name) {
  df <- data.frame(line_data, check.names = FALSE)
  df <- stats::na.omit(df)
  if (!nrow(df)) return(NULL)
  
  # Ensure y is numeric
  df[[2]] <- suppressWarnings(as.numeric(df[[2]]))
  
  # Compute y-axis range with a small padding
  y <- df[[2]]
  y_range <- range(y, na.rm = TRUE)
  span <- diff(y_range)
  pad <- if (span == 0) 0.05 * max(1, abs(y_range[1])) else 0.02 * span
  y_min <- y_range[1] - pad
  y_max <- y_range[2] + pad
  
  # X index and labels (IDs as character)
  n <- nrow(df)
  x_idx <- seq_len(n)
  id_char <- as.character(df[[1]])
  
  # Choose a labeling stride so we don't exceed target_xticks labels.
  # Use "nice" steps (1,2,5,10,20,25,50,100,...) near the raw stride.
  raw_step <- max(1, ceiling(n/30))
  nice_steps <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500)
  step <- nice_steps[which.min(abs(nice_steps - raw_step))]
  
  tick_idx <- unique(c(1, seq(1, n, by = step), n))
  tick_vals <- x_idx[tick_idx]
  tick_text <- id_char[tick_idx]
  
  # Hover text
  hover_text <- paste0(
    "<b>ID: </b>", id_char,
    "<br><b>", feature_name, ":</b> ", df[[2]]
  )
  
  fig <- plotly::plot_ly(
    data = df,
    x    = ~x_idx,
    y    = ~df[[2]],
    type = "scatter",
    mode = "lines+markers",
    text = hover_text,
    hoveron   = "points",
    hoverinfo = "text",
    fill      = "tozeroy",
    fillcolor = "cadetblue",
    marker    = list(color = "black", size = 4),
    line      = list(color = "darkgrey", width = 1)
  ) %>% plotly::layout(
    xaxis = list(
      title     = list(text = "ID", font = list(size = 20)),
      tickmode  = "array",
      tickvals  = tick_vals,
      ticktext  = tick_text,
      automargin = TRUE,
      tickangle = if (length(tick_idx) > 30) -45 else 0
    ),
    yaxis = list(
      title = list(text = feature_name, font = list(size = 20)),
      range = c(y_min, y_max)
    )
  )
  
  fig
}