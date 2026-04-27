lineplot <- function(line_data, feature_name, date_format_string) {
  
  df <- data.frame(line_data, check.names = FALSE)
  df <- stats::na.omit(df)
  if (!nrow(df)) return(NULL)
  
  # Ensure y is numeric
  df[[2]] <- suppressWarnings(as.numeric(df[[2]]))
  
  y <- df[[2]]
  y_min <- min(0.99 * min(y, na.rm = TRUE), 1.01 * min(y, na.rm = TRUE))
  y_max <- max(0.99 * max(y, na.rm = TRUE), 1.01 * max(y, na.rm = TRUE))
  
  if (date_format_string %in% c("Date", "Date_MDY_HM")) {
    # Convert ID to POSIXct and sort by time
    dt <- id_to_posix(df[[1]])
    keep <- !is.na(dt)
    
    if (!any(keep)) {
      # Parsing failed for all rows: fall back to categorical axis
      id_cat <- factor(as.character(df[[1]]), levels = unique(as.character(df[[1]])))
      hover_text <- paste0("<b>ID: </b>", as.character(df[[1]]),
                           "<br><b>", feature_name, ":</b> ", df[[2]])
      return(
        plotly::plot_ly(
          data = df,
          x    = ~id_cat,
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
          xaxis = list(title = list(text = "ID", font = list(size = 20))),
          yaxis = list(title = list(text = feature_name, font = list(size = 20)),
                       range = c(y_min, y_max))
        )
      )
    }
    
    # Keep only parsed rows
    df <- df[keep, , drop = FALSE]
    dt <- dt[keep]
    # Recompute y-range after filtering
    y <- df[[2]]
    y_min <- min(0.99 * min(y, na.rm = TRUE), 1.01 * min(y, na.rm = TRUE))
    y_max <- max(0.99 * max(y, na.rm = TRUE), 1.01 * max(y, na.rm = TRUE))
    
    ord <- order(dt)
    df  <- df[ord, , drop = FALSE]
    dt  <- dt[ord]
    df$dt <- dt  # add real POSIXct column for plotly
    
    hover_text <- paste0(
      "<b>ID: </b>", format(df$dt, "%m/%d/%Y %H:%M"),
      "<br><b>", feature_name, ":</b> ", df[[2]]
    )
    
    fig <- plotly::plot_ly(
      data = df,
      x    = ~dt,
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
      xaxis = list(title = list(text = "ID", font = list(size = 20)),
                   type  = "date"),
      yaxis = list(title = list(text = feature_name, font = list(size = 20)),
                   range = c(y_min, y_max))
    )
    
  } else {
    # Character/categorical IDs
    id_cat <- factor(as.character(df[[1]]), levels = unique(as.character(df[[1]])))
    hover_text <- paste0("<b>ID: </b>", as.character(df[[1]]),
                         "<br><b>", feature_name, ":</b> ", df[[2]])
    
    fig <- plotly::plot_ly(
      data = df,
      x    = ~id_cat,
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
      xaxis = list(title = list(text = "ID", font = list(size = 20))),
      yaxis = list(title = list(text = feature_name, font = list(size = 20)),
                   range = c(y_min, y_max))
    )
  }
  
  fig
}