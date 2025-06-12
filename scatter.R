scatter = function(scatter_data) {
  
  x_name = colnames(scatter_data)[2]
  y_name = colnames(scatter_data)[3]
  id_name = colnames(scatter_data)[1]
  
  # Define the base plot
  fig = ggplot(scatter_data, aes(x = !!sym(x_name), y = !!sym(y_name))) +
    geom_point(aes(text = paste("<b>ID:</b> ", !!sym(id_name), 
                                "<br><b>", y_name, ":</b> ", !!sym(y_name),
                                "<br><b>", x_name, ":</b> ", !!sym(x_name))),
               size = 3, shape = 21, color = "black", fill = "cadetblue") +
    geom_smooth(method = "loess", se = TRUE) +
    labs(x = x_name, y = y_name) +
    theme_bw() +
    theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1))
  
  # Convert to plotly with tooltips
  figure = ggplotly(fig, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "#eeeeee", font = list(size = 12, color = "black")))
  
  return(figure)
}