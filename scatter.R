scatter = function(scatter_data) {
  
  x_name = colnames(scatter_data)[[2]]
  y_name = colnames(scatter_data)[[3]]

  fig = ggplot(scatter_data, aes(x=scatter_data[,2], y=scatter_data[,3],
                                 text=paste("<b>ID:</b> ",scatter_data[,1],"<br><b>",y_name,":</b> ",scatter_data[,3],
                                      "<br><b>",x_name,":</b> ",scatter_data[,2],sep=""))) +
    geom_point(size=3, shape=21, color="black", fill="cadetblue", aes(group=1)) +
    geom_smooth(aes(group=1)) +
    labs(x =x_name, y = y_name) +
    theme_bw() +
    theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1))
  
  figure = ggplotly(fig, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "#eeeeee", font = list(size = 12, color = "black")))
}