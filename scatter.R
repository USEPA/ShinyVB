scatter = function(scatter_data,scatterx,scattery,id) {
  
  scatter_data = data.frame(scatter_data)

  fig = ggplot(scatter_data, aes(x=scatter_data[,scatterx], y=scatter_data[,scattery],
                                 text=paste("<br><b>ID:</b> ",scatter_data[,id],"<br><b>",scattery,":</b> ",scatter_data[,scattery],
                                      "<br><b>",scatterx,":</b> ",scatter_data[,scatterx],sep=""))) +
    geom_point(size=3, shape=21, color="black", fill="cadetblue") +
    geom_smooth(aes(group=1)) +
    labs(x = paste0(scatterx), y = paste0(scattery)) +
    theme_bw() +
    theme(axis.text=element_text(size=14,face="bold"),
          axis.title=element_text(size=20,face="bold"))
  
  figure = ggplotly(fig, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "#eeeeee", font = list(size = 12, color = "black")))
}