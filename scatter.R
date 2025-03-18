scatter = function(scatter_data,scatterx,scattery) {
  
  scatter_data = as.data.frame(scatter_data)

  fig = ggplot(scatter_data, aes(x=scatter_data[,2], y=scatter_data[,3])) +
    geom_point(size=3, shape=21, color="black", fill="cadetblue") +
    geom_smooth() +
    labs(x = paste0(scatterx), y = paste0(scattery))+
    theme_bw() +
    theme(axis.text=element_text(size=14,face="bold"),
          axis.title=element_text(size=20,face="bold"))
  
  return(fig)
}

#ggsave(file="scatter.png",width = 11, height = 8, units = "in", dpi=300)