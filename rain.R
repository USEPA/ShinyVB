raincloud = function(rain_data) {
  
  rain_data = data.frame(rain_data)
  
  name = colnames(rain_data)[2]
  
  add_sample <- function(x) {
    return(c(y = max(x) + .025, 
             label = length(x)))
  }

  rain = ggplot(rain_data, aes(1,rain_data[,2])) +
    
    ggdist::stat_halfeye(
      fill="cadetblue",
      adjust = 1, 
      width = .75, 
      .width = 0,
      justification = -0.7, 
      point_color = NA) +
      
    geom_boxplot(
      fill="navy",
      width = .25,
      position = position_nudge(x = 0.3),
      alpha = 0.5,
      outlier.shape = NA
    ) +
    
    geom_point(
      color = "navy",
      size = 2,
      alpha = .3,
      position = position_jitter(seed = 1, width = .12)
    ) +
    
    stat_summary(
      geom = "text",
      fun = "mean",
      color = "black",
      aes(label=paste0("bar(x) == ",round(after_stat(y), 1))),
      parse=T,
      fontface = "bold",
      size = 6,
      vjust = -23
    ) +
    
    stat_summary(
      geom = "text",
      fun = "median",
      color = "black",
      aes(label=paste0(round(after_stat(y), 1))),
      fontface = "bold",
      size = 6,
      vjust = -14,
      hjust = -0.1
    ) +
      
    stat_summary(
      geom = "text",
      fun.data = add_sample,
      color="black",
      aes(label=paste("n =", after_stat(label))),
      fontface = "bold",
      size = 6,
      hjust = 1,
      vjust = -57,
    ) +
      
    labs(x = NULL,y = colnames(rain_data)[2]) +
    
    theme_bw() +
    
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    
    theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.x = element_line(linewidth = 0.1), panel.grid.major.x = element_line(linewidth = 0.1)) +
    
    theme(axis.text.x=element_text(size=14, face="bold"),
          axis.title.x=element_text(size=20,face="bold")) +
      
    coord_flip()
  
  return(rain)
}

#ggsave(file="raincloud_F.png", FPlus, width = 11, height = 8, units = "in", dpi=300)