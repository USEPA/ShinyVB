raincloud = function(rain_data) {

  rain_data = as.data.frame(rain_data)
  
  add_sample <- function(x) {
    return(c(y = max(x) + .025, 
             label = length(x)))
  }

  rain = ggplot(rain_data, aes(1,V2)) +
    
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
      family = "Roboto Mono",
      fontface = "bold",
      size = 5,
      vjust = -11.5
    ) +
    
    stat_summary(
      geom = "text",
      fun = "median",
      color = "black",
      aes(label=paste0(round(after_stat(y), 1))),
      family = "Roboto Mono",
      fontface = "bold",
      size = 5,
      vjust = -6,
      hjust = -0.1
    ) +
      
    stat_summary(
      geom = "text",
      fun.data = add_sample,
      color="black",
      aes(label=paste("n =", after_stat(label))),
      family = "Roboto Condensed",
      fontface = "bold",
      size = 4,
      hjust = 3,
      vjust = -9,
    ) +
      
    labs(x = NULL,y = "Values") +
    
    theme_bw() +
    
    theme(axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=20,face="bold")) +
      
    coord_flip()
  
  return(rain)
}

#ggsave(file="raincloud_F.png", FPlus, width = 11, height = 8, units = "in", dpi=300)