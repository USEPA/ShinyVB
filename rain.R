raincloud = function(data,date_format_string) {
  
  data = data.frame(data)
  
  print(data)
  data[,2] = as.numeric(data[,2])
  
  name = colnames(data)[2]
  
  add_sample <- function(x) {
    return(c(y = max(x) + .025, 
             label = length(x)))
  }
  
  stat1 = mean(data[,2])
  stat2 = median(data[,2])
  stat3 = length(data[,2])
  stat4 = min(data[,2])
  stat5 = max(data[,2])-min(data[,2])
  
  if (date_format_string == "Character") {
    data[,1]=as_factor(as.character(data[,1]))
  }

  rain = plot(ggplot(data, aes(1,data[,2])) +
         
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
           outlier.shape = NA) +
         
         geom_point(
           color = "navy",
           size = 1,
           alpha = .3,
           position = position_jitter(seed = 1, width = .12)) +
         
         annotation_custom(grid::textGrob(paste("Mean = ",round(stat1,1)),just="left",gp = gpar(fontsize=24),x=unit(0.83,"npc"), y=unit(0.95,"npc"))) +
         annotation_custom(grid::textGrob(paste("Median = ",round(stat2,1)),just="left",gp = gpar(fontsize=24),x=unit(0.83,"npc"), y=unit(0.91,"npc"))) +
         annotation_custom(grid::textGrob(paste("n = ", stat3),just="left",gp = gpar(fontsize=24),x=unit(0.83,"npc"), y=unit(0.87,"npc"))) +
         
         labs(x = NULL,y = name) +
         
         theme_bw() +
         theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
         theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
         theme(panel.grid.minor.x = element_line(linewidth = 0.25), panel.grid.major.x = element_line(linewidth = 0.25)) +
         theme(axis.text.x=element_text(size=16, face="bold"),axis.title.x=element_text(size=20,face="bold")) +
         
         coord_flip())
  
  return(rain)
}