lineplot = function(current_data,lineplot,id_var) {
  
  line_data = current_data
  
  print(line_data)
  
  line = line_data %>%
    ggplot(aes(x=line_data[,id_var], y=line_data[,lineplot])) +
    geom_ribbon(aes(ymin = min(line_data[,lineplot]), ymax = line_data[,lineplot],fill = "red"),alpha=0.9, show.legend = FALSE) +
    geom_line(aes(y = line_data[,lineplot]), color="darkgrey") +
    ylab(lineplot) +
    xlab("ID") +
    theme_bw()+
    theme(axis.text=element_text(size=14, face="bold"),
        axis.title=element_text(size=20,face="bold")) +
    scale_fill_manual(values=("red"="cadetblue")) +
    theme(legend.key = element_blank())
  
return(line)
}