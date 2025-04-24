scatter_confuse = function(scat_dat,reg_stand,dec_crit) {
  
  x_name = colnames(scat_dat)[[2]]
  y_name = colnames(scat_dat)[[3]]
  
  lin_fit = lm(scat_dat[,3] ~ scat_dat[,2])
  
  a = as.numeric(round(coef(lin_fit)[[1]],3))
  b = as.numeric(round(coef(lin_fit)[[2]],3))
  r2 = round(summary(lin_fit)$r.squared, digits = 3)
  n = nrow(scat_dat)
  
  lab = paste("y = ", a, " + ", b, "x, ", "R^2 = ", r2, ", n = ", n)

  fig = ggplot(scat_dat, aes(x=scat_dat[,2], y=scat_dat[,3],text=paste("<b>ID:</b> ",scat_dat[,1],"<br><b>",y_name,":</b> ",scat_dat[,3],"<br><b>",x_name,":</b> ",scat_dat[,2],sep=""))) +
    geom_point(size=3, shape=21, color="black", fill="cadetblue", aes(group=1)) +
    geom_smooth(method="lm", se=FALSE, color="black", aes(group=1)) +
    geom_vline(xintercept = reg_stand, color = "blue") + 
    geom_hline(yintercept = dec_crit, linetype = "dashed", color = "darkgreen") +
    geom_text(aes(label = lab), parse=T, x = 0.77*mean(scat_dat[,2]), y = 0.97*max(scat_dat[,3])) +
    labs(x =x_name, y = y_name) +
    theme_bw() +
    theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1))
  
  figure = ggplotly(fig, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "#eeeeee", font = list(size = 12, color = "black")))
}