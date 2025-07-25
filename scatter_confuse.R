scatter_confuse = function(scat_dat, reg_stand, dec_crit) {
  
  if (!is.numeric(dec_crit) || !is.numeric(reg_stand)) {
    
    showModal(modalDialog("The decision criterion and regulatory standard must be numeric",easyClose = TRUE,
                          footer = tagList(modalButton("Close"))
    ))
    
    return(NULL)
    
  } else {
    
    x_name = colnames(scat_dat)[2]
    y_name = colnames(scat_dat)[3]
    id_name = colnames(scat_dat)[1]
    
    lin_fit = lm(scat_dat[[y_name]] ~ scat_dat[[x_name]])
    
    a = as.numeric(round(coef(lin_fit)[[1]], 3))
    b = as.numeric(round(coef(lin_fit)[[2]], 3))
    r2 = round(summary(lin_fit)$r.squared, digits = 3)
    n = nrow(scat_dat)
    
    lab = paste("y = ", a, " + ", b, "x, ", "R^2 = ", r2, ", n = ", n)
    
    fig = ggplot(scat_dat, aes(x = !!sym(x_name), y = !!sym(y_name))) +
      geom_point(aes(text = paste("<b>ID:</b> ", !!sym(id_name), "<br><b>", y_name, ":</b> ", !!sym(y_name),
                                  "<br><b>", x_name, ":</b> ", !!sym(x_name))), size = 3, shape = 21, color = "black", fill = "cadetblue") +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      geom_vline(xintercept = reg_stand, color = "blue") + 
      geom_hline(yintercept = dec_crit, linetype = "dashed", color = "darkgreen") +
      annotate("text", label = lab, x = 0.85 * mean(scat_dat[[x_name]]), y = 0.999 * max(scat_dat[[y_name]])) +
      labs(x = x_name, y = y_name) +
      theme_bw() +
      theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1))
    
    figure = ggplotly(fig, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "#eeeeee", font = list(size = 12, color = "black")))
    
    return(figure)
  }
}