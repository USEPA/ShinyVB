lineplot = function(line_data,lineplot,date_format_string) {
  
  data = data.frame(line_data)
  
  data=na.omit(data)
  
  if (date_format_string != "Character") {

  figure = plot_ly(data=data, x = ~data[,1], y = ~data[,2], type = 'scatter', text = ~paste("<b>ID: </b>",data[,1],
          "<br><b>",lineplot,":</b> ",data[,2],sep=""), hoveron = 'points',hoverinfo='text',mode = 'lines+markers', fill = 'tozeroy',
          fillcolor = "cadetblue", marker = list(color = "black",size = 4),line = list(color = 'darkgrey', width = 1)) %>%
      layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text=lineplot,font=list(size=20)),
          range=c(min(0.99*min(data[,2]),1.01*min(data[,2])),max(0.99*max(data[,2]),1.01*max(data[,2])))))
  
  } else {
    
    figure = plot_ly(data=data, x = ~as_factor(as.character(data[,1])), y = ~data[,2], type = 'scatter', text = ~paste("<b>ID: </b>",data[,1],
          "<br><b>",lineplot,":</b> ",data[,2],sep=""), hoveron = 'points',hoverinfo='text',mode = 'lines+markers', fill = 'tozeroy',
          fillcolor = "cadetblue", marker = list(color = "black",size = 4),line = list(color = 'darkgrey', width = 1)) %>%
      layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text=lineplot,font=list(size=20)),
          range=c(min(0.99*min(data[,2]),1.01*min(data[,2])),max(0.99*max(data[,2]),1.01*max(data[,2])))))
  }
}