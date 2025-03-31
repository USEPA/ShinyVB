lineplot = function(line_data,lineplot) {
  
  data = data.frame(line_data)
  
  data=na.omit(data)
  
  figure = plot_ly(data=data, x = ~data[,1], y = ~data[,2], type = 'scatter', text = ~paste(
    "<b>ID: </b>",data[,1],
    "<br><b>",lineplot,":</b> ",data[,2],
    sep=""), hoveron = 'points',hoverinfo='text',
    mode = 'none', fill = 'tozeroy', fillcolor = "cadetblue") %>%
   layout(xaxis = list(title = list(text='ID',font=list(size=22))),yaxis = list(title = list(text=lineplot,font=list(size=22)),
                                  range=c(min(0.999*min(data[,2]),1.001*min(data[,2])),max(0.999*max(data[,2]),1.001*max(data[,2])))))

}