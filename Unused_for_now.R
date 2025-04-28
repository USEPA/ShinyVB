# showModal(modalDialog(
#   paste0("The second column has been designated as the response variable by default. 
#          To change this, click on the column name at the BOTTOM of the table."),
#   easyClose = F,
#   footer = div(modalButton('Close'))
#   ))

# observeEvent(input$id, ignoreInit = T, {
#   
#   id_num = which(col_names()==input$id)
#   id_var(id_num)
#   renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
#     
#   })

observeEvent(input$save_rainn, ignoreInit = T, {
  
  if (is.null(ignored_rows)) {
    rain_data = current_data()
  } else {
    rain_data = current_data()[-ignored_rows,]
  }
  
  rain_data1 = data.frame(cbind(rain_data[,id_var],rain_data[,input$rainplot]))
  colnames(rain_data1) = c("ID",input$rainplot)
  
  add_sample = function(x) {
    return(c(y = max(x) + .025,
             label = length(x)))
  }
  
  stat1 = mean(rain_data1[,2])
  stat2 = median(rain_data1[,2])
  stat3 = length(rain_data1[,2])
  stat4 = min(rain_data1[,2])
  stat5 = max(rain_data1[,2])-min(rain_data1[,2])
  
  output$save_rain = downloadHandler(
    filename= "Rainplot.png",
    content = function(file) {
      
      on.exit(removeModal())
      png(file, width=input$rain_width, height=input$rain_height, units="in", res=input$rain_rez)
      plot(ggplot(rain_data1, aes(id_var,rain_data1[,2])) +
             
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
             
             annotation_custom(grid::textGrob(paste("Mean = ",round(stat1,1)),just="left",gp = gpar(fontsize=6),x=unit(0.88,"npc"), y=unit(0.91,"npc"))) +
             annotation_custom(grid::textGrob(paste("Median = ",round(stat2,1)),just="left",gp = gpar(fontsize=6),x=unit(0.88,"npc"), y=unit(0.87,"npc"))) +
             annotation_custom(grid::textGrob(paste("n = ", stat3),just="left",gp = gpar(fontsize=6),x=unit(0.88,"npc"), y=unit(0.95,"npc"))) +
             
             labs(x = NULL,y = input$rainplot) +
             
             theme_bw() +
             
             ggtitle(input$rain_title) +
             theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold")) +
             theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
             theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
             theme(panel.grid.minor.x = element_line(linewidth = 0.1), panel.grid.major.x = element_line(linewidth = 0.1)) +
             theme(axis.text.x=element_text(size=8, face="bold"),
                   axis.title.x=element_text(size=10,face="bold")) +
             
             coord_flip())
      
      dev.off()
    })
  
  showModal(modalDialog(title="Save Options", card(
    fluidRow(
      column(4,numericInput("rain_width", "Image Width (in)", value=6, min=2, max = 12, step = 0.5)),
      column(4,numericInput("rain_height", "Image Height (in)", value=3, min=2, max = 12, step = 0.5)),
      column(4,numericInput("rain_rez", "Image Resolution", value=400, min=100, max = 1200, step = 50))),
    fluidRow(
      column(12,textInput("rain_title", "Plot Title",value="My Raincloud Plot")))),
    footer = div(downloadButton("save_rain", "Save Image"),modalButton('Close'))))
})


observeEvent(input$save_linee, ignoreInit = T, {
  
  if (is.null(ignored_rows)) {
    temp_data = current_data()
  } else {
    temp_data = current_data()[-ignored_rows,]
  }
  
  output$save_line = downloadHandler(
    filename= "Lineplot.png",
    content = function(file) {
      on.exit(removeModal())
      
      png(file, width=input$line_width, height=input$line_height, units="in", res=input$line_rez)
      
      plot(ggplot(data=temp_data, aes(x=temp_data[,id_var], y=temp_data[,input$lineplot])) +
             geom_ribbon(aes(ymin = 0, ymax = temp_data[,input$lineplot] ,group=1), fill="cadetblue", show.legend = FALSE) +
             geom_line(aes(y = temp_data[,input$lineplot]),size=0.1, color="darkgrey") +
             ylab(input$lineplot) +
             xlab("ID") +
             coord_cartesian(ylim = (c(min(0.99*min(temp_data[,input$lineplot]),1.01*min(temp_data[,input$lineplot])),
                                       max(0.99*max(temp_data[,input$lineplot]),1.01*max(temp_data[,input$lineplot]))))) +
             theme_bw() +
             ggtitle(input$line_title) +
             theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
             theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
             theme(axis.text=element_text(size=6, face="bold"),axis.title=element_text(size=8,face="bold")) +
             theme(legend.key = element_blank()))
      
      dev.off()
    })
  
  showModal(modalDialog(title="Save Options", card(
    fluidRow(
      column(4,numericInput("line_width", "Image Width (in)", value=6, min=2, max = 12, step = 0.5)),
      column(4,numericInput("line_height", "Image Height (in)", value=3, min=2, max = 12, step = 0.5)),
      column(4,numericInput("line_rez", "Image Resolution", value=400, min=100, max = 1200, step = 50))),
    fluidRow(
      column(12,textInput("line_title", "Plot Title",value="My Lineplot")))),
    footer = div(downloadButton("save_line", "Save Image"),modalButton('Close'))))
})

observeEvent(input$save_scatt, ignoreInit = T, {
  
  if (is.null(ignored_rows)) {
    temp_data = current_data()
  } else {
    temp_data = current_data()[-ignored_rows,]
  }
  
  output$save_scat = downloadHandler(
    filename= "Scatterplot.png",
    content = function(file) {
      on.exit(removeModal())
      png(file, width=input$scat_width, height=input$scat_height, units="in", res=input$scat_rez)
      
      plot(ggplot(temp_data, aes(x=temp_data[,input$scatterx], y=temp_data[,input$scattery])) +
             geom_point(size=1, shape=21, color="black", fill="cadetblue", aes(group=1)) +
             geom_smooth(aes(group=1)) +
             labs(x = paste0(input$scatterx), y = paste0(input$scattery)) +
             theme_bw() +
             ggtitle(input$scat_title) +
             theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
             theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
             theme(axis.text=element_text(size=10,face="bold"),axis.title=element_text(size=12,face="bold")))
      
      dev.off()
    })
  
  showModal(modalDialog(title="Save Options", card(
    fluidRow(
      column(4,numericInput("scat_width", "Image Width (in)", value=5, min=2, max = 12, step = 0.5)),
      column(4,numericInput("scat_height", "Image Height (in)", value=5, min=2, max = 12, step = 0.5)),
      column(4,numericInput("scat_rez", "Image Resolution", value=500, min=100, max = 1200, step = 50)))),
    fluidRow(
      column(12,textInput("scat_title", "Plot Title",value="My Scatterplot"))),
    footer = div(downloadButton("save_scat", "Save Image"),modalButton('Close'))))
})

# observeEvent(input$stop_xgb_HP_and_errors, {
#   print("Stopping calculation...")
#   stopMulticoreFuture(xgb_perform_calculation)
# })

# observeEvent(input$xgb_hyper_rows_selected, ignoreInit = T, {
#   
#   matrix = xgb_hyper_result()
#   
#   eta_set(matrix[input$xgb_hyper_rows_selected,"eta"])
#   gamma_set(matrix[input$xgb_hyper_rows_selected,"gamma"])
#   max_depth_set(matrix[input$xgb_hyper_rows_selected,"max_depth"])
#   min_child_weight_set(matrix[input$xgb_hyper_rows_selected,"min_child_weight"])
#   nrounds_set(matrix[input$xgb_hyper_rows_selected,"nrounds"])
#   early_stop_set(matrix[input$xgb_hyper_rows_selected,"early_stopping_rounds"])
#   nfold_set(matrix[input$xgb_hyper_rows_selected,"nfold"])
#   subsamp_set(matrix[input$xgb_hyper_rows_selected,"subsample"])
#   colsamp_set(matrix[input$xgb_hyper_rows_selected,"colsample_bytree"])
#   
# })

observeEvent(input$save_corrr, ignoreInit = T, {
  
  if (is.null(ignored_rows)) {
    corr_data = current_data()
  } else {
    corr_data = current_data()[-ignored_rows,]
  }
  
  cov_data = corr_data[,-c(id_var,response_var())]
  
  data_corrs = cor(cov_data,use="pairwise.complete.obs")
  
  output$save_corr = downloadHandler(filename= "Correlations.png",content = function(file) {
    on.exit(removeModal())
    png(file, width=input$corr_width, height=input$corr_height, units="in", res=input$corr_rez)
    
    corrplot(data_corrs, addCoef.col = 'black', method="circle", cl.pos = 'n', is.corr = FALSE, mar=c(0,0,2,0),type="lower",
             col.lim = c(-1.4, 1.4), col = COL2('PRGn'), tl.col="black", tl.srt= 45, title=input$corr_title, cex.main = 2,)
    dev.off()
  })
  
  showModal(modalDialog(title="Save Options", card(
    fluidRow(
      column(4,numericInput("corr_width", "Image Width (in)", value=12, min=8, max = 16, step = 1)),
      column(4,numericInput("corr_height", "Image Height (in)", value=12, min=8, max = 16, step = 1)),
      column(4,numericInput("corr_rez", "Image Resolution", value=300, min=100, max = 500, step = 50))),
    fluidRow(
      column(12,textInput("corr_title", "Plot Title",value="My Correlations")))),
    footer = div(downloadButton("save_corr", "Save Image"),modalButton('Close'))))
})


