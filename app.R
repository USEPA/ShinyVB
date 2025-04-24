setwd(getwd())

library(bsicons)
library(bslib)
library(bsplus)
library(caret)
library(cluster)
library(colorspace)
library(corrplot)
library(DBI)
library(devtools)
library(dplyr)
library(DT)
library(future)
library(ggdist)
library(gghalves)
library(ggplot2)
library(ggpmisc)
library(ggtext)
library(grid)
library(gridExtra)
library(hash)
library(Hmisc)
library(hrbrthemes)
library(htmltools)
library(ipc)
library(isotree)
library(leaflet)
library(lime)
library(lubridate)
library(magrittr)
library(Metrics)
library(Nmisc)
library(permimp)
library(pdp)
library(plotly)
library(plyr)
library(png)
library(promises)
library(pso)
library(ragg)
library(RColorBrewer)
library(RSQLite)
library(reshape2)
library(reactable)
library(readxl)
library(rsample)
library(SHAPforxgboost)
library(shiny)
library(shinybusy)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinythemes)
library(stats)
library(tidymodels)
library(tidyr)
library(tidyverse)
library(units)
library(xgboost)
plan(multicore)

source("renderdata.R")
source("rain.R")
source("lineplot.R")
source("ui.R")
source("map_click.R")
source("scatter.R")
source("scatter_confuse.R")
source("impute.R")
source("lars_coeff.R")
source("lars_perform.R")
source("xgb_pso.R")
source("xgb_feature_selection.R")
source("xgb_call_HP.R")
source("xgb_HP_and_errors.R")
source("xgb_final.R")
source("createAO.R")
source("confusion.R")

#all.functions = list.functions.in.file("app.R", alphabetic = TRUE)

# Define server logic --
server= function(input,output,session) {
  
  temp_db = dbConnect(RSQLite::SQLite(), ":memory:")
  
  bo = reactiveVal(0)
  current_data = reactiveVal()
  response_var = reactiveVal(2)
  #id_var = reactiveVal()
  id_var = 1
  rv = reactiveValues(points = data.frame())
  col_names = reactiveVal()
  cove_names = reactiveVal()
  coves_being_used = reactiveVal()
  progress_list = reactiveVal()
  
  xgb_tree_method_set = reactiveVal("hist")
  xgb_booster_set = reactiveVal("gbtree")
  dart_normalize_type_set = reactiveVal("tree")
  dart_sample_type_set = reactiveVal("uniform")
  
  rate_drop_set = reactiveVal(0.1)
  skip_drop_set = reactiveVal(0.5)
  eta_set = reactiveVal(0.05)
  gamma_set = reactiveVal(1)
  max_depth_set = reactiveVal(3)
  min_child_weight_set = reactiveVal(3)
  nrounds_set = reactiveVal(500)
  early_stop_set = reactiveVal(20)
  nfold_set = reactiveVal(5)
  subsamp_set = reactiveVal(0.8)
  colsamp_set = reactiveVal(0.8)
  
  # xgb_hyper_result = reactiveVal()
  # xgb_hyper_calculation = NULL
  
  xgb_select_result = reactiveVal()
  xgb_select_calculation = NULL
  
  running = reactiveVal(FALSE)
  
  init_data = data.frame()
  EN_scat_dat = data.frame()
  EN_pred_scat_dat = data.frame()
  xgb_scat_dat = data.frame()
  xgb_pred_scat_dat = data.frame()
  ignored_rows = NULL
  xgb_saved_predictions = data.frame()
  HP_matrix = data.frame()
  Optimal_HP = data.frame(max_depth = 3,
                          eta = 0.1,
                          subsample = 0.8,
                          colsample_bytree = 0.8,
                          min_child_weight = 3,
                          gamma = 0,
                          nrounds = 500)
  xgb_final_model = NULL
  EN_model = NULL
  date_format_string = "Other"
  init_feat_props = hash()
  feat_props = hash()
  
  observeEvent(input$corr_check, {
    
    if (is.null(ignored_rows)) {
      corr_data = current_data()
    } else {
      corr_data = current_data()[-ignored_rows,]
    }
    
    cov_data = corr_data[,-c(id_var,response_var())]
    
    data_corrs = cor(cov_data,use="pairwise.complete.obs")
    
    output$corrplot = renderPlot({corrplot(data_corrs, addCoef.col = 'black', method="circle", cl.pos = 'n',is.corr = FALSE,
                                           type="lower",col.lim = c(-1.4, 1.4),col = COL2('PRGn'), tl.col="black", tl.srt= 45)},height = 900, width = 900)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Correlations")
  })
  
  observeEvent(input$data_cell_edit, ignoreInit = T,ignoreNULL = T, {
    
    temp_data=current_data()
    
    info = input$data_cell_edit
    i = info$row
    j = info$col + 1
    
    temp_data = editData(temp_data, input$data_cell_edit, "data", rownames = FALSE)
    
    current_data(temp_data)
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
  })
  
  observeEvent(input$restore, {
    
    response_var(2)
    cove_names(NULL)
    col_names(colnames(init_data))
    current_data(init_data)
    ignored_rows <<- NULL
    feat_props <<- init_feat_props
    
    #updateSelectInput(session,"id",choices=c(col_names()))
    updateSelectInput(session,"rainplot",selected="-")
    updateSelectInput(session,"lineplot",selected="-")
    updateSelectInput(session,"speed",selected="-")
    updateSelectInput(session,"direct",selected="-")
    updateSelectInput(session,"select_choice",selected="Features")
    
    updateSelectInput(session,"col_props",choices=c("-",col_names()))
    updateSelectInput(session,"rainplot",choices=c("-",col_names()))
    updateSelectInput(session,"lineplot",choices=c("-",col_names()))
    updateSelectInput(session,"scatterx",selected="scatterx",choices=c("-",col_names()))
    updateSelectInput(session,"scattery",selected="scattery",choices=c("-",col_names()))
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Data Table")
    
  })
  
  
  
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
  
  output$bo_text = renderUI({
    HTML("To determine site orientation, click once anywhere on the shoreline, then again on another point on the shoreline. 
      A third click, <b>made in the water</b>, calculates/saves the site orientation. A fourth click clears the map, 
      whereby the process can be repeated.<br><br><i>Note: Any newly calculated orientation replaces the previous one.</i>")
  })
  
  output$beach_orient = renderText({bo()})
  
  observeEvent(input$file1, ignoreInit = T, {
    
    init_data <<- read.csv(input$file1$datapath,header = TRUE,sep = input$sep)
    
    if (any(duplicated(init_data[,1]))) {
      showModal(modalDialog(paste("The ID column (column 1) is required to have unique values. Please ensure this prior to data importation."),
                            easyClose = F,footer = div(modalButton('Close'))))
    } else if (any(is.na(init_data[,1]))) {
      
      showModal(modalDialog(paste("The ID column (column 1) has missing values. Please remedy this prior to data importation."),
                            easyClose = F,footer = div(modalButton('Close'))))
      
    } else {
      feat_props_temp = hash()
      
      for (i in 1:ncol(init_data)) {
        .set(feat_props_temp,keys=colnames(init_data)[i],values=c(prop1=2,prop2=NA,prop3=NA,prop4=NA))
      }
      
      init_feat_props <<- feat_props_temp
      feat_props <<- feat_props_temp
      ignored_rows <<- NULL
      
      if (input$IDasDate == "YMD") {
        init_data[,1] = ymd(init_data[,1])
        date_format_string <<- "toLocaleDateString"
      } else if (input$IDasDate == "MDY") {
        init_data[,1] = mdy(init_data[,1])
        date_format_string <<- "toLocaleDateString"
      } else if (input$IDasDate == "MDYHM") {
        init_data[,1] = parse_date_time(init_data[,1],c('%m/%d/%y %H:%M'),exact=TRUE)
        date_format_string <<- "toLocaleString"
      } else {
        date_format_string <<- "Other"
      }
      
      current_data(init_data)
      col_names(colnames(init_data))
      
      enable("restore")
      enable("col_props")
      enable("impute_check")
      enable("corr_check")
      enable("run_iso_forest")
      
      #updateSelectInput(session,"id",choices=c(col_names()))
      updateSelectInput(session,"col_props",choices=c("-",col_names()))
      updateSelectInput(session,"rainplot",choices=c("-",col_names()))
      updateSelectInput(session,"lineplot",choices=c("-",col_names()))
      updateSelectInput(session,"scatterx",choices=c("-",col_names()))
      updateSelectInput(session,"scattery",choices=c("-",col_names()))
      updateSelectInput(session,"speed",choices=c("-",col_names()))
      updateSelectInput(session,"direct",choices=c("-",col_names()))
      
      renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = "Data Table")
      
      # showModal(modalDialog(
      #   paste0("The second column has been designated as the response variable by default. 
      #          To change this, click on the column name at the BOTTOM of the table."),
      #   easyClose = F,
      #   footer = div(modalButton('Close'))
      #   ))
    }
  })
  
  # observeEvent(input$id, ignoreInit = T, {
  #   
  #   id_num = which(col_names()==input$id)
  #   id_var(id_num)
  #   renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
  #     
  #   })
  
  observeEvent(input$col_props, ignoreInit = T,  {
    
    if (input$col_props != "-") {
      
      showModal(modalDialog(title=paste0(input$col_props," Column Properties"),card(
        fluidRow(
          column(3,numericInput("sig_digies",  label="Signif Digits", value = values(feat_props,keys=input$col_props)[1], min=0,max=12,step=1)),
          column(3,numericInput("prop2",  label="Prop2", value = values(feat_props,keys=input$col_props)[2], min=0,max=1,step=0.05)),
          column(3,textInput("prop3",  label="Prop3", value = values(feat_props,keys=input$col_props)[3])),
          column(3,textInput("prop4",  label="Prop4", value = values(feat_props,keys=input$col_props)[4])))),
        footer = div(actionButton("props_close",'Close'))))
    }
  })
  
  observeEvent(input$props_close, ignoreInit = T, {
    removeModal()
    updateSelectInput(session,"col_props",selected="-")
  })
  
  observeEvent(input$sig_digies, ignoreInit = T, {
    
    .set(feat_props,keys=input$col_props,values=c(input$sig_digies,values(feat_props,keys=input$col_props)[2],
                                                  values(feat_props,keys=input$col_props)[3],values(feat_props,keys=input$col_props)[4]))
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
    
  })
  
  observeEvent(input$data_columns_selected, ignoreInit = T, {
    
    if ((input$data_columns_selected+1) != response_var()) {
      response_var(input$data_columns_selected + 1)
      renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
    }
  })
  
  observeEvent(input$rainplot, ignoreInit = T, {
    
    if (input$rainplot != "-") {
      
      if (is.null(ignored_rows)) {
        rain_data0 = current_data()
      } else {
        rain_data0 = current_data()[-ignored_rows,]
      }
      
      rain_data1 = cbind(rain_data0[,id_var],rain_data0[,input$rainplot])
      colnames(rain_data1) = c("ID",input$rainplot)
      
      output$rainplot = renderPlot({raincloud(rain_data1)})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = "Raincloud")
    }
  })
  
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
  
  observeEvent(input$lineplot, ignoreInit = T, {
    
    if (input$lineplot != "-") {
      
      if (is.null(ignored_rows)) {
        line_data0 = current_data()
      } else {
        line_data0 = current_data()[-ignored_rows,]
      }
      
      var_list = c(1,which(colnames(line_data0) == input$lineplot))
      line_data1 = line_data0[,var_list]
      
      output$lineplott = renderPlotly({lineplot(line_data1,input$lineplot)})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Line Plot')
    }
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
  
  observeEvent(input$scatterx, ignoreInit = T, {
    
    if (input$scatterx != "-" & input$scattery!= "-") {
      
      if (is.null(ignored_rows)) {
        scatter_data0 = current_data()
      } else {
        scatter_data0 = current_data()[-ignored_rows,]
      }
      
      var_list = c(1,which(colnames(scatter_data0) == input$scatterx),which(colnames(scatter_data0) == input$scattery))
      scatter_data1 = scatter_data0[,var_list]
      
      output$scatplot = renderPlotly(scatter(scatter_data1))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Scatterplot')
      
    }
  })
  
  observeEvent(input$scattery, ignoreInit = T, {
    
    if (input$scatterx != "-" & input$scattery!= "-") {
      
      if (is.null(ignored_rows)) {
        scatter_data0 = current_data()
      } else {
        scatter_data0 = current_data()[-ignored_rows,]
      }
      
      var_list = c(1,which(colnames(scatter_data0) == input$scatterx),which(colnames(scatter_data0) == input$scattery))
      scatter_data1 = scatter_data0[,var_list]
      
      output$scatplot = renderPlotly(scatter(scatter_data1))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Scatterplot')
    }
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
  
  observeEvent(input$continue_impute, {
    removeModal()
    current_data(imputing(current_data(),id_var,response_var(),ignored_rows,input$data_seed))
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
  })
  
  observeEvent(input$end_impute, {
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
    removeModal()
  })
  
  observeEvent(input$impute_check, {
    
    if (is.null(ignored_rows)) {
      temp_data = current_data()[,-c(id_var,response_var())]
    } else {
      temp_data = current_data()[-ignored_rows,-c(id_var,response_var())]
    }
    
    crit_n = floor(ncol(temp_data)*0.3)
    
    n_missing=c()
    
    for (d in 1:nrow(temp_data)) {
      
      missing_values = 0
      
      for (e in 1:ncol(temp_data)) {
        if (is.na(temp_data[d,e])) {
          missing_values = missing_values + 1
        }
      }
      
      n_missing = append(n_missing,missing_values)
      
    }
    
    max_missing = max(n_missing)
    bad_rows = which(n_missing > crit_n)
    
    listing = paste(bad_rows,collapse=",")
    
    if (max_missing > crit_n) {
      
      showModal(modalDialog(
        paste0("WARNING: Row numbers (", listing, "), have quite a few missing values. Imputation results for these rows 
               will be highly uncertain. Consider deleting these from the dataset or disabling them."),
        footer = tagList(actionButton("continue_impute", "Impute Anyways"),actionButton("end_impute", "Exit"))))
      
    } else {
      
      current_data(imputing(current_data(),id_var,response_var(),ignored_rows,input$data_seed))
      renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
    }
  })
  
  observeEvent(input$create, {
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Data Table')
    
    if (input$speed != "-" & input$direct != "-") {
      
      if (!(input$A_name %in% col_names()) & !(input$O_name %in% col_names())) {
        
        new_data = createAO(col_names(),input$speed,input$direct,input$A_name,input$O_name,current_data(),bo())
        
        for (i in (ncol(new_data)-1):ncol(new_data)) {
          .set(feat_props,keys=colnames(new_data)[i],values=c(prop1=2,prop2=NA,prop3=NA,prop4=NA))
        }
        
        current_data(new_data)
        col_names(colnames(current_data()))
        
        # updateSelectInput(session,"id",choices=c(col_names()))
        updateSelectInput(session,"col_props",choices=c("-",col_names()))
        updateSelectInput(session,"rainplot",choices=c("-",col_names()))
        updateSelectInput(session,"lineplot",choices=c("-",col_names()))
        updateSelectInput(session,"scatterx",selected=input$scatterx,choices=c("-",col_names()))
        updateSelectInput(session,"scattery",selected=input$scattery,choices=c("-",col_names()))
        
        renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
        
      } else {
        showModal(modalDialog(div("ERROR: BOTH new component columns must have different names 
                                  than any currently existing column names.",style="font-size:160%"),easyClose = T))
      }
      
    } else {
      showModal(modalDialog(div("ERROR: A speed and direction data column must be specified.",style="font-size:160%"),easyClose = T))
    }
  })
  
  observeEvent(input$shinyVB, {
    
    if (input$shinyVB == "Modeling") {
      
      temp_data = as.data.frame(current_data())
      
      cov_list = seq(1,ncol(temp_data))
      
      min_col_removed = min(id_var,response_var())
      max_col_removed = max(id_var,response_var())
      
      removed_cols = c(min_col_removed,max_col_removed)
      remaining_cols = cov_list[! cov_list %in% removed_cols]
      
      temp_data = temp_data[,remaining_cols]
      
      covar_names = c(colnames(temp_data))
      
      updateCheckboxGroupInput(session,"coves_to_use",choices=covar_names,selected=covar_names,inline=T)
      
      cove_names(covar_names)
      
    } else {
      return()
    }
  })
  
  observeEvent(input$run_xgb_select, {
    
    if(running())
      return(NULL)
    running(TRUE)
    
    eta = eta_set()
    gamma = gamma_set()
    max_depth = max_depth_set()
    min_child_weight = min_child_weight_set()
    nrounds = nrounds_set()
    early_stop = early_stop_set()
    subsamp = subsamp_set()
    colsamp = colsamp_set()
    
    if (is.null(ignored_rows)) {
      xgb_select_data = current_data()
    } else {
      xgb_select_data = current_data()[-ignored_rows,]
    }
    
    resvar = response_var()
    xgb_tree_method = xgb_tree_method_set()
    xgb_boost = xgb_booster_set()
    dart_normalize_type = dart_normalize_type_set()
    dart_sample_type = dart_sample_type_set()
    rate_drop = rate_drop_set()
    skip_drop = skip_drop_set()
    
    xgb_standardize = input$xgb_standardize
    coves_to_use = input$coves_to_use
    lc_lowval = input$lc_lowval
    lc_upval = input$lc_upval
    rc_lowval = input$rc_lowval
    rc_upval = input$rc_upval
    train_prop = input$train_prop
    MC_runs = input$MC_runs
    loggy = input$loggy
    randomize = input$randomize
    test_weight = input$test_weight
    seed = input$model_seed
    
    coves_being_used(coves_to_use)
    
    xgb_select_result(NULL)
    
    xgb_select_calculation <<- future({
      
      xgb_selection(xgb_select_data,seed,resvar,coves_to_use,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,
                    xgb_standardize,xgb_tree_method,xgb_boost,dart_normalize_type,dart_sample_type,rate_drop,skip_drop,eta,gamma,max_depth,
                    min_child_weight,subsamp,colsamp,nrounds,early_stop,test_weight,temp_db)
      
    }, seed=TRUE)
    
    prom = xgb_select_calculation %...>% xgb_select_result
    
    prom = catch(xgb_select_calculation,
                 function(e){
                   xgb_select_result(NULL)
                   showModal(modalDialog(paste0("XGB covariate filtering cancelled. No results generated."),footer = modalButton("Close")))
                 })
    
    prom = finally(prom, function(){
      running(FALSE)
    })
    
    output$xgb_select = DT::renderDataTable(server=T,{
      data = datatable(xgb_select_result(),rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                                                                     target = "row",mode="single"),editable=F,options = list(autoWidth=F,dom='t',paging = F,pageLength = 25,scrollX = F,
                                                                                                                             scrollY = TRUE,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                                                                                                          "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}"))) #%>%
      formatRound(data,columns=c(1,3:6), digits=c(0,5,5,5,5))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Feature Selection')
    
    #Return something other than the future so we don't block the UI
    NULL
  })
  
  observeEvent(input$xgb_select_cancel, {
    print("Stopping calculation...")
    stopMulticoreFuture(xgb_select_calculation)
  })
  
  observeEvent(input$xgb_select_rows_selected, ignoreInit = T, {
    
    all_covar = coves_being_used()
    
    temp_data = dbReadTable(temp_db, "xgb_selection_results")
    
    crit_val = as.numeric(input$xgb_select_rows_selected[1])
    
    if (crit_val > 1) {
      
      tossed_covar = temp_data[which(as.numeric(temp_data$Iteration) < crit_val),"Lowest.SHAP"]
      remaining = all_covar[-which(all_covar %in% tossed_covar)]
      
    } else {
      remaining = all_covar
    }
    
    updateCheckboxGroupInput(session,"coves_to_use",choices=cove_names(),selected=remaining,inline=T)
    
  })
  
  observeEvent(input$xgb_HP_and_errors, {
    
    showModal(modalDialog(title="HP Tuning and Prediction Errors", card(
      
      fluidRow(
        column(5,checkboxInput("HP_choice", "Use PSO-HP", TRUE)),
        column(7,selectInput("xgb_hyper_metric", "Evaluation Metric", choices = c("rmse","mae","mape","logloss"), selected = "rmse"))),
      fluidRow(
        column(6,numericInput("pso_max_iter", "Max Iterations", min=1, max=1000, value=25, step = 1)),
        column(6,numericInput("pso_swarm_size", "Swarm Size", min=1, max=200, value=10, step = 1)))),
      fluidRow(
        column(6,numericInput("member_exp", "Membership Weight", min=0.25, max=3, value=0.5, step = 0.25)),
        column(6,numericInput("ss_exp", "Sum of Squares Weight", min=0.25, max=3, value=1, step = 0.25))),
      footer = div(actionButton("run_xgb_HP_and_errors", "Run"),modalButton('Close'))#,actionButton("stop_xgb_HP_and_errors", "Cancel the Calculation"#))
    ))
  })
  
  observeEvent(input$xgb_pred_stand, {
    
    if (nrow(xgb_pred_scat_dat) != 0) {
      
      output$xgb_pred_scatplot = renderPlotly(scatter_confuse(xgb_pred_scat_dat,input$xgb_pred_stand,input$xgb_pred_dc))
      
      confuse_results = confuse(xgb_pred_scat_dat[,2:3],input$xgb_pred_stand,input$xgb_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$xgb_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                                  options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                  orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$xgb_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                        round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    }
  })
  
  observeEvent(input$xgb_pred_dc, {
    
    if (nrow(xgb_pred_scat_dat) != 0) {
      
      output$xgb_pred_scatplot = renderPlotly(scatter_confuse(xgb_pred_scat_dat,input$xgb_pred_stand,input$xgb_pred_dc))
      
      confuse_results = confuse(xgb_pred_scat_dat[,2:3],input$xgb_pred_stand,input$xgb_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$xgb_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                                  options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                  orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$xgb_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                        round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    }
  })
  
  observeEvent(input$run_xgb_HP_and_errors, {
    
    xgb_HP_results = xgb_call_HP(current_data(),response_var(),id_var,input$model_seed,ignored_rows,input$coves_to_use,input$lc_lowval,
                                 input$lc_upval,input$rc_lowval,input$rc_upval,input$train_prop,input$MC_runs,input$loggy,input$randomize,input$xgb_standardize,
                                 input$xgb_hyper_metric,input$pso_max_iter,input$pso_swarm_size,input$member_exp,input$ss_exp,input$HP_choice,input$eta,input$gamma,
                                 input$max_depth,input$min_child_weight,input$subsamp,input$colsamp,input$nrounds)
    
    best_centroid = xgb_HP_results[[2]]
    xgb_saved_predictions <<- xgb_HP_results[[1]]
    
    if (input$HP_choice) {
      Optimal_HP <<- data.frame(max_depth = best_centroid[1],eta = best_centroid[2],subsample = best_centroid[3],colsample_bytree = best_centroid[4],
                                min_child_weight = best_centroid[5],gamma = best_centroid[6],nrounds = best_centroid[7])
    }
    
    output$xgb_predictions = DT::renderDataTable(server = T, {data = datatable(xgb_saved_predictions,rownames = F,selection =
                                                                                 list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,options = list(autoWidth = F,
                                                                                                                                                                                            paging = TRUE,pageLength = 25,scrollX = TRUE,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                                                                                                                                                                                            initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))
    #{if (date_format_string != "Other") formatDate(data,1,date_format_string) else .}
    })
    
    xgb_pred_scat_dat <<- xgb_saved_predictions[,1:3]
    output$xgb_pred_scatplot = renderPlotly(scatter_confuse(xgb_pred_scat_dat,input$xgb_pred_stand,input$xgb_pred_dc))
    
    confuse_results = confuse(xgb_pred_scat_dat[,2:3],input$xgb_pred_stand,input$xgb_pred_dc)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$xgb_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                                options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$xgb_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    
    resid_data = xgb_pred_scat_dat[,c(1,3)] %>% mutate(Residuals = round(xgb_pred_scat_dat[,2]-xgb_pred_scat_dat[,3],3))
    
    output$xgb_pred_resid_scatplot = renderPlotly(scatter(resid_data))
    
    output$xgb_pred_lineplot = renderPlotly(plot_ly(xgb_pred_scat_dat, x = ~xgb_pred_scat_dat[,1], y = ~xgb_pred_scat_dat[,2], name="Observations", type="scatter", mode = "lines",
                                                    text = ~paste("<b>ID: </b>",xgb_pred_scat_dat[,1],"<br><b>Observed Value:</b> ",xgb_pred_scat_dat[,2],sep=""),
                                                    hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5)) %>%
                                              add_trace(y = ~xgb_pred_scat_dat[,3], name="Predictions", mode = 'lines',
                                                        text = ~paste("<b>ID: </b>",xgb_pred_scat_dat[,1],"<br><b>Predicted Value:</b> ",round(xgb_pred_scat_dat[,3],3),sep=""),
                                                        hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                                              layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Predictions",font=list(size=20)),
                                                                                                                           range=c(min(0.99*min(xgb_pred_scat_dat[,2],xgb_pred_scat_dat[,3]),1.01*min(xgb_pred_scat_dat[,2],xgb_pred_scat_dat[,3])),
                                                                                                                                   max(0.99*max(xgb_pred_scat_dat[,2],xgb_pred_scat_dat[,3]),1.01*max(xgb_pred_scat_dat[,2],xgb_pred_scat_dat[,3]))))))
    
    removeModal()
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: HP and Errors')
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
  
  observeEvent(input$xgb_stand, {
    
    if (nrow(xgb_scat_dat) != 0) {
      
      output$xgb_scatplot = renderPlotly(scatter_confuse(xgb_scat_dat,input$xgb_stand,input$xgb_dec_crit))
      
      xgb_confuse_results = confuse(xgb_scat_dat[,2:3],input$xgb_stand,input$xgb_dec_crit)
      xgb_confuse_table = matrix(0,nrow=1,ncol=4)
      
      xgb_confuse_table[1,1] = xgb_confuse_results$TP
      xgb_confuse_table[1,2] = xgb_confuse_results$TN
      xgb_confuse_table[1,3] = xgb_confuse_results$FP
      xgb_confuse_table[1,4] = xgb_confuse_results$FN
      
      colnames(xgb_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$xgb_confuse = DT::renderDataTable(server = T, {data = datatable(xgb_confuse_table,rownames = F,selection = 
                                                                               list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                             options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                             orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                            {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$xgb_confuse_text = renderText({paste0("Sensitivity = ",round(xgb_confuse_results$Sensitivity,3),"; Specificity = ",
                                                   round(xgb_confuse_results$Specificity,3),"; Accuracy = ",round(xgb_confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$xgb_dec_crit, {
    
    if (nrow(xgb_scat_dat) != 0) {
      
      output$xgb_scatplot = renderPlotly(scatter_confuse(xgb_scat_dat,input$xgb_stand,input$xgb_dec_crit))
      
      xgb_confuse_results = confuse(xgb_scat_dat[,2:3],input$xgb_stand,input$xgb_dec_crit)
      xgb_confuse_table = matrix(0,nrow=1,ncol=4)
      
      xgb_confuse_table[1,1] = xgb_confuse_results$TP
      xgb_confuse_table[1,2] = xgb_confuse_results$TN
      xgb_confuse_table[1,3] = xgb_confuse_results$FP
      xgb_confuse_table[1,4] = xgb_confuse_results$FN
      
      colnames(xgb_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$xgb_confuse = DT::renderDataTable(server = T, {data = datatable(xgb_confuse_table,rownames = F,selection = 
                                                                               list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                             options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                             orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                          {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$xgb_confuse_text = renderText({paste0("Sensitivity = ",round(xgb_confuse_results$Sensitivity,3),"; Specificity = ",
                                                   round(xgb_confuse_results$Specificity,3),"; Accuracy = ",round(xgb_confuse_results$Accuracy,3))})
    }
  })
  
  observeEvent(input$xgb_final_fitting, {
    
    if (is.null(ignored_rows)) {
      xgb_final_data = current_data()
    } else {
      xgb_final_data = current_data()[-ignored_rows,]
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    xgb_final_data = xgb_final_data[!is.na(xgb_final_data[,response_var()]), ]
    
    var_list = c(response_var(),which(colnames(xgb_final_data) %in% input$coves_to_use))
    xgb_final_data1 = xgb_final_data[,var_list]
    colnames(xgb_final_data1) = c("Response",input$coves_to_use)
    
    model = xgb_final(xgb_final_data1,input$model_seed,input$lc_lowval,input$lc_upval,
                      input$rc_lowval,input$rc_upval,input$loggy,input$randomize,input$xgb_standardize,xgb_tree_method_set(),xgb_booster_set(),
                      dart_normalize_type_set(),dart_sample_type_set(),rate_drop_set(),skip_drop_set(),Optimal_HP$eta,Optimal_HP$gamma,
                      Optimal_HP$max_depth,Optimal_HP$min_child_weight,Optimal_HP$subsample,Optimal_HP$colsample_bytree,Optimal_HP$nrounds)
    
    xgb_stepr = round((max(current_data()[,response_var()])-min(current_data()[,response_var()]))/40,2)
    
    updateNumericInput(session, "xgb_stand",
                       value = round(mean(current_data()[,response_var()]),2),
                       max = round(max(current_data()[,response_var()]),2),
                       min = round(min(current_data()[,response_var()]),2),
                       step = xgb_stepr
    )
    
    xgb_final_model <<- model[[1]]
    xgb_fits = model[[2]]
    xgb_shaps = data.frame(model[[3]])
    
    xgb_results = data.frame(cbind(xgb_final_data[,1],xgb_final_data1[,1],round(xgb_fits,3),xgb_final_data1[,-1]))
    colnames(xgb_results) = c(colnames(xgb_final_data)[[1]],colnames(xgb_final_data)[[2]],"Fitted_Values",colnames(xgb_final_data1[,-1]))
    
    xgb_stepd = round((max(xgb_results[,3])-min(xgb_results[,3]))/40,2)
    
    updateNumericInput(session, "xgb_dec_crit",
                       value = round(mean(xgb_results[,3]),2),
                       max = round(max(xgb_results[,3]),2),
                       min = round(min(xgb_results[,3]),2),
                       step = xgb_stepd
    )
    
    rsq_xgb = cor(xgb_results[,2], xgb_results[,3])^2
    
    output$xgb_shapes = DT::renderDataTable(server = T, {data = datatable(xgb_shaps,rownames = F,selection =
                                                                            list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,options =
                                                                            list(autoWidth = F, dom='t', paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                                                                              className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                         "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$xgb_fits = DT::renderDataTable(server = T, {data = datatable(xgb_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                                                                                                                  target = "row",mode = "single"),editable = F,options = list(autoWidth = F,paging = T,pageLength = 25,scrollX = T,scrollY = T,columnDefs = list(list(
                                                                                                                    className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                                                               "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    xgb_scat_dat <<- xgb_results[,1:3]
    
    output$xgb_scatplot = renderPlotly(scatter_confuse(xgb_scat_dat,input$xgb_stand,input$xgb_dec_crit))
    
    xgb_resid_data = xgb_results[,c(1,3)]
    xgb_resid_data = xgb_resid_data %>% mutate(Residuals = round(xgb_results[,2]-xgb_results[,3],3))
    output$xgb_resid_scatplot = renderPlotly(scatter(xgb_resid_data))
    
    output$xgb_lineplot = renderPlotly(plot_ly(xgb_scat_dat, x = ~xgb_scat_dat[,1], y = ~xgb_scat_dat[,2], name="Observations", type="scatter", mode = "lines",
                                               text = ~paste("<b>ID: </b>",xgb_scat_dat[,1],"<br><b>Observed Value:</b> ",xgb_scat_dat[,2],sep=""),hoveron = 'points',hoverinfo='text',
                                               line = list(color = "#2c3e50", width = 1.5)) %>%
                                         add_trace(y = ~xgb_scat_dat[,3], name="Fitted_Values", mode = 'lines',text = ~paste("<b>ID: </b>",xgb_scat_dat[,1],"<br><b>Fitted Value:</b> ",
                                                                                                                             round(xgb_scat_dat[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                                         layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Fitted Values",
                                                                                                                                   font=list(size=20)),range=c(min(0.99*min(xgb_scat_dat[,2],xgb_scat_dat[,3]),1.01*min(xgb_scat_dat[,2],xgb_scat_dat[,3])),max(0.99*max(xgb_scat_dat[,2],
                                                                                                                                                                                                                                                                         xgb_scat_dat[,3]),1.01*max(xgb_scat_dat[,2],xgb_scat_dat[,3]))))))
    
    xgb_confuse_results = confuse(xgb_scat_dat[,2:3],input$xgb_stand,input$xgb_dec_crit)
    xgb_confuse_table = matrix(0,nrow=1,ncol=4)
    
    xgb_confuse_table[1,1] = xgb_confuse_results$TP
    xgb_confuse_table[1,2] = xgb_confuse_results$TN
    xgb_confuse_table[1,3] = xgb_confuse_results$FP
    xgb_confuse_table[1,4] = xgb_confuse_results$FN
    
    colnames(xgb_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$xgb_confuse = DT::renderDataTable(server = T, {data = datatable(xgb_confuse_table,rownames = F,selection = 
                                                                             list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                           options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                           orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$xgb_confuse_text = renderText({paste0("Sensitivity = ",round(xgb_confuse_results$Sensitivity,3),"; Specificity = ",
                                                 round(xgb_confuse_results$Specificity,3),"; Accuracy = ",round(xgb_confuse_results$Accuracy,3))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Final Fitting')
  })
  
  observeEvent(input$EN_pred_stand, {
    
    if (nrow(EN_pred_scat_dat) != 0) {
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_pred_scat_dat,input$EN_pred_stand,input$EN_pred_dc))
      
      confuse_results = confuse(EN_pred_scat_dat[,2:3],input$EN_pred_stand,input$EN_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                   list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                                 options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                 orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                          {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                       round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$EN_pred_dc, {
    
    if (nrow(EN_pred_scat_dat) != 0) {
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_pred_scat_dat,input$EN_pred_stand,input$EN_pred_dc))
      
      confuse_results = confuse(EN_pred_scat_dat[,2:3],input$EN_pred_stand,input$EN_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                   list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                                 options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                 orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                          {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                       round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$elastic_pred, {
    
    set.seed(input$model_seed)
    
    EN_stepr = round((max(current_data()[,response_var()])-min(current_data()[,response_var()]))/40,2)
    
    updateNumericInput(session, "EN_pred_stand",
                       value = round(mean(current_data()[,response_var()]),2),
                       max = round(max(current_data()[,response_var()]),2),
                       min = round(min(current_data()[,response_var()]),2),
                       step = EN_stepr
    )
    
    if (is.null(ignored_rows)) {
      EN_data0 = current_data()
    } else {
      EN_data0 = current_data()[-ignored_rows,]
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    EN_data0 = EN_data0[!is.na(EN_data0[,response_var()]),]
    
    var_list = c(1,response_var(),which(colnames(EN_data0) %in% input$coves_to_use))
    EN_data = EN_data0[,var_list]
    colnames(EN_data) = c(colnames(current_data())[[1]],"Response",input$coves_to_use)
    
    # RANDOMIZE DATA
    if (input$randomize==TRUE) {
      random_index = sample(1:nrow(EN_data), nrow(EN_data))
      EN_data = EN_data[random_index, ]
    }
    
    data = EN_data[,-1]
    
    if (any(is.na(data[,-1]))) {
      
      showModal(modalDialog(paste("Elastic Net does not tolerate missing feature values. You can either Impute these
                (on the Data tab) or Disable rows/columns with missing values."),footer = modalButton("Close")))
      
    } else {
      
      #Create n folds
      tot_folds = 5
      folds = cut(seq(1, nrow(data)), breaks = tot_folds, labels = FALSE)
      
      fold_predictions = matrix(0, nrow = 0, ncol = 2)
      fold_predictions = as.data.frame(fold_predictions)
      
      coeff_folds = matrix(0, nrow = ncol(data), ncol = tot_folds+1)
      coeff_folds = as.data.frame(coeff_folds)
      coeff_folds[,1] = c("(Intercept)",input$coves_to_use)
      
      #Perform cross validation
      for (f in 1:tot_folds) {
        
        testIndices = which(folds == f, arr.ind = TRUE)
        testData = data[testIndices, ]
        trainData = data[-testIndices, ]
        
        temp_preds = matrix(0, nrow = nrow(testData), ncol = 2*input$MC_runs)
        temp_preds = data.frame(temp_preds)
        
        temp_coeffs = matrix(0, nrow = ncol(data), ncol = input$MC_runs+1)
        temp_coeffs = data.frame(temp_coeffs)
        temp_coeffs[,1] = c("(Intercept)",input$coves_to_use)
        
        withProgress(
          message = 'EN Estimation Progress',
          detail = paste("MC runs:", x = input$MC_runs,"; Fold:",y = f),
          value = (1-1/tot_folds) - (1/tot_folds)*(tot_folds-f),
          {
            
            for (i in 1:input$MC_runs) {
              
              incProgress(1/(input$MC_runs*tot_folds), detail = paste("MC run:",i,"/",input$MC_runs,"; Fold:",f,"/",tot_folds))
              
              # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS in test data and train data
              if (input$loggy == TRUE) {
                
                for (j in 1:nrow(trainData)) {
                  if (trainData[j, 1] == "TNTC") {
                    trainData[j, 1] = log10(runif(1, min = input$rc_lowval, max = input$rc_upval))
                  }
                  
                  if (trainData[j, 1] == "ND") {
                    trainData[j, 1] = log10(runif(1, min = input$lc_lowval, max = input$lc_upval))
                  }
                }
                
                for (j in 1:nrow(testData)) {
                  if (testData[j, 1] == "TNTC") {
                    testData[j, 1] = log10(runif(1, min = input$rc_lowval, max = input$rc_upval))
                  }
                  
                  if (testData[j, 1] == "ND") {
                    testData[j, 1] = log10(runif(1, min = input$lc_lowval, max = input$lc_upval))
                  }
                }
              } else {
                for (j in 1:nrow(trainData)) {
                  if (trainData[j, 1] == "TNTC") {
                    trainData[j, 1] = (runif(1, min = input$rc_lowval, max = input$rc_upval))
                  }
                  
                  if (trainData[j, 1] == "ND") {
                    trainData[j, 1] = (runif(1, min = input$lc_lowval, max = input$lc_upval))
                  }
                }
                for (j in 1:nrow(testData)) {
                  
                  if (testData[j, 1] == "TNTC") {
                    testData[j, 1] = (runif(1, min = input$rc_lowval, max = input$rc_upval))
                  }
                  
                  if (testData[j, 1] == "ND") {
                    testData[j, 1] = (runif(1, min = input$lc_lowval, max = input$lc_upval))
                  }
                }
              }
              
              temp_preds[,2*i-1] = testData[,1]
              
              train_control = trainControl(method = "repeatedcv",number = 5,repeats = 5,search = "random",verboseIter = F)
              
              # Train the model
              model = train(Response ~ .,data = trainData,method = "glmnet",preProcess = c("center", "scale"),tuneLength = 25,trControl = train_control)
              
              coeffs = as.matrix(coef(model$finalModel,model$finalModel$lambdaOpt))
              coeffs = as.data.frame(coeffs)
              coeffs = round(coeffs,3)
              temp_coeffs[,i+1] = coeffs
              
              preds = predict(model, testData[,-1])
              temp_preds[,2*i] = round(preds,3)
              
            } #End the MC Runs
            
            coeff_folds[,f+1] = rowMeans(temp_coeffs[,-1])
            
            even_columns = temp_preds[,seq(2, ncol(temp_preds), by = 2)]
            odd_columns = temp_preds[,seq(1, ncol(temp_preds), by = 2)]
            
            obs_mean_values = rowMeans(odd_columns)
            pred_mean_values = rowMeans(even_columns)
            fold_preds = cbind(obs_mean_values,pred_mean_values)
            
            fold_predictions = rbind(fold_predictions,fold_preds)
          })
      } #End the Fold runs
      
      prediction_results = data.frame(cbind(EN_data[,1],fold_predictions[,1],fold_predictions[,2],data[,-1]))
      colnames(prediction_results) = c(colnames(EN_data)[[1]],colnames(data)[[1]],"Predictions",colnames(data[,-1]))
      
      prediction_results = prediction_results[order(prediction_results[,1]),]
      
      final_coeffs = data.frame(cbind(coeff_folds[,1],rowMeans(coeff_folds[,-1])))
      colnames(final_coeffs) = c("Feature","Coefficient")
      
      EN_stepd = round((max(prediction_results[,3])-min(prediction_results[,3]))/40,2)
      
      updateNumericInput(session, "EN_pred_dc",
                         value = round(mean(prediction_results[,3]),2),
                         max = round(max(prediction_results[,3]),2),
                         min = round(min(prediction_results[,3]),2),
                         step = EN_stepd
      )
      
      output$EN_preds = DT::renderDataTable(server = T, {data = datatable(prediction_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                                                                                                                           target = "row",mode = "single"),editable = F,options = list(autoWidth = F,paging = T,pageLength = 25,scrollX = T,scrollY = T,columnDefs = list(list(
                                                                                                                             className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                                                                        "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_coeffs = DT::renderDataTable(server = T, {data = datatable(final_coeffs,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                                                                                                                           target = "row",mode = "single"),editable = F,options = list(autoWidth = F, dom='t', paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                                                                                                                             className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                                                                        "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      EN_pred_scat_dat <<- prediction_results[,1:3]
      output$EN_pred_scatplot = renderPlotly(scatter_confuse(EN_pred_scat_dat,input$EN_pred_stand,input$EN_pred_dc))
      
      resid_data = prediction_results[,c(1,3)]
      resid_data = resid_data %>% mutate(Residuals = round(prediction_results[,2]-prediction_results[,3],3))
      output$EN_pred_resid_scatter = renderPlotly(scatter(resid_data))
      
      output$EN_pred_lineplot = renderPlotly(plot_ly(EN_pred_scat_dat, x = ~EN_pred_scat_dat[,1], y = ~EN_pred_scat_dat[,2], name="Observations", type="scatter", mode = "lines",
                                                     text = ~paste("<b>ID: </b>",EN_pred_scat_dat[,1],"<br><b>Observed Value:</b> ",EN_pred_scat_dat[,2],sep=""),hoveron = 'points',hoverinfo='text',
                                                     line = list(color = "#2c3e50", width = 1.5)) %>%
                                               add_trace(y = ~EN_pred_scat_dat[,3], name="Predictions", mode = 'lines',text = ~paste("<b>ID: </b>",EN_pred_scat_dat[,1],"<br><b>Prediction:</b> ",
                                                                                                                                     round(EN_pred_scat_dat[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                                               layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Predictions Values",
                                                                                                                                         font=list(size=20)),range=c(min(0.99*min(EN_pred_scat_dat[,2],EN_pred_scat_dat[,3]),1.01*min(EN_pred_scat_dat[,2],EN_pred_scat_dat[,3])),
                                                                                                                                                                     max(0.99*max(EN_pred_scat_dat[,2],EN_pred_scat_dat[,3]),1.01*max(EN_pred_scat_dat[,2],EN_pred_scat_dat[,3]))))))
      
      confuse_results = confuse(EN_pred_scat_dat[,2:3],input$EN_pred_stand,input$EN_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                   list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                                 options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                 orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
              {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                       round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'Elastic Net: Prediction')
    }
  })
  
  observeEvent(input$EN_stand, {
    
    if (nrow(EN_scat_dat) != 0) {
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_scat_dat,input$EN_stand,input$EN_dec_crit))
      
      EN_confuse_results = confuse(EN_scat_dat[,2:3],input$EN_stand,input$EN_dec_crit)
      EN_confuse_table = matrix(0,nrow=1,ncol=4)
      
      EN_confuse_table[1,1] = EN_confuse_results$TP
      EN_confuse_table[1,2] = EN_confuse_results$TN
      EN_confuse_table[1,3] = EN_confuse_results$FP
      EN_confuse_table[1,4] = EN_confuse_results$FN
      
      colnames(EN_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_confuse = DT::renderDataTable(server = T, {data = datatable(EN_confuse_table,rownames = F,selection = 
                                                                              list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                            options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                            orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                          {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_confuse_text = renderText({paste0("Sensitivity = ",round(EN_confuse_results$Sensitivity,3),"; Specificity = ",
                                                  round(EN_confuse_results$Specificity,3),"; Accuracy = ",round(EN_confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$EN_dec_crit, {
    
    if (nrow(EN_scat_dat) != 0) {
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_scat_dat,input$EN_stand,input$EN_dec_crit))
      
      EN_confuse_results = confuse(EN_scat_dat[,2:3],input$EN_stand,input$EN_dec_crit)
      EN_confuse_table = matrix(0,nrow=1,ncol=4)
      
      EN_confuse_table[1,1] = EN_confuse_results$TP
      EN_confuse_table[1,2] = EN_confuse_results$TN
      EN_confuse_table[1,3] = EN_confuse_results$FP
      EN_confuse_table[1,4] = EN_confuse_results$FN
      
      colnames(EN_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_confuse = DT::renderDataTable(server = T, {data = datatable(EN_confuse_table,rownames = F,selection = 
                                                                              list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                            options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                            orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                          {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_confuse_text = renderText({paste0("Sensitivity = ",round(EN_confuse_results$Sensitivity,3),"; Specificity = ",
                                                  round(EN_confuse_results$Specificity,3),"; Accuracy = ",round(EN_confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$elastic_fit, {
    
    set.seed(input$model_seed)
    
    if (is.null(ignored_rows)) {
      EN_data0 = current_data()
    } else {
      EN_data0 = current_data()[-ignored_rows,]
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    EN_data0 = EN_data0[!is.na(EN_data0[,response_var()]),]
    
    var_list = c(response_var(),which(colnames(EN_data0) %in% input$coves_to_use))
    EN_data = EN_data0[,var_list]
    colnames(EN_data) = c("Response",input$coves_to_use)
    
    if (any(is.na(EN_data[,-1]))) {
      
      showModal(modalDialog(paste("Elastic Net does not tolerate missing feature values. You can either Impute these
                (on the Data tab) or Disable rows/columns with missing values."),footer = modalButton("Close")))
      
    } else {
      
      EN_stepr = round((max(current_data()[,response_var()])-min(current_data()[,response_var()]))/40,2)
      
      updateNumericInput(session, "EN_stand",
                         value = round(mean(current_data()[,response_var()]),2),
                         max = round(max(current_data()[,response_var()]),2),
                         min = round(min(current_data()[,response_var()]),2),
                         step = EN_stepr
      )
      
      train_control = trainControl(method = "repeatedcv",number = 5,repeats = 5,search = "random",verboseIter = F)
      
      # Train the model
      EN_model <<- train(Response ~ .,data = EN_data,method = "glmnet",preProcess = c("center", "scale"),tuneLength = 25,trControl = train_control)
      
      EN_HP = c(alpha=EN_model$bestTune[[1]],lambda=EN_model$bestTune[[2]])
      
      EN_coeffs = as.matrix(coef(EN_model$finalModel,EN_model$finalModel$lambdaOpt))
      EN_coeffs = as.data.frame(EN_coeffs)
      names = c("(Intercept)",input$coves_to_use)
      EN_coeffs = cbind(names,round(EN_coeffs,3))
      colnames(EN_coeffs) = c("Feature","Coefficient")
      
      EN_fits = predict(EN_model, EN_data[,-1])
      
      EN_results0 = data.frame(EN_data0[,1]) %>%
        mutate(Observed=EN_data[,1],Fitted_Values=round(EN_fits,3))
      
      EN_stepd = round((max(EN_results0$Fitted_Values)-min(EN_results0$Fitted_Values))/40,2)
      
      updateNumericInput(session, "EN_dec_crit",
                         value = round(mean(EN_results0$Fitted_Values),2),
                         max = round(max(EN_results0$Fitted_Values),2),
                         min = round(min(EN_results0$Fitted_Values),2),
                         step = EN_stepd
      )
      
      EN_results = cbind(EN_results0,EN_data[,-1])
      colnames(EN_results)[[1]] = colnames(EN_data0)[[1]]
      
      rsq_EN = cor(EN_results[,2], EN_results[,3])^2
      
      output$EN_fits = DT::renderDataTable(server = T, {data = datatable(EN_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                                                                                                                  target = "row",mode = "single"),editable = F,options = list(autoWidth = F,paging = T,pageLength = 25,scrollX = T,scrollY = T,columnDefs = list(list(
                                                                                                                    className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                                                               "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_coeffs = DT::renderDataTable(server = T, {data = datatable(EN_coeffs,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                                                                                                                   target = "row",mode = "single"),editable = F,options = list(autoWidth = F, dom='t', paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                                                                                                                     className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                                                                "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      EN_scat_dat <<- EN_results0[,1:3]
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_scat_dat,input$EN_stand,input$EN_dec_crit))
      
      resid_data = EN_results0[,c(1,3)]
      resid_data = resid_data %>% mutate(Residuals = round(EN_results0[,2]-EN_results0[,3],3))
      output$EN_resid_scatplot = renderPlotly(scatter(resid_data))
      
      output$EN_lineplot = renderPlotly(plot_ly(EN_scat_dat, x = ~EN_scat_dat[,1], y = ~EN_scat_dat[,2], name="Observations", type="scatter", mode = "lines",
                                                text = ~paste("<b>ID: </b>",EN_scat_dat[,1],"<br><b>Observed Value:</b> ",EN_scat_dat[,2],sep=""),hoveron = 'points',hoverinfo='text',
                                                line = list(color = "#2c3e50", width = 1.5)) %>%
                                          add_trace(y = ~EN_scat_dat[,3], name="Predictions", mode = 'lines',text = ~paste("<b>ID: </b>",EN_scat_dat[,1],"<br><b>Fitted Value:</b> ",
                                                                                                                           round(EN_scat_dat[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                                          layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Fitted Values",
                                                                                                                                    font=list(size=20)),range=c(min(0.99*min(EN_scat_dat[,2],EN_scat_dat[,3]),1.01*min(EN_scat_dat[,2],EN_scat_dat[,3])),max(0.99*max(EN_scat_dat[,2],
                                                                                                                                                                                                                                                                      EN_scat_dat[,3]),1.01*max(EN_scat_dat[,2],EN_scat_dat[,3]))))))
      
      EN_confuse_results = confuse(EN_scat_dat[,2:3],input$EN_stand,input$EN_dec_crit)
      EN_confuse_table = matrix(0,nrow=1,ncol=4)
      
      EN_confuse_table[1,1] = EN_confuse_results$TP
      EN_confuse_table[1,2] = EN_confuse_results$TN
      EN_confuse_table[1,3] = EN_confuse_results$FP
      EN_confuse_table[1,4] = EN_confuse_results$FN
      
      colnames(EN_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_confuse = DT::renderDataTable(server = T, {data = datatable(EN_confuse_table,rownames = F,selection = 
                                                                              list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,
                                                                            options = list(autoWidth = F,dom='t', paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                            orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
              {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_confuse_text = renderText({paste0("Sensitivity = ",round(EN_confuse_results$Sensitivity,3),"; Specificity = ",
                                                  round(EN_confuse_results$Specificity,3),"; Accuracy = ",round(EN_confuse_results$Accuracy,3))})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'Elastic Net: Fitting')
    }
  })
  
  output$map = renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(270, 40, zoom = 5)
  })
  
  observeEvent(input$map_click, {map_click(input$map_click,rv,bo)})
  
  observeEvent(input$xgb_params, {
    
    showModal(modalDialog(title="XGB Hyperparameters",easyClose=F,card(
      fluidRow(
        column(4,numericInput("eta", label="Eta", value = eta_set(),min=0,max=1,step=0.01)),
        column(4,numericInput("gamma", label="Gamma", value = gamma_set(), min=0, max=20, step = 1)),
        column(4,numericInput("max_depth", label="Max Tree Depth", value = max_depth_set(), min=1))),
      fluidRow(
        column(6,numericInput("min_child_weight", label="Min Child Weight", value = min_child_weight_set(), min=1)),
        column(6,numericInput("nfold", label="CV Folds", value = nfold_set(), min=2,max=20))),
      fluidRow(
        column(6,numericInput("nrounds", label="Iteration Rounds", value = nrounds_set(), min=100)),
        column(6,numericInput("early_stop", label="Early Stopping", value = early_stop_set(), min=10))),
      fluidRow(
        column(6,numericInput("subsamp", label="Subsample Proportion", value = subsamp_set(), min=0,max=1, step=0.01)),
        column(6,numericInput("colsamp", label="Column Sample Proportion", value = colsamp_set(), min=0,max=1,step=0.01))),
      fluidRow(
        column(4,selectInput("xgb_tree_method",label = "Tree Method",selected =xgb_tree_method_set(),choices = c("hist","exact","approx"))),
        column(4,selectInput("xgb_booster",label = "Booster",selected =xgb_booster_set(),choices = c("gbtree","gblinear","dart")))),
      fluidRow(column(12,tags$h5("CAUTION: DART booster + Feature Selection = long computational times!"))),
      fluidRow(
        column(6, selectInput("dart_normalize_type",label = "Normalization Type",selected =dart_normalize_type_set(),choices = c("tree","forest"))),
        column(6, selectInput("dart_sample_type",label = "Sample Algorithm",selected =dart_sample_type_set(),choices = c("uniform","weighted")))),
      # fluidRow(
      #   column(12, selectInput("objective",
      #          label = "Objective Fcn",
      #          selected ="reg:linear",
      #          choices = c("reg:linear","reg:logistic","binary:logistic","multi:softmax","multi:softprob")))),
      fluidRow(
        column(6,numericInput("rate_drop", label="Drop Rate", value = rate_drop_set(), min=0,max=1,step=0.01)),
        column(6,numericInput("skip_drop", label="Skip Prob", value = skip_drop_set(), min=0,max=1,step=0.01)))),
      footer = div(actionButton("xgb_hyper_settings",label='Close'))))
    
    if (xgb_booster_set() == "dart") {
      shinyjs::enable("dart_normalize_type")
      shinyjs::enable("dart_sample_type")
      shinyjs::enable("rate_drop")
      shinyjs::enable("skip_drop")
    } else {
      shinyjs::disable("dart_normalize_type")
      shinyjs::disable("dart_sample_type")
      shinyjs::disable("rate_drop")
      shinyjs::disable("skip_drop")
    }
  })
  
  observeEvent(input$run_iso_forest, {
    
    # if (is.null(ignored_rows)) {
    #   iso_data0 = current_data()
    #   iso_data = current_data()[,-c(id_var,response_var())]
    # } else {
    #   iso_data0 = current_data()[-ignored_rows,]
    #   iso_data = iso_data0[,-c(id_var,response_var())]
    # }
    
    iso_data = current_data()[,-c(id_var,response_var())]
    
    samp_size = min(nrow(iso_data), 10000L)
    ndim = input$iso_ndim
    
    iso_results = matrix(NA, nrow = nrow(iso_data), ncol = 6)
    iso_results = data.frame(iso_results)
    colnames(iso_results) = c("ID","Depth_Score","Adj_Depth_Score","Density_Score","Adj_Density_Score","Overall")
    
    iso_results[,1] = current_data()[,1]
    
    techs = c("depth","adj_depth", "density", "adj_density")
    
    if (ndim == 1) {
      std_data = FALSE
    } else {
      std_data = TRUE
    }
    
    for (i in 1:4) {
      
      if (ndim == 1 & i != 3 & i != 4) {
        miss_action = "divide"
      } else {
        miss_action = "impute"
      }
      
      if (i == 2) {
        pen_range = TRUE
      } else {
        pen_range = FALSE
      }
      
      if (i == 3 | i == 4) {
        if (ndim == 1) {
          pooled_gain = 0.75
          mingain = 0.25
        } else {
          pooled_gain = 1
          mingain = 0.25
        }
      } else {
        pooled_gain = 0
        mingain = 0
      }
      
      isoforest = isolation.forest(iso_data,sample_size = samp_size,ntrees = 100,ndim = ndim,max_depth = ceiling(log2(samp_size)),
                                   prob_pick_pooled_gain = pooled_gain,min_gain = mingain,missing_action = miss_action,penalize_range = pen_range,
                                   standardize_data = std_data,scoring_metric = techs[[i]],output_score = TRUE,seed = input$data_seed)
      
      iso_results[,i+1] = round(isoforest$scores,3)
    }
    
    iso_results[,6] = round((iso_results[,2] * iso_results[,3] * iso_results[,4] * iso_results[,5])^0.25 - 0.7071,3)
    
    output$iso_outliers = DT::renderDataTable(server = T, {data = datatable(iso_results,rownames = F,selection = list(selection = "multiple",
                                                                                                                      selected = list(rows = NULL),target = "row"),editable = F,options = list(paging = TRUE,pageLength = 25,scrollY = TRUE,
                                                                                                                                                                                               columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                                                                                                                                                                                                 JS("function(settings, json) {","$(this.api().table().header()).css({'background-color':'#073744', 'color': '#fff'});","}")))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Outlier Metric')
    
  })
  
  observeEvent(input$select_choice, ignoreInit = T, {
    
    if (input$select_choice == "Rows") {
      enable("ignore_rows")
      enable("enable_rows")
    } else if (input$select_choice == "Features") {
      disable("ignore_rows")
      disable("enable_rows")
    }
    
    #More stuff to come
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
  })
  
  observeEvent(input$ignore_rows, ignoreInit = T, {
    
    if (input$data_tabs == "Data Table") {
      add_in = input$data_rows_selected[-1]
    }
    if (input$data_tabs == "Outlier Metric") {
      add_in = input$iso_outliers_rows_selected
    }
    
    if(identical(unique(append(ignored_rows,add_in)),integer(0))) {
      ignored_rows <<- NULL
    } else {
      ignored_rows <<- unique(append(ignored_rows,add_in))
    }
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
  })
  
  observeEvent(input$enable_rows, ignoreInit = T, {
    
    if (input$data_tabs == "Data Table") {
      add_back = input$data_rows_selected[-1]
    }
    if (input$data_tabs == "Outlier Metric") {
      add_back = input$iso_outliers_rows_selected
    }
    
    if(identical(ignored_rows[!ignored_rows %in% add_back],integer(0))) {
      ignored_rows <<- NULL
    } else {
      ignored_rows <<- ignored_rows[!ignored_rows %in% add_back]
    }
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
  })
  
  observeEvent(input$xgb_hyper_settings, ignoreInit = T, {
    
    eta_set(input$eta)
    gamma_set(input$gamma)
    max_depth_set(input$max_depth)
    min_child_weight_set(input$min_child_weight)
    nrounds_set(input$nrounds)
    early_stop_set(input$early_stop)
    nfold_set(input$nfold)
    subsamp_set(input$subsamp)
    colsamp_set(input$colsamp)
    
    xgb_tree_method_set(input$xgb_tree_method)
    xgb_booster_set(input$xgb_booster)
    
    dart_normalize_type_set(input$dart_normalize_type)
    dart_sample_type_set(input$dart_sample_type)
    rate_drop_set(input$rate_drop)
    skip_drop_set(input$skip_drop)
    
    Optimal_HP <<- data.frame(max_depth = input$max_depth,eta = input$eta,subsample = input$subsamp,colsample_bytree = input$colsamp,
                              min_child_weight = input$min_child_weight,gamma = input$gamma,nrounds = input$nrounds)
    
    removeModal()
    
  })
  
  observeEvent(input$xgb_booster, {
    if (input$xgb_booster == "dart") {
      shinyjs::enable("dart_normalize_type")
      shinyjs::enable("dart_sample_type")
      shinyjs::enable("rate_drop")
      shinyjs::enable("skip_drop")
    } else {
      shinyjs::disable("dart_normalize_type")
      shinyjs::disable("dart_sample_type")
      shinyjs::disable("rate_drop")
      shinyjs::disable("skip_drop")
    }
  })
}

# Create Shiny app ----
shinyApp(ui, server)