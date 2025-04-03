setwd(getwd())

library(bsicons)
library(bslib)
library(bsplus)
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
library(ggtext)
library(grid)
library(gridExtra)
library(hash)
library(Hmisc)
library(hrbrthemes)
library(htmltools)
library(ipc)
library(leaflet)
library(lubridate)
library(magrittr)
library(Nmisc)
library(plotly)
library(plyr)
library(png)
library(promises)
library(ragg)
library(RColorBrewer)
library(RSQLite)
library(reshape2)
library(reactable)
library(readxl)
library(shiny)
library(shinybusy)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinythemes)
library(tidymodels)
library(tidyr)
library(units)
plan(multicore)

source("renderdata.R")
source("rain.R")
source("lineplot.R")
source("ui.R")
source("map_click.R")
source("scatter.R")
source("impute.R")
source("lars_coeff.R")
source("lars_perform.R")
source("xgb_pso.R")
source("xgb_select.R")
source("xgb_perform.R")
source("createAO.R")

#all.functions = list.functions.in.file("app.R", alphabetic = TRUE)

# Define server logic --
server= function(input,output,session) {
  
  temp_db = dbConnect(RSQLite::SQLite(), ":memory:")
  
  bo = reactiveVal(0)
  current_data = reactiveVal()
  response_var = reactiveVal(2)
  id_var = reactiveVal(1)
  rv = reactiveValues(points = data.frame())
  col_names = reactiveVal()
  cove_names = reactiveVal()
  date_format = reactiveVal()
  progress_list = reactiveVal()
  
  xgb_tree_method_set = reactiveVal("hist")
  xgb_booster_set = reactiveVal("-")
  dart_normalize_type_set = reactiveVal("tree")
  dart_sample_type_set = reactiveVal("uniform")
  
  rate_drop_set = reactiveVal(0.1)
  skip_drop_set = reactiveVal(0.5)
  eta_set = reactiveVal(0.1)
  gamma_set = reactiveVal(0.5)
  max_depth_set = reactiveVal(3)
  min_child_weight_set = reactiveVal(3)
  nrounds_set = reactiveVal(1000)
  early_stop_set = reactiveVal(50)
  nfold_set = reactiveVal(10)
  subsamp_set = reactiveVal(0.8)
  colsamp_set = reactiveVal(0.8)
  
  xgb_hyper_result = reactiveVal()
  xgb_hyper_calculation = NULL
  
  xgb_select_result = reactiveVal()
  xgb_select_calculation = NULL
  
  running = reactiveVal(FALSE)
  
  init_data = data.frame()
  date_format_string = ""
  init_feat_props = hash()
  feat_props = hash()
  
  correls = function(current_data,id_var,response_var) {
    
    cov_data = current_data[,-c(id_var,response_var)]
    
    return(cor(cov_data,use="pairwise.complete.obs"))
  }
  
  observeEvent(input$data_cell_edit, ignoreInit = T,ignoreNULL = T, {
    
    temp_data=current_data()
    
    info = input$data_cell_edit
    i = info$row
    j = info$col + 1
    
    temp_data = editData(temp_data, input$data_cell_edit, "data", rownames = FALSE)

    current_data(temp_data)
    
    renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
  })
  
  observeEvent(input$restore, {

    response_var(2)
    id_var(1)
    cove_names(NULL)
    col_names(colnames(init_data))
    current_data(init_data)
    feat_props <<- init_feat_props

    updateSelectInput(session,"id",choices=c(col_names()))
    updateSelectInput(session,"rainplot",selected="-")
    updateSelectInput(session,"lineplot",selected="-")
    updateSelectInput(session,"scatterx",selected="-")
    updateSelectInput(session,"scattery",selected="-")
    updateSelectInput(session,"speed",selected="-")
    updateSelectInput(session,"direct",selected="-")
    

    updateSelectInput(session,"col_props",choices=c("-",col_names()))
    updateSelectInput(session,"rainplot",choices=c("-",col_names()))
    updateSelectInput(session,"lineplot",choices=c("-",col_names()))
    updateSelectInput(session,"scatterx",choices=c("-",col_names()))
    updateSelectInput(session,"scattery",choices=c("-",col_names()))
    
    renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
    
  })
  
  observeEvent(input$corr_check, {
    
    corr_data = correls(current_data(),id_var(),response_var())
    
    output$corrplot = renderPlot({
               corrplot(corr_data, addCoef.col = 'black', method="circle", cl.pos = 'n', is.corr = FALSE, type="lower",col.lim = c(-1.4, 1.4),
                col = COL2('PRGn'), tl.col="black", tl.srt= 45)},height = 900, width = 900)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Correlations")
  })
  
  observeEvent(input$save_corrr, ignoreInit = T, {
    
    corr_data = correls(current_data(),id_var(),response_var())
    
    output$save_corr = downloadHandler(
      filename= "Correlations.png",
      content = function(file) {
        on.exit(removeModal())
        png(file, width=input$corr_width, height=input$corr_height, units="in", res=input$corr_rez)
        
        corrplot(corr_data, addCoef.col = 'black', method="circle", cl.pos = 'n', is.corr = FALSE, mar=c(0,0,2,0),
                      type="lower",col.lim = c(-1.4, 1.4), col = COL2('PRGn'), tl.col="black", tl.srt= 45, title=input$corr_title, cex.main = 2,)
        
        
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
  
  observeEvent(input$IDasDate, {
    date_format(input$IDasDate)
  })
  
  observeEvent(input$file1, ignoreInit = T, {
    
    init_data <<- read.csv(input$file1$datapath,header = input$header,sep = input$sep)
    feat_props_temp = hash()
    
    for (i in 1:ncol(init_data)) {
      .set(feat_props_temp,keys=colnames(init_data)[i],values=c(prop1=2,prop2=NA,prop3=NA,prop4=NA))
    }
    
    init_feat_props <<- feat_props_temp
    feat_props <<- feat_props_temp
    
    
    if (date_format() == "YMD") {
      init_data[,1] = ymd(init_data[,1])
      date_format_string <<- "toLocaleDateString"
    } else if (date_format() == "MDY") {
      init_data[,1] = mdy(init_data[,1])
      date_format_string <<- "toLocaleDateString"
    } else if (date_format() == "MDYHM") {
      init_data[,1] = parse_date_time(init_data[,1],c('%m/%d/%y %H:%M'),exact=TRUE)
      date_format_string <<- "toLocaleString"
    } else {
      date_format_string <<- "-"
    }
    
    current_data(init_data)
    col_names(colnames(init_data))
    
    updateSelectInput(session,"id",choices=c(col_names()))
    updateSelectInput(session,"col_props",choices=c("-",col_names()))
    updateSelectInput(session,"rainplot",choices=c("-",col_names()))
    updateSelectInput(session,"lineplot",choices=c("-",col_names()))
    updateSelectInput(session,"scatterx",choices=c("-",col_names()))
    updateSelectInput(session,"scattery",choices=c("-",col_names()))
    updateSelectInput(session,"speed",choices=c("-",col_names()))
    updateSelectInput(session,"direct",choices=c("-",col_names()))
    
    renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Data Table")
    
    showModal(modalDialog(
      paste0("The second column has been designated as the response variable by default. 
             To change this, click on the column name at the BOTTOM of the table."),
      easyClose = F,
      footer = div(modalButton('Close'))
      ))
  })
  
  observeEvent(input$id, ignoreInit = T, {
    
    id_num = which(col_names()==input$id)
    id_var(id_num)
    renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
      
    })
  
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
    
    renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
    
  })
  
  observeEvent(input$data_columns_selected, ignoreInit = T, {
    
    if ((input$data_columns_selected+1) != response_var()) {
      response_var(input$data_columns_selected + 1)
      renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
    }
    
  })

  observeEvent(input$rainplot, ignoreInit = T, {
    
    if (input$rainplot != "-") {
      
      rain_data0 = current_data()
      rain_data1 = cbind(rain_data0[,id_var()],rain_data0[,input$rainplot])
      colnames(rain_data1) = c("ID",input$rainplot)
      
      output$rainplot = renderPlot({raincloud(rain_data1)},height = 900, width = 1200)

      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = "Raincloud")
    }
  })
  
  observeEvent(input$save_rainn, ignoreInit = T, {
    
    rain_data = current_data()
    rain_data1 = data.frame(cbind(rain_data[,id_var()],rain_data[,input$rainplot]))
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
        plot(ggplot(rain_data1, aes(1,rain_data1[,2])) +
               
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
      
      line_data0 = current_data()
      line_data1 = cbind(line_data0[,id_var()],line_data0[,input$lineplot])
      colnames(line_data1) = c("ID",input$lineplot)
      
      output$lineplott = renderPlotly({lineplot(line_data1,input$lineplot)})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Line Plot')
    }
  })
  
  observeEvent(input$save_linee, ignoreInit = T, {
    
    temp_data = current_data()

    output$save_line = downloadHandler(
      filename= "Lineplot.png",
      content = function(file) {
        on.exit(removeModal())
        
        png(file, width=input$line_width, height=input$line_height, units="in", res=input$line_rez)

        plot(ggplot(data=temp_data, aes(x=temp_data[,id_var()], y=temp_data[,input$lineplot])) +
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

  ScatPlot = reactive({
    list(input$scatterx, input$scattery)
  })
  
  observeEvent(ScatPlot(), ignoreInit = T, {
    
    if (input$scatterx != "-" & input$scattery!= "-") {
      
      scatter_data0 = current_data()
      scatter_data1 = cbind(scatter_data0[,id_var()],scatter_data0[,input$scatterx],scatter_data0[,input$scattery])
      colnames(scatter_data1) = c("ID",input$scatterx,input$scattery)
      
      output$scatplot = renderPlotly(scatter(scatter_data1,input$scatterx,input$scattery,id_var()))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Scatterplot')

    }
  })
  
  observeEvent(input$save_scatt, ignoreInit = T, {
    
    temp_data = current_data()
    
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
    current_data(imputing(current_data(),id_var(),response_var()))
    renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
  })
  
  observeEvent(input$end_impute, {
    renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
    removeModal()
  })
  
  observeEvent(input$impute_check, {
    
    temp_data = current_data()
    
    crit_n = floor((ncol(temp_data)-2)*0.3)
    
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
               will be highly uncertain. Consider deleting these from the dataset."),
        footer = tagList(actionButton("continue_impute", "Impute Anyways"),actionButton("end_impute", "Exit"))))
      
    } else {
    
    current_data(imputing(current_data(),id_var(),response_var()))
    renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
    
    }
  })
  
  observeEvent(input$create, {
    
    if (input$speed != "-" & input$direct != "-") {
      
      if (!(input$A_name %in% col_names()) & !(input$O_name %in% col_names())) {
        
        new_data = createAO(col_names(),input$speed,input$direct,input$A_name,input$O_name,current_data(),bo())

        for (i in (ncol(new_data)-1):ncol(new_data)) {
          .set(feat_props,keys=colnames(new_data)[i],values=c(prop1=2,prop2=NA,prop3=NA,prop4=NA))
        }

        current_data(new_data)
        col_names(colnames(current_data()))

        updateSelectInput(session,"id",choices=c(col_names()))
        updateSelectInput(session,"col_props",choices=c("-",col_names()))
        updateSelectInput(session,"rainplot",choices=c("-",col_names()))
        updateSelectInput(session,"lineplot",choices=c("-",col_names()))
        updateSelectInput(session,"scatterx",choices=c("-",col_names()))
        updateSelectInput(session,"scattery",choices=c("-",col_names()))

        renderdata(current_data(),response_var(),id_var(),date_format_string,feat_props,output)
        
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
  
      coves = as.data.frame(current_data())
      cov_list = seq(1,ncol(coves))
      
      min_col_removed = min(id_var(),response_var())
      max_col_removed = max(id_var(),response_var())
      
      removed = c(min_col_removed,max_col_removed)
      remaining = cov_list[! cov_list %in% removed]
      
      coves = coves[,remaining]
      
      covar_names = c(colnames(coves))
      
      updateCheckboxGroupInput(session,"coves_to_use",choices=covar_names,selected=covar_names,inline=T)
      
      cove_names(covar_names)
    
    } else {
      return()
    }
  })
  
  observeEvent(input$lars_coeff, {
    
    lars_coeffs = lars_coeff(current_data(),response_var(),input$coves_to_use,input$nd_val,input$tntc_val,input$tntc_multy,input$MC_runs,
                              input$loggy,input$randomize,input$lars_tech,input$standardize,input$max_steps)
    
    print(lars_coeffs)
    
  })
  
  observeEvent(input$lars_uncert, {
    
    lars_uncert = lars_uncert(current_data(),response_var(),id_var(),input$samp_prop,input$coves_to_use,input$nd_val,input$tntc_val,input$tntc_multy,
                              input$MC_runs,input$loggy,input$randomize,input$lars_tech,input$standardize,input$max_steps)
    
    lars_fits = lars_uncert[,1]
    lars_predicts = lars_uncert[,2]
    
    print(lars_fits)
    print(lars_predicts)
    
  })
  
  # observeEvent(input$xgb_perform, {
  #   
  #   xgb_perform = xgb_perform(current_data(),response_var(),id_var(),input$rnd_seed,input$coves_to_use,input$nd_val,input$tntc_val,input$tntc_multy,input$MC_runs,
  #                           input$loggy,input$randomize,input$xgb_tech,input$rate_drop,input$eta,input$gamma,input$max_depth,input$min_child_weight,input$subsample,
  #                           input$colsample_bytree,input$samp_prop,input$nrounds,input$early_stopping_rounds) 
  #   
  #   xgb_fits = xgb_perform[,1]
  #   xgb_predicts = xgb_perform[,2]
  #   
  #   print(xgb_fits)
  #   print(xgb_predicts)
  #   
  # })
  
  observeEvent(input$xgb_select, {
    
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
    
    xgb_select_data = current_data()
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
    seed = input$rnd_seed
    
    xgb_select_result(NULL)
    
    xgb_select_calculation <<- future({
      
      xgb_select(xgb_select_data,seed,resvar,coves_to_use,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,xgb_standardize,xgb_tree_method,xgb_boost,
                 dart_normalize_type,dart_sample_type,rate_drop,skip_drop,eta,gamma,max_depth,min_child_weight,subsamp,colsamp,nrounds,early_stop,test_weight,temp_db)
      
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
      data = datatable(xgb_select_result(),rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),target = "row",mode="single"),editable=F,
                options = list(
                  autoWidth=F,
                  paging = TRUE,
                  pageLength = 25,
                  scrollX = TRUE,
                  scrollY = TRUE,
                  columnDefs = list(list(className = 'dt-center',orderable=T,targets=0)),
                  initComplete = JS("function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}"))) %>%
                  formatRound(columns=c(1,3:6), digits=c(0,5,5,5,5))
      data$x$data[[1]] = as.numeric(data$x$data[[1]])
      data
    })
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Feat. Selection')
    
    #Return something other than the future so we don't block the UI
    NULL
    
  })
  
  observeEvent(input$xgb_select_cancel, {
    print("Stopping calculation...")
    stopMulticoreFuture(xgb_select_calculation)
  })
  
  observeEvent(input$xgb_select_rows_selected, ignoreInit = T, {
    
    all_covar = cove_names()
    temp_data = dbReadTable(temp_db, "xgb_select_results")
    
    crit_val = as.numeric(input$xgb_select_rows_selected[1])
    
    if (crit_val > 1) {
      
    covar_drop_list = temp_data[which(as.numeric(temp_data$Iteration) < crit_val),"Worst.SHAP"]
    remaining = all_covar[-which(all_covar %in% covar_drop_list)]
    
    } else {
      
      remaining = all_covar
    }
    
    updateCheckboxGroupInput(session,"coves_to_use",choices=cove_names(),selected=remaining,inline=T)
  })
  
  observeEvent(input$xgb_perform, {
    
    showModal(modalDialog(title="XGB Performance", card(
      
      fluidRow(
        column(12,selectInput("xgb_hyper_metric", "Evaluation Metric", choices = c("rmse","mae","mape","logloss"), selected = "rmse"))),
      fluidRow(
        column(6,numericInput("pso_max_iter", "Max Iterations", min=1, max=200, value=10, step = 1)),
        column(6,numericInput("pso_swarm_size", "Swarm Size", min=1, max=200, value=20, step = 1)))),
      footer = div(actionButton("run_xgb_perform", "Run"),modalButton('Close'),actionButton("stop_xgb_perform", "Cancel the Calculation"))
      ))
  })
  
  observeEvent(input$run_xgb_perform, {
    
    set.seed(seed)
    
    xgb_perform_data = current_data()
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    xgb_perform_data = xgb_perform_data[!is.na(xgb_perform_data[, 1]), ]
    
    if (xgb_standardize == TRUE) {
      for (i in 1:nrow(xgb_perform_data)) {
        for (j in 1:ncol(xgb_perform_data)) {
          if (is.numeric(xgb_perform_data[i, j]) == TRUE) {
            xgb_perform_data[i, j] = (xgb_perform_data[i, j] - min(na.omit(xgb_perform_data[, j]))) / (max(na.omit(xgb_perform_data[, j])) - min(na.omit(xgb_perform_data[, j])))
          }
        }
      }
    }
    
    #Randomly shuffle the data
    xgb_perform_data = xgb_perform_data[sample(nrow(xgb_perform_data)),]
    
    #Create 5 equally size folds
    folds = cut(seq(1,nrow(xgb_perform_data)),breaks=5,labels=FALSE)
    
    #Perform 5 fold cross validation
    for(i in 1:5) {
      
      testIndexes = which(folds==i,arr.ind=TRUE)
      testData = xgb_perform_data[testIndexes, ]
      trainData = xgb_perform_data[-testIndexes, ]
      
      pso_result = xgb_perform(trainData,testData,response_var(),input$coves_to_use,input$lc_lowval,input$lc_upval,input$rc_lowval,input$rc_upval,
              input$MC_runs,input$loggy,input$randomize,input$xgb_standardize,input$xgb_hyper_metric,input$pso_max_iter,input$pso_swarm_size)
      
  })
  
  observeEvent(input$stop_xgb_perform, {
    print("Stopping calculation...")
    stopMulticoreFuture(xgb_perform_calculation)
  })
  
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
  
  output$map = renderLeaflet({ 
    leaflet() |> 
      addTiles() |> 
      setView(270, 40, zoom = 5)
    })
  
  observeEvent(input$map_click, {map_click(input$map_click,rv,bo)})
  
  observeEvent(input$xgb_params, {
    
      showModal(modalDialog(title="XGB Hyperparameters",easyClose=F,card(
        fluidRow(
          column(4,numericInput("eta", label="Eta", value = eta_set(),min=0,max=1)),
          column(4,numericInput("gamma", label="Gamma", value = gamma_set(), min=0)),
          column(4,numericInput("max_depth", label="Max Tree Depth", value = max_depth_set(), min=1))),
        fluidRow(
          column(6,numericInput("min_child_weight", label="Min Child Weight", value = min_child_weight_set(), min=1)),
          column(6,numericInput("nfold", label="CV Folds", value = nfold_set(), min=2,max=20))),
        fluidRow(
          column(6,numericInput("nrounds", label="Iteration Rounds", value = nrounds_set(), min=100)),
          column(6,numericInput("early_stop", label="Early Stopping", value = early_stop_set(), min=10))),
        fluidRow(
          column(6,numericInput("subsamp", label="Subsample Proportion", value = subsamp_set(), min=0,max=1)),
          column(6,numericInput("colsamp", label="Column Sample Proportion", value = colsamp_set(), min=0,max=1))),
        fluidRow(
          column(4, selectInput("xgb_tree_method",
                                label = "Tree Method",
                                selected =xgb_tree_method_set(),
                                choices = c("hist","exact","approx"))),
          column(4, selectInput("xgb_booster",
                                label = "Booster",
                                selected ="-",
                                choices = c("-","gbtree","gblinear","dart")))),
        fluidRow(
          column(6, disabled(selectInput(
            "dart_normalize_type",
            label = "Normalization Type",
            selected =dart_normalize_type_set(),
            choices = c("tree","forest")))),
          column(6, disabled(selectInput(
            "dart_sample_type",
            label = "Sample Algorithm",
            selected =dart_sample_type_set(),
            choices = c("uniform","weighted"))))),
        # fluidRow(
        #   column(12, selectInput("objective",
        #          label = "Objective Fcn",
        #          selected ="reg:linear",
        #          choices = c("reg:linear","reg:logistic","binary:logistic","multi:softmax","multi:softprob")))),
        fluidRow(
          column(6,disabled(numericInput("rate_drop", label="Drop Rate", value = rate_drop_set(), min=0,max=1))),
          column(6,disabled(numericInput("skip_drop", label="Skip Prob", value = skip_drop_set(), min=0,max=1))))),
        footer = div(actionButton("xgb_hyper_settings",label='Close'))))
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
    
    removeModal()

  })
  
  observeEvent(input$xgb_booster, {
    
    if (input$xgb_booster == "dart") {
      shinyjs::toggleState("dart_normalize_type")
      shinyjs::toggleState("dart_sample_type")
      shinyjs::toggleState("rate_drop")
      shinyjs::toggleState("skip_drop")
    }
  })
}
  
# Create Shiny app ----
shinyApp(ui, server)