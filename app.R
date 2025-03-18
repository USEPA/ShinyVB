setwd("C:/Users/mcytersk/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/VBWEB_Shiny/Shiny_VB")

library(bsicons)
library(bslib)
library(bsplus)
library(colorspace)
library(devtools)
library(dplyr)
library(DT)
library(ggdist)
library(gghalves)
library(ggplot2)
library(ggtext)
library(grid)
library(gridExtra)
library(Hmisc)
library(hrbrthemes)
library(htmltools)
library(leaflet)
library(lubridate)
library(magrittr)
library(plotly)
library(plyr)
library(ragg)
library(RColorBrewer)
library(reshape2)
library(reactable)
library(readxl)
library(shiny)
library(shinybusy)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(tidyr)
library(units)

source("rain.R")
source("lineplot.R")
source("ui.R")
source("map_click.R")
source("scatter.R")
source("impute.R")
source("lars_coeff.R")
source("lars_uncert.R")
source("xgb_hyper.R")
source("xgb_select.R")
source("xgb_uncert.R")
source("createAO.R")

# Define server logic --
server= function(input,output,session) {
  
  bo = reactiveVal(0)
  current_data = reactiveVal()
  response_var = reactiveVal(2)
  id_var = reactiveVal(1)
  rv = reactiveValues(points = data.frame())
  col_names = reactiveVal()
  cove_names = reactiveVal()
  date_format = reactiveVal()
  progress_list = reactiveVal()
  
  init_data = data.frame()
  date_format_string = ""
  
  renderdata = function(current_data,response_var,id_var){

    pushed_data = current_data
    
    if (date_format_string != "-") {
      
      col_list = seq(1,ncol(current_data))
      remaining = col_list[-id_var]
      
      output$data = DT::renderDataTable(server=T,{
        datatable(pushed_data,rownames=F,selection=list(selected = list(rows = NULL, cols = response_var-1),
                                                        target = "row+column",mode="single"),editable=T,
                  options = list(
                    autoWidth=F,
                    paging = TRUE,
                    pageLength = 25,
                    scrollX = TRUE,
                    scrollY = TRUE,
                    columnDefs = list(list(
                      targets = '_all', className = 'dt-center'
                    )),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
                      "}"))) %>%
          formatStyle(id_var,backgroundColor = 'lightgray') %>%
          formatStyle(response_var,backgroundColor = "#b0bed9") %>%
          formatRound(columns=remaining, digits=c(1,1,1,2,1,5,2,2,1,1,1)) %>%
          formatDate(id_var,date_format_string)
      })
        
    } else {
        
        output$data = DT::renderDataTable(server=T,{
          datatable(pushed_data,rownames=F,selection=list(selected = list(rows = NULL, cols = response_var-1),
                                                          target = "row+column",mode="single"),editable=T,
                    options = list(
                      autoWidth=F,
                      paging = TRUE,
                      pageLength = 25,
                      scrollX = TRUE,
                      scrollY = TRUE,
                      columnDefs = list(list(
                        targets = '_all', className = 'dt-center'
                      )),
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
                        "}"))) %>%
            formatStyle(id_var,backgroundColor = 'lightgray') %>%
            formatStyle(response_var,backgroundColor = "#b0bed9") %>%
            formatRound(columns=1:ncol(pushed_data), digits=c(1,1,1,2,1,5,2,2,1,1,1))
        })
      }
    }
  
  observeEvent(input$data_cell_edit, ignoreInit = T,ignoreNULL = T, {
    
    temp_data=current_data()
    
    info = input$data_cell_edit
    i = info$row
    j = info$col + 1  # offset by 1
    
    temp_data = editData(temp_data, input$data_cell_edit, "data", rownames = FALSE)

    current_data(temp_data)
    
    renderdata(current_data(),response_var(),id_var())
  })
  
  observeEvent(input$restore, {

    response_var(2)
    id_var(1)
    col_names(colnames(init_data))
    current_data(init_data)

    updateSelectInput(session,"rainplot",selected="-")
    updateSelectInput(session,"lineplot",selected="-")
    updateSelectInput(session,"scatterx",selected="-")
    updateSelectInput(session,"scattery",selected="-")
    updateSelectInput(session,"speed",selected="-")
    updateSelectInput(session,"direct",selected="-")
    
    renderdata(current_data(),response_var(),id_var())
    
  })
  
  output$bo_text = renderUI({
    HTML("To determine site orientation, click once anywhere on the shoreline, then again on another point on the shoreline. A third click, <b>made in the water</b>,
          calculates/saves the site orientation. A fourth click clears the map, whereby the process can be repeated.<br><br><i>Note: Any newly calculated orientation replaces the previous one.</i>")
  })

  output$beach_orient = renderText({bo()})
  
  observeEvent(input$IDasDate, {
    date_format(input$IDasDate)
  })
  
  observeEvent(input$file1, ignoreInit = T, {
    
    init_data <<- read.csv(input$file1$datapath,header = input$header,sep = input$sep)
    
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
    updateSelectInput(session,"rainplot",choices=c("-",col_names()))
    updateSelectInput(session,"lineplot",choices=c("-",col_names()))
    updateSelectInput(session,"scatterx",choices=c("-",col_names()))
    updateSelectInput(session,"scattery",choices=c("-",col_names()))
    updateSelectInput(session,"speed",choices=c("-",col_names()))
    updateSelectInput(session,"direct",choices=c("-",col_names()))
    
    renderdata(current_data(),response_var(),id_var())
    
    showModal(modalDialog(
      paste0("The second column has been designated as the response variable by default. To change this, click on the column name at the BOTTOM of the table."),
      easyClose = T
    ))
    
  })
  
  observeEvent(input$id, ignoreInit = T, {
    
    id_num = which(col_names()==input$id)
    
    id_var(id_num)
      
    renderdata(current_data(),response_var(),id_var())
      
    })
  
  observeEvent(input$data_columns_selected, ignoreInit = T, {
    
    if ((input$data_columns_selected+1) != response_var()) {
      response_var(input$data_columns_selected + 1)
      renderdata(current_data(),response_var(),id_var())
    }
    
  })

  observeEvent(input$rainplot, ignoreInit = T, {
    
    if (input$rainplot != "-") {
      
      rain_data0 = current_data()
      
      rain_data1 = cbind(rain_data0[,id_var()],rain_data0[,input$rainplot])
      
      output$rainplot = renderPlot(raincloud(rain_data1))
      
      boxx = modalDialog(plotOutput("rainplot"),title=paste0(input$rainplot," Raincloud Plot"),easyClose = TRUE,size="l")
      
      showModal(boxx)
    }
  })
  
  observeEvent(input$lineplot, ignoreInit = T, {
    
    if (input$lineplot != "-") {
      
      output$lineplot = renderPlot(lineplot(current_data(),input$lineplot,id_var()))
      
      boxx = modalDialog(plotOutput("lineplot"),title=paste0(input$lineplot," Line Plot"),easyClose = TRUE,size="l")
      
      showModal(boxx)
    }
  })
  
  toListen = reactive({
    list(input$scatterx, input$scattery)
  })
  
  observeEvent(toListen(), ignoreInit = T, {
    
    if (input$scatterx != "-" & input$scattery!= "-") {
      
      scatter_data0 = current_data()
      
      ident_var = id_var()
      
      scatter_data1 = cbind(scatter_data0[,ident_var],scatter_data0[,input$scatterx],scatter_data0[,input$scattery])
      
      colnames(scatter_data1) = c("ID",input$scatterx,input$scattery)
      
      output$scatter = renderPlot(scatter(scatter_data1,input$scatterx,input$scattery))
      
      scatt = modalDialog(plotOutput("scatter"),title=paste0("Scatter Plot: ",input$scattery," by ", input$scatterx), easyClose = TRUE, size="l")
      
      showModal(scatt)
    }
  })
  
  observeEvent(input$continue, {
    
    removeModal()
    
    #add_busy_spinner(spin = "fading-circle")
    
    current_data(imputing(current_data(),id_var(),response_var()))
    
    renderdata(current_data(),response_var(),id_var())

  })
  
  observeEvent(input$end, {
    removeModal()
    renderdata(current_data(),response_var(),id_var())
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
        paste0("WARNING: Row numbers (", listing, "), have quite a few missing values. Imputation results for these rows will be highly uncertain. Consider deleting these from the dataset."),
        footer = tagList(actionButton("continue", "Impute Anyways"),actionButton("end", "Exit"))))
      
    } else {
    
    current_data(imputing(current_data(),id_var(),response_var()))
    renderdata(current_data(),response_var(),id_var())
    
    }
  })
  
  observeEvent(input$create, {
      
    current_data(createAO(col_names(),input$speed,input$direct,input$A_name,input$O_name,current_data(),bo()))
      
    col_names(colnames(current_data()))
      
    renderdata(current_data(),response_var(),id_var())
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
      
      cove_names = c(colnames(coves))
      
      updateCheckboxGroupInput(session,"coves_to_use",choices=cove_names,selected=cove_names,inline=T)
    
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
  
  observeEvent(input$xgb_uncert, {
    
    xgb_uncert = xgb_uncert(current_data(),response_var(),id_var(),input$coves_to_use,input$nd_val,input$tntc_val,input$tntc_multy,input$MC_runs,
                            input$loggy,input$randomize,input$xgb_tech,input$drop_rate,input$eta,input$gamma,input$max_depth,input$min_child_weight,input$subsample,
                            input$colsample_bytree,input$samp_prop,input$nrounds,input$early_stopping_rounds) 
    
    xgb_fits = xgb_uncert[,1]
    xgb_predicts = xgb_uncert[,2]
    
    print(xgb_fits)
    print(xgb_predicts)
    
  })
  
  observeEvent(input$xgb_select, {
    
    xgb_select = xgb_select(current_data(),response_var(),input$coves_to_use,input$nd_val,input$tntc_val,input$tntc_multy,input$MC_runs,
                            input$loggy,input$randomize,input$xgb_tech,input$drop_rate,input$eta,input$gamma,input$max_depth,input$min_child_weight,input$subsample,
                            input$colsample_bytree,input$samp_prop,input$nrounds,input$early_stopping_rounds) 
    
    print(xgb_select)
    
  })
  
  observeEvent(input$xgb_hyper, {
    
    xgb_hyper = xgb_hyper(current_data(),response_var(),input$coves_to_use,input$nd_val,input$tntc_val,input$tntc_multy,input$MC_runs,
                          input$loggy,input$randomize,input$xgb_tech,input$drop_rates,input$etas,input$gammas,input$max_depths,input$min_child_weights,input$subsamples,
                          input$colsample_bytrees,input$nroundss,input$early_stopping_roundss,input$nfolds)
    
    print(xgb_hyper)
    
  })
  
  output$map = renderLeaflet({ 
    leaflet() |> 
      addTiles() |> 
      setView(270, 40, zoom = 5)
    })
  
  observeEvent(input$map_click, {map_click(input$map_click,rv,bo)})
  
  observeEvent(input$xgb_params, {
    showModal(div(modalDialog(
      numericInput("eta", label="eta", value = 0.1, min=0,max=1),
      numericInput("gamma", label="gamma", value = 0.5, min=0),
      numericInput("max_depth", label="max tree depth", value = 3, min=1),
      numericInput("min_child_weight", label="min node members", value = 2, min=1),
      numericInput("subsamp", label="Subsampling prop", value = 0.75, min=0,max=1),
      numericInput("colsamp", label="Column sampling prop", value = 0.75, min=0,max=1),
      numericInput("nrounds", label="nrounds", value = 1000, min=10),
      numericInput("early_stop", label="early stopping", value = 10, min=5),
      selectInput(
        "tree_method",
        label = "Tree Method",
        selected ="hist",
        choices = c("hist","exact","approx")),
      selectInput(
        "xgb_tech",
        label = "Booster",
        selected ="DART",
        choices = c("DART","gbtree","gblinear")),
      numericInput("drop_rate", label="Drop Rate", value = 0.1, min=0,max=1),
      numericInput("skip_drop", label="Skip Prob", value = 0.5, min=0,max=1),
      selectInput(
        "normalize_type",
        label = "Normalization Type",
        selected ="tree",
        choices = c("tree","forest")),
      selectInput(
        "sample_type",
        label = "Sample Algorithm",
        selected ="uniform",
        choices = c("uniform","weighted"))),
    style = 'width:900px; padding:5px;'))
  })
}
  
# Create Shiny app ----
shinyApp(ui, server)