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
source("xgb_hyper.R")
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
    
    output$plot = renderPlot({
      corrplot(corr_data, addCoef.col = 'black', method="circle", cl.pos = 'n', is.corr = FALSE, type="lower",col.lim = c(-1.4, 1.4), col = COL2('PRGn'), tl.col="black", tl.srt= 45)
    },height = 1000, width = 1000)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'Plots')
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
    
    showModal(modalDialog(
      paste0("The second column has been designated as the response variable by default. To change this, click on the column name at the BOTTOM of the table."),
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
          column(3,numericInput("prop2",  label="Prop Missing", value = values(feat_props,keys=input$col_props)[2], min=0,max=1,step=0.05)),
          column(3,textInput("prop3",  label="Color", value = values(feat_props,keys=input$col_props)[3])),
          column(3,textInput("prop4",  label="Status", value = values(feat_props,keys=input$col_props)[4])))),
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
      output$rainplot = renderPlot(raincloud(rain_data1))
      
      boxx = modalDialog(plotOutput("rainplot"),title=paste0(input$rainplot," Raincloud Plot"),easyClose = F,size="l",
            footer = div(modalButton('Close')))
      
      showModal(boxx)
    }
  })
  
  observeEvent(input$lineplot, ignoreInit = T, {
    
    if (input$lineplot != "-") {
      
      output$lineplot = renderPlot(lineplot(current_data(),input$lineplot,id_var()))
      boxx = modalDialog(plotOutput("lineplot"),title=paste0(input$lineplot," Line Plot"),easyClose = F,size="l",footer = div(modalButton('Close')))
      showModal(boxx)
    }
  })
  
  ScatPlot = reactive({
    list(input$scatterx, input$scattery)
  })
  
  observeEvent(ScatPlot(), ignoreInit = T, {
    
    if (input$scatterx != "-" & input$scattery!= "-") {
      
      scatter_data0 = current_data()
      
      ident_var = id_var()
      
      scatter_data1 = cbind(scatter_data0[,ident_var],scatter_data0[,input$scatterx],scatter_data0[,input$scattery])
      
      colnames(scatter_data1) = c("ID",input$scatterx,input$scattery)
      
      output$scatter = renderPlot(scatter(scatter_data1,input$scatterx,input$scattery))
      
      scatt = modalDialog(plotOutput("scatter"),title=paste0("Scatter Plot: ",input$scattery," by ", input$scatterx), easyClose = F, size="l",footer = div(modalButton('Close')))
      
      showModal(scatt)
    }
  })
  
  observeEvent(input$continue_impute, {
    
    removeModal()
    current_data(imputing(current_data(),id_var(),response_var()))
    #add_busy_spinner(spin = "fading-circle")
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
        paste0("WARNING: Row numbers (", listing, "), have quite a few missing values. Imputation results for these rows will be highly uncertain. Consider deleting these from the dataset."),
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
        showModal(modalDialog(div("ERROR: BOTH new component columns must have different names than any currently existing column names.",style="font-size:160%"),easyClose = T))
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
  
  observeEvent(input$xgb_uncert, {
    
    xgb_uncert = xgb_uncert(current_data(),response_var(),id_var(),input$coves_to_use,input$nd_val,input$tntc_val,input$tntc_multy,input$MC_runs,
                            input$loggy,input$randomize,input$xgb_tech,input$rate_drop,input$eta,input$gamma,input$max_depth,input$min_child_weight,input$subsample,
                            input$colsample_bytree,input$samp_prop,input$nrounds,input$early_stopping_rounds) 
    
    xgb_fits = xgb_uncert[,1]
    xgb_predicts = xgb_uncert[,2]
    
    print(xgb_fits)
    print(xgb_predicts)
    
  })
  
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
    
    xgb_select_result(NULL)
    
    xgb_select_calculation <<- future({
      
      xgb_select(xgb_select_data,resvar,coves_to_use,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,xgb_standardize,xgb_tree_method,xgb_boost,
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
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Covariates')
    
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
  
  observeEvent(input$xgb_hyper_ranges, {
    
    showModal(modalDialog(title="Hyperparameter Grid Search Ranges", card(
      
      fluidRow(
        column(6,selectInput("xgb_hyper_metric", "Evaluation Metric", choices = c("rmse","mae","mape","logloss"), selected = "rmse")),
        column(6)),
      fluidRow(
        column(12,sliderInput("eta_r", "eta", 0, 1, width="100%", value=c(0.1,0.20), step = 0.05,
                              ticks = F, dragRange = TRUE))),
      fluidRow(
        column(12,sliderInput("gamma_r", "gamma", 0, 10, width="100%", value=c(0.5,0.75), step = 0.25,
                              ticks = F, dragRange = TRUE))),
      fluidRow(
        column(12,sliderInput("max_depth_r", "Max Tree Depth", 1, 10, width="100%", value=c(2,4), step = 1,
                              ticks = F, dragRange = TRUE))),
      fluidRow(
        column(12,sliderInput("min_child_weight_r", "Min Node Members", 1, 20, width="100%", value=c(2,4), step = 1,
                              ticks = F, dragRange = TRUE))),
      fluidRow(
        column(12,sliderInput("nrounds_r", "Number of Iterations", 100, 2000, width="100%", value=c(500,1000), step = 100,
                              ticks = F, dragRange = TRUE))),
      fluidRow(
        column(12,sliderInput("early_stop_r", "Early Stopping", 10, 100, width="100%", value=c(40,50), step = 10,
                              ticks = F, dragRange = TRUE))),
      fluidRow(
        column(12,sliderInput("nfold_r", "CV Folds", 2, 20, width="100%", value=c(5,10), step = 1,
                              ticks = F, dragRange = TRUE))),
      fluidRow(
        column(12,sliderInput("subsamp_r", "Subsampling Prop", 0, 1, width="100%", value=c(0.75,0.85), step = 0.05,
                              ticks = F, dragRange = TRUE))),
      fluidRow(
        column(12,sliderInput("colsamp_r", "Column Sample Prop", 0, 1, width="100%", value=c(0.75,0.85), step = 0.05,
                              ticks = F, dragRange = TRUE)))),
      footer = div(actionButton("run_xgb_hyper", "Run"),modalButton('Close'),actionButton("stop_xgb_hyper", "Cancel the Calculation"))
      ))
  })
  
  observeEvent(input$run_xgb_hyper, {
    
    if(running())
      return(NULL)
    running(TRUE)
    
    eta_list = seq(from = input$eta_r[1], to = input$eta_r[2],by = 0.05)
    
    gamma_list = seq(from = input$gamma_r[1], to = input$gamma_r[2],by = 0.25)
    
    max_depth_list = seq(from = input$max_depth_r[1], to = input$max_depth_r[2],by = 1)
    
    min_child_weight_list = seq(from = input$min_child_weight_r[1], to = input$min_child_weight_r[2],by = 1)
    
    nrounds_list = seq(from = input$nrounds_r[1], to = input$nrounds_r[2],by = 100)
    
    early_stop_list =seq(from = input$early_stop_r[1], to = input$early_stop_r[2],by = 10)
    
    nfold_list = seq(from = input$nfold_r[1], to = input$nfold_r[2],by = 1)
    
    subsamp_list = seq(from = input$subsamp_r[1], to = input$subsamp_r[2],by = 0.05)
    
    colsamp_list = seq(from = input$colsamp_r[1], to = input$colsamp_r[2],by = 0.05)
    
    xgb_hyper_data = current_data()
    resvar = response_var()
    coves_to_use = input$coves_to_use
    lc_lowval = input$lc_lowval
    lc_upval = input$lc_upval
    rc_lowval = input$rc_lowval
    rc_upval = input$rc_upval
    MC_runs = input$MC_runs
    loggy = input$loggy
    randomize = input$randomize
    xgb_standardize = input$xgb_standardize
    xgb_hyper_metric = input$xgb_hyper_metric

    xgb_hyper_result(NULL)
    
    xgb_hyper_calculation <<- future({
      
      xgb_hyper(xgb_hyper_data,resvar,coves_to_use,lc_lowval,lc_upval,rc_lowval,rc_upval, MC_runs,loggy,randomize,xgb_standardize,xgb_hyper_metric,
                eta_list,gamma_list,max_depth_list,min_child_weight_list,subsamp_list,colsamp_list,nrounds_list,nfold_list,early_stop_list)
  
    }, seed=TRUE)
    
    
    prom = xgb_hyper_calculation %...>% xgb_hyper_result
    
    prom <- catch(xgb_hyper_calculation,
                  function(e){
                    xgb_hyper_result(NULL)
                    showModal(modalDialog(paste0("Hyperparameter Grid Search cancelled. No results generated."),footer = modalButton("Close")))
                  })
    
    prom = finally(prom, function(){
      running(FALSE)
    })

    output$xgb_hyper = DT::renderDataTable(server=T,{
      datatable(xgb_hyper_result(),rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),target = "row",mode="single"),editable=F,
                options = list(
                  autoWidth=F,
                  paging = TRUE,
                  pageLength = 25,
                  scrollX = TRUE,
                  scrollY = TRUE,
                  columnDefs = list(list(targets = '_all', className = 'dt-center')),
                  initComplete = JS("function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))
    })
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Hyper Optimize')
    
    #Return something other than the future so we don't block the UI
    NULL
    
  })
  
  observeEvent(input$stop_xgb_hyper, {
    print("Stopping calculation...")
    stopMulticoreFuture(xgb_hyper_calculation)
  })
  
  observeEvent(input$xgb_hyper_rows_selected, ignoreInit = T, {
    
    matrix = xgb_hyper_result()
    
    eta_set(matrix[input$xgb_hyper_rows_selected,"eta"])
    gamma_set(matrix[input$xgb_hyper_rows_selected,"gamma"])
    max_depth_set(matrix[input$xgb_hyper_rows_selected,"max_depth"])
    min_child_weight_set(matrix[input$xgb_hyper_rows_selected,"min_child_weight"])
    nrounds_set(matrix[input$xgb_hyper_rows_selected,"nrounds"])
    early_stop_set(matrix[input$xgb_hyper_rows_selected,"early_stopping_rounds"])
    nfold_set(matrix[input$xgb_hyper_rows_selected,"nfold"])
    subsamp_set(matrix[input$xgb_hyper_rows_selected,"subsample"])
    colsamp_set(matrix[input$xgb_hyper_rows_selected,"colsample_bytree"])
    
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