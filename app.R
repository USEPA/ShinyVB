setwd(getwd())

library(bslib)
library(bsplus)
library(cluster)
library(corrplot)
library(future)
library(ggdist)
library(ggplot2)
library(glmnetUtils)
library(grDevices)
library(grid)
library(hash)
library(ipc)
library(isotree)
library(leaflet)
library(lubridate)
library(Matrix)
library(methods)
library(missForest)
library(openxlsx)
library(plotly)
library(plyr)
library(promises)
library(pso)
library(purrr)
library(RSQLite)
library(SHAPforxgboost)
library(shiny)
library(shinybusy)
library(shinyjs)
library(shinythemes)
library(shinyvalidate)
library(stats)
library(stringr)
library(tidyverse)
library(tools)
library(units)
library(xgboost)
library(DT)
# library(NCmisc)
# library(here)

plan(multicore)

source("renderdata.R")
source("renderPCAdata.R")
source("rain.R")
source("lineplot.R")
source("ui.R")
source("map_click.R")
source("scatter.R")
source("scatter_confuse.R")
source("impute.R")
source("createAO.R")
source("confusion.R")
source("xgbcl_call_optimize_HP.R")
source("xgbcl_call_predict.R")
source("xgb_call_optimize_HP.R")
source("xgb_call_predict.R")
source("xgb_pso.R")
source("xgbcl_pso.R")
source("xgb_pred_errors.R")
source("xgbcl_pred_errors.R")
source("xgb_feature_selection.R")
source("xgbcl_feature_selection.R")

# Define server logic

server= function(input,output,session) {
  
  # Code to show what libraries are in use by the project
  # funcs = 
  #   list.files(here::here(), pattern ="\\.R$", recursive = TRUE, full.names = TRUE) %>%
  #   map(list.functions.in.file) %>%
  #   flatten
  
  # Extract just the unique package names
  # packages <- 
  #   funcs %>%
  #   names %>%
  #   str_extract("package:[[:alnum:]]*") %>%
  #   str_split(",") %>%
  #   unlist %>%
  #   str_remove("package:") %>%
  #   unique %>%
  #   sort
  # 
  # packages
  
  # Validation Rules for most numeric inputs
  
  iv = InputValidator$new()
  
  # iv$add_rule("lc_lowval", sv_between(0,1))
  # iv$add_rule("lc_upval", sv_between(1,10))
  # iv$add_rule("rc_lowval", sv_between(0,100))
  # iv$add_rule("rc_upval", sv_between(1000,10000))
  
  iv$add_rule("train_pct", sv_between(1,100))
  iv$add_rule("MC_runs", sv_between(1,10000))
  iv$add_rule("num_folds", sv_between(2,20))
  
  iv$add_rule("data_seed", sv_gte(1))
  iv$add_rule("model_seed", sv_gte(1))
  iv$add_rule("iso_ndim", sv_between(1, 10))
  iv$add_rule("sig_digies", sv_between(0, 12))
  
  iv$add_rule("psocl_max_iter", sv_between(5, 1000))
  iv$add_rule("psocl_swarm_size", sv_between(3, 200))
  iv$add_rule("membercl_exp", sv_between(0.25, 3))
  iv$add_rule("sscl_exp", sv_between(0.25,3))
  
  iv$add_rule("etacl", sv_between(0,1))
  iv$add_rule("gammacl", sv_between(0,20))
  iv$add_rule("nroundscl", sv_between(100,3000))
  iv$add_rule("max_depthcl", sv_between(1,10))
  iv$add_rule("min_child_weightcl", sv_between(1,20))
  iv$add_rule("subsampcl", sv_between(0,1))
  iv$add_rule("colsampcl", sv_between(0,1))
  iv$add_rule("ratecl_drop", sv_between(0,1))
  iv$add_rule("skipcl_drop", sv_between(0,1))
  
  iv$add_rule("LG_pred_dc", sv_between(0, 1))
  iv$add_rule("LG_fit_dc", sv_between(0, 1))
  iv$add_rule("XGBCL_pred_dc", sv_between(0,1))
  iv$add_rule("XGBCL_dec_crit", sv_between(0,1))
  
  iv$add_rule("pso_max_iter", sv_between(5, 1000))
  iv$add_rule("pso_swarm_size", sv_between(3, 200))
  iv$add_rule("member_exp", sv_between(0.25, 3))
  iv$add_rule("ss_exp", sv_between(0.25,3))
  
  iv$add_rule("eta", sv_between(0,1))
  iv$add_rule("gamma", sv_between(0,20))
  iv$add_rule("nrounds", sv_between(100,3000))
  iv$add_rule("max_depth", sv_between(1,10))
  iv$add_rule("min_child_weight", sv_between(1,20))
  iv$add_rule("subsamp", sv_between(0,1))
  iv$add_rule("colsamp", sv_between(0,1))
  iv$add_rule("rate_drop", sv_between(0,1))
  iv$add_rule("skip_drop", sv_between(0,1))
  
  iv$enable()
  
  # Read in WQX station and shoreline data
  
  station_data <<- data.frame(read.csv("stations.csv"))
  shoreline_data <<- data.frame(read.csv("shorelines.csv"))
  
  # Create a temporary SQL database
  
  temp_db = dbConnect(RSQLite::SQLite(), ":memory:")
  
  # Leaflet reactive variables
  
  bo = reactiveVal(0)
  map_clicks = reactiveValues(points = data.frame())
  
  # General reactive variables
  
  current_data = reactiveVal()
  response_var = reactiveVal(2)
  col_names = reactiveVal()
  feat_names = reactiveVal()
  feats_being_used = reactiveVal()
  init_data = data.frame()
  init_feat_props = hash()
  feat_props = hash()
  PCA_dataset = reactiveVal()
  pca_axes = reactiveVal()
  pcax_being_used = reactiveVal()
  
  # General non-reactive variables
  
  id_var = 1
  ignored_rows = NULL
  date_format_string = "Other"

  # XGB hyperparameters
  
  xgb_tree_method_set = reactiveVal("hist")
  xgb_booster_set = reactiveVal("gbtree")
  dart_normalize_type_set = reactiveVal("tree")
  dart_sample_type_set = reactiveVal("uniform")
  rate_drop_set = reactiveVal(0.1)
  skip_drop_set = reactiveVal(0.5)
  eta_set = reactiveVal(0.05)
  gamma_set = reactiveVal(1)
  max_depth_set = reactiveVal(2)
  min_child_weight_set = reactiveVal(3)
  nrounds_set = reactiveVal(100)
  subsamp_set = reactiveVal(0.8)
  colsamp_set = reactiveVal(0.8)
  xgb_select_result = reactiveVal()
  xgb_select_calculation = NULL
  running = reactiveVal(FALSE)
  # xgb_hyper_result = reactiveVal()
  # xgb_hyper_calculation = NULL
  
  # XGBCL hyperparameters
  
  xgbcl_tree_method_set = reactiveVal("hist")
  xgbcl_booster_set = reactiveVal("gbtree")
  dartcl_normalize_type_set = reactiveVal("tree")
  dartcl_sample_type_set = reactiveVal("uniform")
  ratecl_drop_set = reactiveVal(0.1)
  skipcl_drop_set = reactiveVal(0.5)
  etacl_set = reactiveVal(0.05)
  gammacl_set = reactiveVal(1)
  max_depthcl_set = reactiveVal(2)
  min_child_weightcl_set = reactiveVal(3)
  nroundscl_set = reactiveVal(100)
  subsampcl_set = reactiveVal(0.8)
  colsampcl_set = reactiveVal(0.8)
  xgbcl_select_result = reactiveVal()
  xgbcl_select_calculation = NULL
  
  #General modeling hyperparameters
  
  early_stop_set = reactiveVal(20)
  nfold_set = reactiveVal(5)
  
  # Logistic Regression results
  
  LG_scat_dat = data.frame()
  LG_pred_scat_dat = data.frame()
  LG_model = NULL
  
  # Elastic Net results
  
  EN_scat_dat = data.frame()
  EN_pred_scat_dat = data.frame()
  EN_model = NULL
  
  # XGBoost Classifier results
  
  XGBCL_scat_dat = data.frame()
  XGBCL_pred_scat_dat = data.frame()
  Optimal_CLHP = data.frame(max_depth = 2,eta = 0.05,subsample = 0.8,colsample_bytree = 0.8,min_child_weight = 3,gamma = 1,nrounds = 100)
  XGBCL_model = NULL
  
  # XGBoost results
  
  XGB_scat_dat = data.frame()
  XGB_pred_scat_dat = data.frame()
  XGB_saved_predictions = data.frame()
  Optimal_HP = data.frame(max_depth = 2,eta = 0.05,subsample = 0.8,colsample_bytree = 0.8,min_child_weight = 3,gamma = 1,nrounds = 100)
  XGB_model = NULL

  # Render leaflet map
  
  output$map = renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(270, 40, zoom = 5)
  })
  
  # Compute beach orientation based on map clicks
  
  observeEvent(input$map_click, {map_click(input$map_click,map_clicks,bo)})
  
  # Add/Remove shoreline markers
  
  observeEvent(input$show_shorelines, {
    
    leafletProxy("map") %>% clearGroup('shore_markers')
    
    zoom = input$map_zoom
    zoom_threshold = 13
    bounds = input$map_bounds
    
    if (zoom < zoom_threshold) {
      
      showModal(modalDialog(tags$h4("Zoom to at least level 13 to show beach shorelines."),easyClose = FALSE))
      
    } else {
      
      if (!is.null(bounds)) {
        visible_data = data.frame(shoreline_data[shoreline_data$startlat >= bounds$south & shoreline_data$endlat <= bounds$north &
                                                   shoreline_data$startlong >= bounds$west & shoreline_data$endlong <= bounds$east,])
      }
      
      uniq_rows = data.frame(visible_data[!duplicated(visible_data[3:6]),])
      uniq_rows = uniq_rows[-nrow(uniq_rows),]
      
      if (nrow(uniq_rows)>0) {
        
        for (i in 1:nrow(uniq_rows)) {
          
          shoreline = data.frame(
            lon = c(uniq_rows[i,5],uniq_rows[i,6]),       
            lat =c(uniq_rows[i,3],uniq_rows[i,4])
          )
          name = as.character(uniq_rows[i,2])
          
          # 
          
          leafletProxy("map") %>% addPolylines(., lng=shoreline$lon, lat=shoreline$lat,color='#2c3e50',label = name, opacity=1.0,
                                               group="shore_markers",popup = paste("Beach ID: ", name)) %>%
            addCircleMarkers(lng=shoreline$lon[1],lat=shoreline$lat[1],radius=2,opacity=1.0, color="green", group="shore_markers") %>%
            addCircleMarkers(lng=shoreline$lon[2],lat=shoreline$lat[2],radius=2,opacity=1.0, color="green", group="shore_markers")
        }
      }
    }
  })
  
  observeEvent(input$clear_shorelines, {
    leafletProxy("map") %>% clearGroup('shore_markers')
  })
  
  # Add/Remove monitoring station markers
  
  observeEvent(input$show_stations, {
    
    leafletProxy("map") %>% clearGroup('monitor_stations')
    
    zoom = input$map_zoom
    zoom_threshold = 11
    bounds = input$map_bounds
    
    if (zoom < zoom_threshold) {
      
      showModal(modalDialog(tags$h4("Zoom to at least level 11 to show monitoring stations."),easyClose = FALSE))
      
    } else {
      
      if (!is.null(bounds)) {
        visible_data = data.frame(station_data[station_data$Latitude >= bounds$south & station_data$Latitude <= bounds$north &
                                                 station_data$Longitude >= bounds$west & station_data$Longitude <= bounds$east,])
      }
      
      uniq_rows = data.frame(visible_data[!duplicated(visible_data[2:3]),])
      uniq_rows[,2] = as.numeric(uniq_rows[,2])
      uniq_rows[,3] = as.numeric(uniq_rows[,3])
      
      if (nrow(uniq_rows)>0) {
        
        leafletProxy("map",data=uniq_rows) %>%
          {
            for (i in 1:nrow(uniq_rows)) {
              addMarkers(.,lng=uniq_rows[i,3],lat=uniq_rows[i,2],popup = as.character(uniq_rows[i,1]), label = as.character(uniq_rows[i,1]), group="monitor_stations")
            }
          }
      }
    }
  })
  
  observeEvent(input$clear_stations, {
    leafletProxy("map") %>% clearGroup('monitor_stations')
  })
  
  # Other map stuff
  
  observeEvent(input$map_marker_click, {
    
    marker = input$map_marker_click
    
    if (is.null(marker$group)) {
      return()
    } else if (marker$group=='monitor_stations') {
      
      showModal(modalDialog(title="WQX Data Retrieval Parameters",easyClose=F,card(
        fluidRow(column(6,dateInput("start_date", "Start Date:")),column(6,dateInput("end_date", "End Date:"))),
        fluidRow(column(12,selectInput("data_type",label = "Data Type",selected ="Phys/Chem", selectize=FALSE, choices = c("Phys/Chem","Station Info", "Microbial","Toxicity"))))),
        footer = modalButton("Close")))
    } else {
      return()
    }
  })
  
  output$bo_text = renderUI({
    HTML("To determine site orientation, click once anywhere on the shoreline, then again on another point on the shoreline. 
      A third click, <b>made in the water</b>, calculates/saves the site orientation. A fourth click clears the map, 
      whereby the process can be repeated.<br><br><i>Note: Any newly calculated orientation replaces the previous one.</i>")
  })
  
  output$zoom_level = renderText({
    input$map_zoom
  })
  
  output$beach_orient = renderText({bo()})
  
  # Create a feature correlation matrix
  
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
  
  # Create PCA dataset for later analysis
  
  observeEvent(input$pca_check, {
    
    data0 = current_data()
    data = data0[,-response_var()]
    feat_data = data[,-1]
    
    pca_axes_max = ncol(feat_data)
    
    updateNumericInput(session, "num_axes",
                       max = pca_axes_max
    )
    
    updateNumericInput(session, "num_axes_using",
                       value = input$num_axes,
                       max = input$num_axes
    )
    
    if (any(is.na(feat_data))) {
      
      showModal(modalDialog(paste("PCA does not allow missing feature values. You can either Impute these
              or Disable the rows with missing values."),footer = modalButton("Close")))
      
    } else {
      
      if (is.null(ignored_rows)) {
        feat_data = feat_data
      } else {
        feat_data = feat_data()[-ignored_rows,]
      }
      
      n_axes = input$num_axes
      
      # Run PCA on feature data
      pca_result = prcomp(feat_data, scale. = TRUE)
      pca_summary = summary(pca_result)
      
      PCA_data = data.frame(cbind(data0[,1],data0[,response_var()],pca_result$x[,1:n_axes]))
      colnames(PCA_data) = c(colnames(data0)[1],colnames(data0)[response_var()],paste0("PC",seq(1,n_axes)))
      
      PCA_dataset(PCA_data)
      
      pca_axes(colnames(PCA_dataset())[3:ncol(PCA_data)])
      pcax_being_used(pca_axes())
      
      PCA_coefficients = data.frame(round(pca_result$rotation[,1:n_axes],4))
      PCA_coefficients = cbind(Feature = rownames(PCA_coefficients), PCA_coefficients)
      
      PCA_summary_df = data.frame(rbind(round(pca_summary$importance[1,1:n_axes],3),pca_summary$importance[2,1:n_axes],pca_summary$importance[3,1:n_axes]))
      summary_rownames= c("Std. Dev.","Var Explained","Cum Var Explained")
      PCA_summary_df = cbind(summary_rownames,PCA_summary_df)
      colnames(PCA_summary_df)[1] = "Metric"
      
      output$PCA_coeffs = DT::renderDataTable(server = T, {data = datatable(PCA_coefficients,rownames = F,selection =
                    list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                    list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                    className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$PCA_summary = DT::renderDataTable(server = T, {data = datatable(PCA_summary_df,rownames = F,selection =
                    list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                    list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                    className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      renderPCAdata(PCA_dataset(),date_format_string,ignored_rows,output)
      
      iv$add_rule("num_axes_using", sv_between(2, input$num_axes))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = "PCA Results")
    }
  })
  
  # Provide dataset cell value editing
  
  observeEvent(input$data_cell_edit, ignoreInit = T,ignoreNULL = T, {
    
    temp_data=current_data()
    
    info = input$data_cell_edit
    i = info$row
    j = info$col + 1
    
    temp_data = editData(temp_data, input$data_cell_edit, "data", rownames = FALSE)
    
    current_data(temp_data)
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
  })
  
  # Restore the original dataset
  
  observeEvent(input$restore, {
    
    response_var(2)
    
    temp_data = init_data[,-1]
    temp_data = temp_data[,-1]
    
    feat_names(colnames(temp_data))
    feats_being_used(colnames(temp_data))
    pca_axes(NULL)
    pcax_being_used(NULL)
    col_names(colnames(init_data))
    current_data(init_data)
    PCA_dataset(NULL)
    ignored_rows <<- NULL
    feat_props <<- init_feat_props
    
    #updateSelectInput(session,"id",choices=c(col_names()))
    updateSelectInput(session,"speed",selected="-")
    updateSelectInput(session,"direct",selected="-")
    updateSelectInput(session,"select_choice",selected="Features")
    updateSelectInput(session,"col_props",selected='-', choices=c("-",col_names()))
    updateSelectInput(session,"rainplot",selected='-', choices=c("-",col_names()))
    updateSelectInput(session,"lineplot",selected='-', choices=c("-",col_names()))
    updateSelectInput(session,"scatterx",selected="-",choices=c("-",col_names()))
    updateSelectInput(session,"scattery",selected="-",choices=c("-",col_names()))
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
    renderPCAdata(PCA_dataset(),date_format_string,ignored_rows,output)
    
    output$PCA_coeffs = NULL
    output$PCA_summary = NULL
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Data Table")
    
  })
  
  # Upload excel/csv data file
  
  observeEvent(input$file1, ignoreInit = T, {
    
    ext = tools::file_ext(input$file1$name)
    
    if (ext == "xlsx") {
      init_data <<- read.xlsx(input$file1$datapath)
    } else {
      init_data <<- read.csv(input$file1$datapath,header = TRUE,sep = input$sep)
    }
    
    if (any(duplicated(init_data[,1]))) {
      showModal(modalDialog(paste("The ID (Column 1) is required to have unique values, but duplicate values were found. Please remedy prior to data importation."),
                            easyClose = F,footer = div(modalButton('Close'))))
    } else if (any(is.na(init_data[,1]))) {
      
      showModal(modalDialog(paste("The ID (Column 1) has missing values. Please remedy prior to data importation."),
                            easyClose = F,footer = div(modalButton('Close'))))
      
    } else {
      feat_props_temp = hash()
      
      for (i in 1:ncol(init_data)) {
        .set(feat_props_temp,keys=colnames(init_data)[i],values=c(prop1=2,prop2=NA,prop3=NA,prop4=NA))
      }
      
      init_feat_props <<- feat_props_temp
      feat_props <<- feat_props_temp
      ignored_rows <<- NULL
      PCA_dataset(NULL)
      
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
      
      updateNumericInput(session, "LG_binarize_crit_value",value = round(median(current_data()[,response_var()]),2),
                         min=min(current_data()[,response_var()]),max=max(current_data()[,response_var()]))
      updateNumericInput(session, "XGBCL_binarize_crit_value",value = round(median(current_data()[,response_var()]),2),
                         min=min(current_data()[,response_var()]),max=max(current_data()[,response_var()]))
      
      iv$add_rule("LG_binarize_crit_value", sv_between(min(init_data[,response_var()]),max(init_data[,response_var()])))
      iv$add_rule("XGBCL_binarize_crit_value", sv_between(min(init_data[,response_var()]),max(init_data[,response_var()])))
      
      feat_data0 = init_data[,-1]
      feat_data = feat_data0[,-1]
      
      feats_being_used(colnames(feat_data))
      feat_names(colnames(feat_data))
      
      enable("restore")
      enable("col_props")
      enable("impute_check")
      enable("corr_check")
      enable("pca_check")
      enable("run_iso_forest")
      
      pca_axes_max = ncol(init_data)-2
      
      updateNumericInput(session, "num_axes",
                         value = pca_axes_max,
                         max = pca_axes_max
      )
      
      updateNumericInput(session, "num_axes_using",
                         value = input$num_axes,
                         max = input$num_axes
      )
      
      iv$add_rule("num_axes", sv_between(2, pca_axes_max))
      
      updateSelectInput(session,"col_props",choices=c("-",col_names()))
      updateSelectInput(session,"rainplot",choices=c("-",col_names()))
      updateSelectInput(session,"lineplot",choices=c("-",col_names()))
      updateSelectInput(session,"scatterx",choices=c("-",col_names()))
      updateSelectInput(session,"scattery",choices=c("-",col_names()))
      updateSelectInput(session,"speed",choices=c("-",col_names()))
      updateSelectInput(session,"direct",choices=c("-",col_names()))
      
      renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
      renderPCAdata(PCA_dataset(),date_format_string,ignored_rows,output)
      
      output$PCA_coeffs = NULL
      output$PCA_summary = NULL
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = "Data Table")

    }
  })
  
  # Input column properties into the hash table
  
  observeEvent(input$col_props, ignoreInit = T,  {
    
    if (input$col_props != "-") {
      
      showModal(modalDialog(title=paste0(input$col_props,"Column Properties"),card(
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
  
  # Change the response variable
  
  observeEvent(input$data_columns_selected, ignoreInit = T, {
    
    if ((input$data_columns_selected+1) != response_var()) {
      response_var(input$data_columns_selected + 1)
      renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
      
      pca_axes(NULL)
      pcax_being_used(NULL)
      PCA_dataset(NULL)
      renderPCAdata(PCA_dataset(),date_format_string,ignored_rows,output)
      output$PCA_coeffs = NULL
      output$PCA_summary = NULL
    }
  })
  
  # Isolation Forest analysis for outlier detection
  
  observeEvent(input$run_iso_forest, {
    
    req(iv$is_valid())
    
    # if (is.null(ignored_rows)) {
    #   iso_data0 = current_data()
    #   iso_data = current_data()[,-c(id_var,response_var())]
    # } else {
    #   iso_data0 = current_data()[-ignored_rows,]
    #   iso_data = iso_data0[,-c(id_var,response_var())]
    # }
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    iso_data = data[,-c(id_var,rv)]
    
    samp_size = min(nrow(iso_data), 10000L)
    ndim = input$iso_ndim
    
    iso_results = matrix(NA, nrow = nrow(iso_data), ncol = 6)
    iso_results = data.frame(iso_results)
    colnames(iso_results) = c("ID","Depth_Score","Adj_Depth_Score","Density_Score","Adj_Density_Score","Overall")
    
    iso_results[,1] = data[,1]
    
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
                                                                                                                      selected = list(rows = NULL),target = "row"),editable = F,extensions="Buttons",options = list(paging = TRUE,dom="ltBp",
                                                                                                                                                                                                                    buttons = c('copy', 'csv', 'excel'),pageLength = 20,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                                                                                                                                                                                                                    initComplete =JS("function(settings, json) {","$(this.api().table().header()).css({'background-color':'#073744', 'color': '#fff'});","}")))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Outlier Metric')
    
  })
  
  # Selection of dataset rows for enabling/disabling
  
  observeEvent(input$select_choice, ignoreInit = T, {
    
    if (input$select_choice == "Rows") {
      enable("ignore_rows")
      enable("enable_rows")
    } else if (input$select_choice == "Features") {
      disable("ignore_rows")
      disable("enable_rows")
    }
    
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
  
  # Plotting functions
  
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
  
  # Feature imputation
  
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
  
  # Create wind/wave/current A/O components
  
  observeEvent(input$create, {
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Data Table')
    
    if (input$speed != "-" & input$direct != "-") {
      
      if (!(input$A_name %in% col_names()) & !(input$O_name %in% col_names())) {
        
        pca_axes(NULL)
        pcax_being_used(NULL)
        PCA_dataset(NULL)
        
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
        
        PCA_dataset(NULL)
        renderPCAdata(PCA_dataset(),date_format_string,ignored_rows,output)
        renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,feat_props,ignored_rows,output)
        
      } else {
        showModal(modalDialog(div("ERROR: BOTH new component columns must have different names 
                                  than any currently existing column names.",style="font-size:160%"),easyClose = T))
      }
      
    } else {
      showModal(modalDialog(div("ERROR: A speed and direction data column must be specified.",style="font-size:160%"),easyClose = T))
    }
  })
  
  # Toggle between initial feature and PCA data use
  
  observeEvent(input$use_pca_data, {
    
    if (input$use_pca_data) {

      shinyjs::disable("feats_to_use")
      shinyjs::enable("pcax_to_use")
      
    } else {
      
      shinyjs::enable("feats_to_use")
      shinyjs::disable("pcax_to_use")
    }
  })
  
  # Perform certain book-keeping functions when the "Modeling" tab is selected
  
  observeEvent(input$shinyVB, {
    
    if (input$shinyVB == "Modeling") {
      
      if (is.null(current_data())) {
        return()
        
      } else {
        
        temp_data = current_data()
        temp_data1 = temp_data[,-response_var()]
        temp_data2 = temp_data1[,-1]
        
        feat_names(colnames(temp_data2))
        
        updateCheckboxGroupInput(session,"feats_to_use",choices=feat_names(),selected=feats_being_used(),inline=T)
        
        if (input$use_pca_data) {
          delay(1,disable("feats_to_use"))
        }
        
        updateNumericInput(session, "LG_binarize_crit_value",value = round(median(current_data()[,response_var()]),2),
                           min=min(current_data()[,response_var()]),max=max(current_data()[,response_var()]))
        updateNumericInput(session, "XGBCL_binarize_crit_value",value = round(median(current_data()[,response_var()]),2),
                           min=min(current_data()[,response_var()]),max=max(current_data()[,response_var()]))
      }
      
      if (is.null(PCA_dataset())) {
        return()
        
      } else {
        
        tmp_data = PCA_dataset()
        tmp_data1 = tmp_data[,-1]
        tmp_data2 = tmp_data1[,-1]
        
        pca_axes(colnames(tmp_data2))
        
        updateCheckboxGroupInput(session,"pcax_to_use",choices=pca_axes(),selected=pcax_being_used(),inline=T)
        
        if (!input$use_pca_data) {
          delay(1,disable("pcax_to_use"))
        }
      }
      
    } else {
      return()
    }
  })
  
  observeEvent(input$feats_to_use, {
      feats_being_used(input$feats_to_use)
  })
  
  observeEvent(input$pcax_to_use, {
    pcax_being_used(input$pcax_to_use)
  })
  
  # Logistic regression predictions
  
  observeEvent(input$LG_pred_dc, {
    
    if (nrow(LG_pred_scat_dat) != 0) {
      
      output$LG_pred_scatplot = renderPlot(ggplot(LG_pred_scat_dat, aes(x=LG_pred_scat_dat[,3], fill=as.factor(LG_pred_scat_dat[,2]))) +
                    geom_density(alpha = 0.6, color = "black", size = 0.5) +
                    scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                    geom_vline(xintercept = input$LG_pred_dc, linetype = "dashed", color = "darkgreen") +
                    labs(x = "Predicted Probability", y = "Density", fill="OBS") +
                    theme_bw() +
                    theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                    theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                    theme(legend.position = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      confuse_results = confuse(LG_pred_scat_dat[,2:3],0.5,input$LG_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$LG_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$run_pred_LG, {
    
    req(iv$is_valid())
    
    set.seed(input$model_seed)
    
    if (input$use_pca_data) {
      data0 = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data0 = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
  
    MC_runs = input$MC_runs
    
    crit_value = input$LG_binarize_crit_value
    
    if (input$LG_binarize) {
      new_Y = ifelse(test = data0[,rv] >= crit_value, yes = 1, no = 0)
      data0[,rv] = new_Y
    }
    
    if (is.null(ignored_rows)) {
      data1 = data0
    } else {
      data1 = data0[-ignored_rows,]
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    data1 = data1[!is.na(data1[,rv]),]
    
    var_list = c(1,rv,which(colnames(data1) %in% feats_to_use))
    data2 = data1[,var_list]
    colnames(data2) = c(colnames(data0)[1],"Response",feats_to_use)
    
    # RANDOMIZE DATA
    if (input$randomize==TRUE) {
      random_index = sample(1:nrow(data2), nrow(data2))
      data2 = data2[random_index, ]
    }
    
    data = data2[,-1]
    
    # if (any(is.na(data[,-1]))) {
    # 
    #   showModal(modalDialog(paste("Logistic Regression does not tolerate missing feature values. You can either Impute these
    #             (on the Data tab) or Disable rows/columns with missing values."),footer = modalButton("Close")))
    # 
    # } else {
      
      #Create n folds
      tot_folds = input$num_folds
      folds = cut(seq(1, nrow(data)), breaks = tot_folds, labels = FALSE)
      
      fold_predictions = matrix(0, nrow = 0, ncol = 2)
      fold_predictions = as.data.frame(fold_predictions)
      
      coeff_folds = matrix(0, nrow = ncol(data), ncol = tot_folds+1)
      coeff_folds = as.data.frame(coeff_folds)
      coeff_folds[,1] = c("(Intercept)",feats_to_use)
      
      #Perform cross validation
      for (f in 1:tot_folds) {
        
        testIndices = which(folds == f, arr.ind = TRUE)
        testData = data[testIndices, ]
        trainData = data[-testIndices, ]
        
        temp_preds = matrix(0, nrow = nrow(testData), ncol = 2*MC_runs)
        temp_preds = data.frame(temp_preds)
        
        temp_coeffs = matrix(0, nrow = ncol(data), ncol = MC_runs+1)
        temp_coeffs = data.frame(temp_coeffs)
        temp_coeffs[,1] = c("(Intercept)",feats_to_use)
        
        withProgress(
          message = 'Logistic Prediction Progress',
          detail = paste("MC runs:", x=1,"; Fold:",y = f),
          value = (1-1/tot_folds) - (1/tot_folds)*(tot_folds-f),
          {
            
            for (i in 1:MC_runs) {
              
              incProgress(1/(MC_runs*tot_folds), detail = paste("MC run:",i,"/",MC_runs,"; Fold:",f,"/",tot_folds))
              
              # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS in test data and train data
              if (input$loggy == TRUE) {
                
                for (j in 1:nrow(trainData)) {
                  if (trainData[j, 1] == "TNTC") {
                    trainData[j, 1] = log10(runif(1, min = input$rc_lowval, max = input$rc_upval))
                    ifelse(test = trainData[j, 1] >= crit_value, yes = 1, no = 0)
                  }
                  
                  if (trainData[j, 1] == "ND") {
                    trainData[j, 1] = log10(runif(1, min = input$lc_lowval, max = input$lc_upval))
                    ifelse(test = trainData[j, 1] >= crit_value, yes = 1, no = 0)
                  }
                }
                
                for (j in 1:nrow(testData)) {
                  if (testData[j, 1] == "TNTC") {
                    testData[j, 1] = log10(runif(1, min = input$rc_lowval, max = input$rc_upval))
                    ifelse(test = testData[j, 1] >= crit_value, yes = 1, no = 0)
                  }
                  
                  if (testData[j, 1] == "ND") {
                    testData[j, 1] = log10(runif(1, min = input$lc_lowval, max = input$lc_upval))
                    ifelse(test = testData[j, 1] >= crit_value, yes = 1, no = 0)
                  }
                }
              } else {
                for (j in 1:nrow(trainData)) {
                  if (trainData[j, 1] == "TNTC") {
                    trainData[j, 1] = (runif(1, min = input$rc_lowval, max = input$rc_upval))
                    ifelse(test = trainData[j, 1] >= crit_value, yes = 1, no = 0)
                  }
                  
                  if (trainData[j, 1] == "ND") {
                    trainData[j, 1] = (runif(1, min = input$lc_lowval, max = input$lc_upval))
                    ifelse(test = trainData[j, 1] >= crit_value, yes = 1, no = 0)
                  }
                }
                for (j in 1:nrow(testData)) {
                  
                  if (testData[j, 1] == "TNTC") {
                    testData[j, 1] = (runif(1, min = input$rc_lowval, max = input$rc_upval))
                    ifelse(test = testData[j, 1] >= crit_value, yes = 1, no = 0)
                  }
                  
                  if (testData[j, 1] == "ND") {
                    testData[j, 1] = (runif(1, min = input$lc_lowval, max = input$lc_upval))
                    ifelse(test = testData[j, 1] >= crit_value, yes = 1, no = 0)
                  }
                }
              }
              
              temp_preds[,2*i-1] = testData[,1]
              
              # Train the model
              fit_mod = cva.glmnet(x=as.matrix(trainData[,-1]),y=trainData[,1],nfolds=tot_folds,family="binomial",type.measure=input$LG_eval,na.action="na.omit",
                                   standardize=input$LG_standard,intercept=TRUE)
              
              get_model_params = function(fit) {
                alpha = fit$alpha
                lambdaMin = sapply(fit$modlist, `[[`, "lambda.min")
                lambdaSE = sapply(fit$modlist, `[[`, "lambda.1se")
                error = sapply(fit$modlist, function(mod) {min(mod$cvm)})
                best = which.min(error)
                data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
                           lambdaSE = lambdaSE[best], eror = error[best])
              }
              
              alpha = get_model_params(fit_mod)$alpha
              lambda = get_model_params(fit_mod)$lambdaMin
              
              model = glmnet(x=as.matrix(trainData[,-1]),trainData[,1],lambda=lambda, alpha=alpha,na.action="na.omit", family="binomial",type.measure=input$LG_eval,
                             standardize=input$LG_standard,intercept=TRUE)
              
              preds = predict(model, newx = as.matrix(testData[,-1]), type = "response")
              
              tmp_coeffs = coef(model, s = lambda, exact = TRUE)
              coeffs = data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = round(tmp_coeffs@x,4))
              
              for (h in 1:nrow(coeffs)) {
                if(temp_coeffs[h,1] %in% coeffs[,1]) {
                  temp_coeffs[h,i+1] = coeffs[which(coeffs[,1] == temp_coeffs[h,1]),2]
                } else {
                  temp_coeffs[h,i+1] = 0
                }
              }
              
              temp_preds[,2*i] = round(preds,3)
              
            } #End the MC Runs
            
            coeff_folds[,f+1] = round(rowMeans(temp_coeffs[,-1]),5)
            
            even_columns = temp_preds[,seq(2, ncol(temp_preds), by = 2)]
            odd_columns = temp_preds[,seq(1, ncol(temp_preds), by = 2)]
            
            obs_mean_values = ifelse(test = rowMeans(odd_columns) >= 0.5, yes = 1, no = 0)
            pred_mean_values = round(rowMeans(even_columns),3)
            fold_preds = cbind(obs_mean_values,pred_mean_values)
            
            fold_predictions = rbind(fold_predictions,fold_preds)
          })
        
      } #End the Fold runs
    
      if (input$use_pca_data) {
        prediction_results = data.frame(cbind(data2[,1],fold_predictions[,1],fold_predictions[,2],round(data[,-1],4)))
      } else {
        prediction_results = data.frame(cbind(data2[,1],fold_predictions[,1],fold_predictions[,2],data[,-1]))
      }
      
      colnames(prediction_results) = c(colnames(data2)[1],colnames(data0)[rv],"Predictions",feats_to_use)
      
      prediction_results = prediction_results[order(prediction_results[,1]),]
      
      final_coeffs = data.frame(cbind(coeff_folds[,1],round(rowMeans(coeff_folds[,-1]),4),round(exp(rowMeans(coeff_folds[,-1])),4)))
      colnames(final_coeffs) = c("Feature","Coefficient","Odds Ratio")
      
      output$LG_preds = DT::renderDataTable(server = T, {data = datatable(prediction_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp",buttons = c('copy', 'csv', 'excel'),paging = T,
                  pageLength = 17,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_pred_coeffs = DT::renderDataTable(server = T, {data = datatable(final_coeffs,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                       target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),
                       paging = F,scrollX = F,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      LG_pred_scat_dat <<- prediction_results[,1:3]
      
      x_name = colnames(LG_pred_scat_dat)[[2]]
      y_name = colnames(LG_pred_scat_dat)[[3]]
      
      output$LG_pred_scatplot = renderPlot(ggplot(LG_pred_scat_dat, aes(x=LG_pred_scat_dat[,3], fill=as.factor(LG_pred_scat_dat[,2]))) +
        geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
        scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
        geom_vline(xintercept = input$LG_pred_dc, linetype = "dashed", color = "darkgreen") +
        labs(x = "Predicted Probability", y = "Density", fill="OBS") +
        theme_bw() +
        theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
        theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
        theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      confuse_results = confuse(LG_pred_scat_dat[,2:3],0.5,input$LG_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$LG_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                        round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'LG: Predict')
  })
  
  # Logistic regression fitting
  
  observeEvent(input$LG_fit_dc, {
    
    if (nrow(LG_scat_dat) != 0) {
      
      output$LG_scatplot = renderPlot(ggplot(LG_scat_dat, aes(x=LG_scat_dat[,3], fill=as.factor(LG_scat_dat[,2]))) +
                      geom_density(alpha = 0.6, color = "black", size = 0.5) +
                      scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                      geom_vline(xintercept = input$LG_fit_dc, linetype = "dashed", color = "darkgreen") +
                      labs(x = "Fitted Probability", y = "Density", fill="OBS") +
                      theme_bw() +
                      theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                      theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                      theme(legend.position = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      confuse_results = confuse(LG_scat_dat[,2:3],0.5,input$LG_fit_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$LG_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$run_fitted_LG, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data0 = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data0 = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    set.seed(input$model_seed)
    
    crit_value = input$LG_binarize_crit_value
    
    if (input$LG_binarize) {
      new_Y = ifelse(test = data0[,rv] >= crit_value, yes = 1, no = 0)
      data0[,rv] = new_Y
    }
    
    if (is.null(ignored_rows)) {
      data1 = data0
    } else {
      data1 = data0[-ignored_rows,]
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    data1 = data1[!is.na(data1[,rv]),]
    
    var_list = c(1,rv,which(colnames(data1) %in% feats_to_use))
    data2 = data1[,var_list]
    colnames(data2) = c(colnames(data0)[1],"Response",feats_to_use)
    
    # RANDOMIZE DATA
    if (input$randomize==TRUE) {
      random_index = sample(1:nrow(data2), nrow(data2))
      data2 = data2[random_index, ]
    }
    
    data = data2[,-1]
    
    MC_runs = input$MC_runs
    
    # if (any(is.na(data[,-1]))) {
    # 
    #   showModal(modalDialog(paste("Logistic Regression does not tolerate missing feature values. You can either Impute these
    #             (on the Data tab) or Disable rows/columns with missing values."),footer = modalButton("Close")))
    # 
    # } else {
    
    fitted_coeffs = matrix(0, nrow = ncol(data), ncol = 3)
    fitted_coeffs = data.frame(fitted_coeffs)
    fitted_coeffs[,1] = c("(Intercept)",feats_to_use)
    
    fitted_values = matrix(0, nrow = 0, ncol = 2)
    fitted_values = data.frame(fitted_values)
    
    temp_fits = matrix(0, nrow = nrow(data), ncol = 2*MC_runs)
    temp_fits = data.frame(temp_fits)
    
    temp_coeffs = matrix(0, nrow = ncol(data), ncol = MC_runs+1)
    temp_coeffs = data.frame(temp_coeffs)
    temp_coeffs[,1] = c("(Intercept)",feats_to_use)
    
    withProgress(
      message = 'Logistic Fitting Progress',
      detail = paste("MC runs:", x=1),
      value = 1/MC_runs,
      {
        
        for (i in 1:MC_runs) {
          
          # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS in test data and train data
          if (input$loggy == TRUE) {
            for (j in 1:nrow(data)) {
              if (data[j, 1] == "TNTC") {
                data[j, 1] = log10(runif(1, min = input$rc_lowval, max = input$rc_upval))
                ifelse(test = data[j, 1] >= crit_value, yes = 1, no = 0)
              }
              
              if (data[j, 1] == "ND") {
                data[j, 1] = log10(runif(1, min = input$lc_lowval, max = input$lc_upval))
                ifelse(test = data[j, 1] >= crit_value, yes = 1, no = 0)
              }
            }
          } else {
            for (j in 1:nrow(data)) {
              if (data[j, 1] == "TNTC") {
                data[j, 1] = (runif(1, min = input$rc_lowval, max = input$rc_upval))
                ifelse(test = data[j, 1] >= crit_value, yes = 1, no = 0)
              }
              
              if (data[j, 1] == "ND") {
                data[j, 1] = (runif(1, min = input$lc_lowval, max = input$lc_upval))
                ifelse(test = data[j, 1] >= crit_value, yes = 1, no = 0)
              }
            }
          }
          
          temp_fits[,2*i-1] = data[,1]
          
          # determine best alpha and lambda
          fit_mod = cva.glmnet(x=as.matrix(data[,-1]),y=data[,1],nfolds=input$num_folds,family="binomial",type.measure=input$LG_eval,na.action="na.omit",
                      standardize=input$LG_standard,intercept=TRUE)
          
          get_model_params = function(fit) {
            alpha = fit$alpha
            lambdaMin = sapply(fit$modlist, `[[`, "lambda.min")
            lambdaSE = sapply(fit$modlist, `[[`, "lambda.1se")
            error = sapply(fit$modlist, function(mod) {min(mod$cvm)})
            best = which.min(error)
            data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
                       lambdaSE = lambdaSE[best], eror = error[best])
          }
          
          alpha = get_model_params(fit_mod)$alpha
          lambda = get_model_params(fit_mod)$lambdaMin
          
          # fit final model
          model = glmnet(x=as.matrix(data[,-1]),data[,1],lambda=lambda, alpha=alpha, na.action="na.omit",family="binomial",type.measure=input$LG_eval,
                      standardize=input$LG_standard,intercept=TRUE)
          
          fits = predict(model, newx = as.matrix(data[,-1]), type = "response")
          temp_fits[,2*i] = round(fits,3)
          
          tmp_coeffs = coef(model, s = lambda, exact = TRUE)
          coeffs = data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = round(tmp_coeffs@x,4))
          
          for (h in 1:nrow(coeffs)) {
            if(temp_coeffs[h,1] %in% coeffs[,1]) {
              temp_coeffs[h,i+1] = coeffs[which(coeffs[,1] == temp_coeffs[h,1]),2]
            } else {
              temp_coeffs[h,i+1] = 0
            }
          }
          
          incProgress(1/MC_runs, detail = paste("MC run:",i,"/",MC_runs))
        } #End the MC Runs
      })
    
    # Must add: fit final/global LG_Model
    
    fitted_coeffs[,2] = round(rowMeans(temp_coeffs[,-1]),4)
    fitted_coeffs[,3] = round(exp(fitted_coeffs[,2]),4)
    
    even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
    odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
    
    obs_mean_values = ifelse(test = rowMeans(odd_columns) >= 0.5, yes = 1, no = 0)
    fit_mean_values = round(rowMeans(even_columns),3)
    model_results = cbind(obs_mean_values,fit_mean_values)
    
    fitted_model_results = data.frame(cbind(data2[,1],model_results[,1],model_results[,2],data[,-1]))
    colnames(fitted_model_results) = c(colnames(data2)[1],colnames(data0)[rv],"Fitted_Prob",feats_to_use)
    
    fitted_model_results = fitted_model_results[order(fitted_model_results[,1]),]
    
    colnames(fitted_coeffs) = c("Feature","Coefficient","Odds Ratio")
    
    output$LG_fits = DT::renderDataTable(server = T, {data = datatable(fitted_model_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
              target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp",buttons = c('copy', 'csv', 'excel'),paging = T,
              pageLength = 17,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
              JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$LG_coeffs = DT::renderDataTable(server = T, {data = datatable(fitted_coeffs,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
              target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),
              paging = F,scrollX = F,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
              initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    LG_scat_dat <<- fitted_model_results[,1:3]
    
    x_name = colnames(LG_scat_dat)[[2]]
    y_name = colnames(LG_scat_dat)[[3]]
    
    output$LG_scatplot = renderPlot(ggplot(LG_scat_dat, aes(x=LG_scat_dat[,3], fill=as.factor(LG_scat_dat[,2]))) +
                            geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
                            scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                            geom_vline(xintercept = input$LG_fit_dc, linetype = "dashed", color = "darkgreen") +
                            labs(x = "Fitted Probability", y = "Density", fill="OBS") +
                            theme_bw() +
                            theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                            theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                            theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
    
    confuse_results = confuse(LG_scat_dat[,2:3],0.5,input$LG_fit_dc)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$LG_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$LG_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                  round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'LG: Fitting')
  })
  
  # HP optimization of the XGB Classifier
  
  observeEvent(input$XGBCL_optimize_HP, {
    
    showModal(modalDialog(title="HP Optimization", card(
      fluidRow(
        column(4,numericInput("psocl_max_iter", "Max Iterations", min=5, max=1000, value=20, step = 1)),
        column(2),
        column(4,numericInput("psocl_swarm_size", "Swarm Size", min=3, max=200, value=10, step = 1))),
      fluidRow(
        column(4,numericInput("membercl_exp", "Membership Weight", min=0.25, max=3, value=0.5, step = 0.25)),
        column(2),
        column(4,numericInput("sscl_exp", "Sum of Squares Weight", min=0.25, max=3, value=1, step = 0.25)))),
      footer = div(actionButton("run_XGBCL_optimize_HP", "Run"),modalButton('Close'))))
  })
  
  observeEvent(input$run_XGBCL_optimize_HP, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    xgbcl_optim_HP_results = xgbcl_call_optimize_HP(data,rv,id_var,input$model_seed,ignored_rows,feats_to_use,input$lc_lowval,
                                                input$lc_upval,input$rc_lowval,input$rc_upval,input$MC_runs,input$num_folds,input$loggy,input$randomize,
                                                input$XGBCL_standard,input$XGBCL_eval,input$psocl_max_iter,input$psocl_swarm_size,input$membercl_exp,
                                                input$sscl_exp,input$XGBCL_binarize,input$XGBCL_binarize_crit_value)
    
    xgbcl_optim_HP_results1 = data.frame(xgbcl_optim_HP_results)
    
    Optimal_CLHP$max_depth <<- round(xgbcl_optim_HP_results1[1,1],0)
    Optimal_CLHP$eta <<- round(xgbcl_optim_HP_results1[2,1],3)
    Optimal_CLHP$subsample <<- round(xgbcl_optim_HP_results1[3,1],2)
    Optimal_CLHP$colsample_bytree <<- round(xgbcl_optim_HP_results1[4,1],2)
    Optimal_CLHP$min_child_weight <<- round(xgbcl_optim_HP_results1[5,1],0)
    Optimal_CLHP$gamma <<- round(xgbcl_optim_HP_results1[6,1],1)
    Optimal_CLHP$nrounds <<- round(xgbcl_optim_HP_results1[7,1],0)
    
    output$XGBCL_optim_hp = DT::renderDataTable(server=T,{data = datatable(Optimal_CLHP,rownames=F,extensions='Buttons',selection=list(selected =
                    list(rows = NULL, cols = NULL),target = "row",mode="single"),editable=F,options = list(autoWidth=F,dom='tB',paging = F,pageLength = 5,scrollX = F,
                    scrollY = F,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                    initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGBCL: HP Optim')
  })
  
  # Set HP values for the XGB Classifier
  
  observeEvent(input$XGBCL_params, {
    
    showModal(modalDialog(title="XGBCL Hyperparameters",easyClose=F,card(
      fluidRow(
        column(4,numericInput("etacl", label="Eta", value = etacl_set(),min=0,max=1,step=0.01)),
        column(4,numericInput("gammacl", label="Gamma", value = gammacl_set(), min=0, max=20, step = 1)),
        column(4,numericInput("nroundscl", label="# Rounds", value = nroundscl_set(), min=100, max=3000, step = 25))),
      fluidRow(
        column(6,numericInput("max_depthcl", label="Max Tree Depth", value = max_depthcl_set(), min=1,max=10, step=1)),
        column(6,numericInput("min_child_weightcl", label="Min Leaf Size", value = min_child_weightcl_set(), min=1,max=20,step=1))),
      fluidRow(
        column(6,numericInput("subsampcl", label="Subsample Proportion", value = subsampcl_set(), min=0,max=1, step=0.01)),
        column(6,numericInput("colsampcl", label="Column Sample Proportion", value = colsampcl_set(), min=0,max=1,step=0.01))),
      fluidRow(
        column(4,selectInput("XGBCL_tree_method",label = "Tree Method",selected =xgbcl_tree_method_set(),choices = c("hist","exact","approx"))),
        column(4,selectInput("XGBCL_booster",label = "Booster",selected =xgbcl_booster_set(),choices = c("gbtree","gblinear","dart")))),
      fluidRow(column(12,tags$h5("CAUTION: DART booster + Feature Selection = long computational times!"))),
      fluidRow(
        column(6, selectInput("dartcl_normalize_type",label = "Normalization Type",selected =dartcl_normalize_type_set(),choices = c("tree","forest"))),
        column(6, selectInput("dartcl_sample_type",label = "Sample Algorithm",selected =dartcl_sample_type_set(),choices = c("uniform","weighted")))),
      fluidRow(
        column(6,numericInput("ratecl_drop", label="Drop Rate", value = ratecl_drop_set(), min=0,max=1,step=0.01)),
        column(6,numericInput("skipcl_drop", label="Skip Prob", value = skipcl_drop_set(), min=0,max=1,step=0.01)))),
      footer = div(actionButton("save_XGBCL_hp_settings",label='Save Settings'),modalButton("Close"))))
    
    if (xgbcl_booster_set() == "dart") {
      shinyjs::enable("dartcl_normalize_type")
      shinyjs::enable("dartcl_sample_type")
      shinyjs::enable("ratecl_drop")
      shinyjs::enable("skipcl_drop")
    } else {
      shinyjs::disable("dartcl_normalize_type")
      shinyjs::disable("dartcl_sample_type")
      shinyjs::disable("ratecl_drop")
      shinyjs::disable("skipcl_drop")
    }
  })
  
  observeEvent(input$save_XGBCL_hp_settings, ignoreInit = T, {
    
    etacl_set(input$etacl)
    gammacl_set(input$gammacl)
    max_depthcl_set(input$max_depthcl)
    min_child_weightcl_set(input$min_child_weightcl)
    nroundscl_set(input$nroundscl)
    subsampcl_set(input$subsampcl)
    colsampcl_set(input$colsampcl)
    
    xgbcl_tree_method_set(input$XGBCL_tree_method)
    xgbcl_booster_set(input$XGBCL_booster)
    
    dartcl_normalize_type_set(input$dartcl_normalize_type)
    dartcl_sample_type_set(input$dartcl_sample_type)
    ratecl_drop_set(input$ratecl_drop)
    skipcl_drop_set(input$skipcl_drop)
    
    Optimal_HPCL <<- data.frame(max_depth = input$max_depthcl,eta = input$etacl,subsample = input$subsampcl,colsample_bytree = input$colsampcl,
                              min_child_weight = input$min_child_weightcl,gamma = input$gammacl,nrounds = input$nroundscl)
    
    removeModal()
    
  })
  
  observeEvent(input$XGBCL_booster, {
    if (input$XGBCL_booster == "dart") {
      shinyjs::enable("dartcl_normalize_type")
      shinyjs::enable("dartcl_sample_type")
      shinyjs::enable("ratecl_drop")
      shinyjs::enable("skipcl_drop")
    } else {
      shinyjs::disable("dartcl_normalize_type")
      shinyjs::disable("dartcl_sample_type")
      shinyjs::disable("ratecl_drop")
      shinyjs::disable("skipcl_drop")
    }
  })
  
  # XGBCL feature selection
  
  observeEvent(input$run_XGBCL_select, {
    
    if(running())
      return(NULL)
    running(TRUE)
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
      pcax_being_used(feats_to_use)
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
      feats_being_used(feats_to_use)
    }
    
    crit_value = input$XGBCL_binarize_crit_value
    eval_metric = input$XGBCL_eval
    
    if (input$XGBCL_binarize) {
      new_Y = ifelse(test = data[,rv] >= crit_value, yes = 1, no = 0)
      data[,rv] = new_Y
    }
    
    if (is.null(ignored_rows)) {
      xgbcl_select_data = data
    } else {
      xgbcl_select_data = data[-ignored_rows,]
    }
    
    eta = etacl_set()
    gamma = gammacl_set()
    max_depth = max_depthcl_set()
    min_child_weight = min_child_weightcl_set()
    nrounds = nroundscl_set()
    early_stop = early_stop_set()
    subsamp = subsampcl_set()
    colsamp = colsampcl_set()
    
    xgb_tree_method = xgbcl_tree_method_set()
    xgb_boost = xgbcl_booster_set()
    dart_normalize_type = dartcl_normalize_type_set()
    dart_sample_type = dartcl_sample_type_set()
    rate_drop = ratecl_drop_set()
    skip_drop = skipcl_drop_set()
    
    xgb_standardize = input$XGBCL_standard
    lc_lowval = input$lc_lowval
    lc_upval = input$lc_upval
    rc_lowval = input$rc_lowval
    rc_upval = input$rc_upval
    train_prop = input$train_pct/100
    MC_runs = input$MC_runs
    loggy = input$loggy
    randomize = input$randomize
    seed = input$model_seed
    
    xgbcl_select_result(NULL)

    xgbcl_select_calculation <<- future({

      xgbcl_selection(xgbcl_select_data,seed,rv,feats_to_use,crit_value,eval_metric,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,
                    xgb_standardize,xgb_tree_method,xgb_boost,dart_normalize_type,dart_sample_type,rate_drop,skip_drop,eta,gamma,max_depth,
                    min_child_weight,subsamp,colsamp,nrounds,early_stop,temp_db)

    }, seed=TRUE)

    prom = xgbcl_select_calculation %...>% xgbcl_select_result

    prom = catch(xgbcl_select_calculation,
                 function(e){
                   xgbcl_select_result(NULL)
                   showModal(modalDialog(paste0("XGBCL covariate filtering cancelled. No results generated."),footer = modalButton("Close")))
                 })

    prom = finally(prom, function(){
      running(FALSE)
    })
    
    xgbcl_selection_results = dbReadTable(temp_db, "xgbcl_selection_results")
    xgbcl_selection_results = xgbcl_selection_results[,-3]
    xgbcl_selection_results = xgbcl_selection_results[,-2]
    
    weighted_mean = round(input$testcl_weight*as.numeric(xgbcl_selection_results[,5])+(1-input$testcl_weight)*as.numeric(xgbcl_selection_results[,4]),4)
    final_xgbcl_select_result = cbind(xgbcl_selection_results,weighted_mean)
    colnames(final_xgbcl_select_result) = c(colnames(xgbcl_selection_results),"Weighted Mean LnLoss")
    
    output$XGBCL_select = DT::renderDataTable(server=T,{
      data = datatable(final_xgbcl_select_result,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode="single"),editable=F,extensions="Buttons", options = list(autoWidth=F,dom='tB',paging = F,pageLength = 17,scrollX = F,
                  scrollY = TRUE,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                  initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}"))) %>%
                  formatRound(columns=c(1,3:6), digits=c(0,4,4,4,4))
      data$x$data[[1]] = as.numeric(data$x$data[[1]])
      data
    })
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGBCL: Feat Select')
    
    #Return something other than the future so we don't block the UI
    NULL
  })
  
  observeEvent(input$testcl_weight, {
    
    tables = dbListTables(temp_db)
    
    if ('xgbcl_selection_results' %in% tables) {
      
      xgbcl_selection_results = dbReadTable(temp_db, "xgbcl_selection_results")
      xgbcl_selection_results = xgbcl_selection_results[,-3]
      xgbcl_selection_results = xgbcl_selection_results[,-2]
      
      weighted_mean = round(input$testcl_weight*as.numeric(xgbcl_selection_results[,5])+(1-input$testcl_weight)*as.numeric(xgbcl_selection_results[,4]),4)
      final_xgbcl_select_result = cbind(xgbcl_selection_results,weighted_mean)
      colnames(final_xgbcl_select_result) = c(colnames(xgbcl_selection_results),"Weighted Mean LnLoss")
      
      output$XGBCL_select = DT::renderDataTable(server=T,{
        data = datatable(final_xgbcl_select_result,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode="single"),editable=F,extensions="Buttons", options = list(autoWidth=F,dom='tB',paging = F,pageLength = 17,scrollX = F,
                  scrollY = TRUE,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                  initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}"))) %>%
          formatRound(columns=c(1,3:6), digits=c(0,4,4,4,4))
        data$x$data[[1]] = as.numeric(data$x$data[[1]])
        data
      })
    }
  })
  
  observeEvent(input$XGBCL_select_cancel, {
    print("Stopping calculation...")
    stopMulticoreFuture(xgbcl_select_calculation)
  })
  
  observeEvent(input$XGBCL_select_rows_selected, ignoreInit = T, {
    
    if (input$use_pca_data) {
      all_feats = pca_axes()
    } else {
      all_feats = feat_names()
    }
    
    temp_data = dbReadTable(temp_db, "xgbcl_selection_results")
    
    crit_val = as.numeric(input$XGBCL_select_rows_selected[1])
    
    if (crit_val > 1) {
      
      tossed_feats = temp_data[which(as.numeric(temp_data$Iteration) < crit_val),"Lowest_SHAP"]
      remaining = all_feats[-which(all_feats %in% tossed_feats)]
      
    } else {
      remaining = all_feats
    }
    
    if (input$use_pca_data) {
      updateCheckboxGroupInput(session,"pcax_to_use",choices=pca_axes(),selected=remaining,inline=T)
    } else {
      updateCheckboxGroupInput(session,"feats_to_use",choices=feat_names(),selected=remaining,inline=T)
    }
  })
  
  # XGB Classifier predictions
  
  observeEvent(input$XGBCL_pred_dc, {
    
    if (nrow(XGBCL_pred_scat_dat) != 0) {
      
      output$XGBCL_pred_scatplot = renderPlot(ggplot(XGBCL_pred_scat_dat, aes(x=XGBCL_pred_scat_dat[,3], fill=as.factor(XGBCL_pred_scat_dat[,2]))) +
                                             geom_density(alpha = 0.6, color = "black", size = 0.5) +
                                             scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                                             geom_vline(xintercept = input$XGBCL_pred_dc, linetype = "dashed", color = "darkgreen") +
                                             labs(x = "Predicted Probability", y = "Density", fill="OBS") +
                                             theme_bw() +
                                             theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                             theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                             theme(legend.position = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      confuse_results = confuse(XGBCL_pred_scat_dat[,2:3],0.5,input$XGBCL_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGBCL_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGBCL_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$run_pred_XGBCL, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    xgbcl_pred_results = xgbcl_call_predict(data,rv,id_var,input$model_seed,ignored_rows,feats_to_use,input$XGBCL_eval,input$lc_lowval,
                              input$lc_upval,input$rc_lowval,input$rc_upval,input$train_pct/100,input$MC_runs,input$num_folds,input$loggy,input$randomize,
                              input$XGBCL_standard,xgbcl_tree_method_set(),xgbcl_booster_set(),dartcl_normalize_type_set(),dartcl_sample_type_set(),
                              ratecl_drop_set(),skipcl_drop_set(),Optimal_CLHP$eta,Optimal_CLHP$gamma,Optimal_CLHP$max_depth,
                              Optimal_CLHP$min_child_weight,Optimal_CLHP$subsample,Optimal_CLHP$colsample_bytree,Optimal_CLHP$nrounds,
                              input$XGBCL_binarize,input$XGBCL_binarize_crit_value)
    
    XGBCL_saved_predictions <<- data.frame(xgbcl_pred_results[[1]])
    
    XGBCL_pred_shapes = data.frame(xgbcl_pred_results[[2]])
    colnames(XGBCL_pred_shapes) = c("Feature","SHAP Value")
    
    output$XGBCL_predictions = DT::renderDataTable(server = T, {data = datatable(XGBCL_saved_predictions,rownames = F,selection =
              list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions='Buttons',options = list(autoWidth = F,
              paging = TRUE,pageLength = 17,dom="ltBp",buttons = c('copy', 'csv', 'excel'),scrollX = TRUE,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',
              orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744',
              'color': '#fff'});","}")))
    })
    
    output$XGBCL_pred_shapes = DT::renderDataTable(server = T, {data = datatable(XGBCL_pred_shapes,rownames = F,selection =
                        list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                        list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                        className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    XGBCL_pred_scat_dat <<- XGBCL_saved_predictions[,1:3]
    
    output$XGBCL_pred_scatplot = renderPlot(ggplot(XGBCL_pred_scat_dat, aes(x=XGBCL_pred_scat_dat[,3], fill=as.factor(XGBCL_pred_scat_dat[,2]))) +
                                           geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
                                           scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                                           geom_vline(xintercept = input$XGBCL_pred_dc, linetype = "dashed", color = "darkgreen") +
                                           labs(x = "Predicted Probability", y = "Density", fill="OBS") +
                                           theme_bw() +
                                           theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                           theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                           theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
    
    confuse_results = confuse(XGBCL_pred_scat_dat[,2:3],0.5,input$XGBCL_pred_dc)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$XGBCL_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$XGBCL_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    
    output$XGBCL_used_hp_pred = DT::renderDataTable(server=T,{data = datatable(Optimal_CLHP,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                      target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                      pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                      initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    removeModal()
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGBCL: Predict')
    
  })
  
  # XGB Classifier fitting
  
  observeEvent(input$XGBCL_dec_crit, {
    
    if (nrow(XGBCL_scat_dat) != 0) {
      
      output$XGBCL_scatplot = renderPlot(ggplot(XGBCL_scat_dat, aes(x=XGBCL_scat_dat[,3], fill=as.factor(XGBCL_scat_dat[,2]))) +
                                                geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
                                                scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                                                geom_vline(xintercept = input$XGBCL_dec_crit, linetype = "dashed", color = "darkgreen") +
                                                labs(x = "Fitted Probability", y = "Density", fill="OBS") +
                                                theme_bw() +
                                                theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                                theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                                theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      xgbcl_confuse_results = confuse(XGBCL_scat_dat[,2:3],0.5,input$XGBCL_dec_crit)
      xgbcl_confuse_table = matrix(0,nrow=1,ncol=4)
      
      xgbcl_confuse_table[1,1] = xgbcl_confuse_results$TP
      xgbcl_confuse_table[1,2] = xgbcl_confuse_results$TN
      xgbcl_confuse_table[1,3] = xgbcl_confuse_results$FP
      xgbcl_confuse_table[1,4] = xgbcl_confuse_results$FN
      
      colnames(xgbcl_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGBCL_confuse = DT::renderDataTable(server = T, {data = datatable(xgbcl_confuse_table,rownames = F,selection = 
                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                    options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                    columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                    {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGBCL_confuse_text = renderText({paste0("Sensitivity = ",round(xgbcl_confuse_results$Sensitivity,3),"; Specificity = ",
                                                   round(xgbcl_confuse_results$Specificity,3),"; Accuracy = ",round(xgbcl_confuse_results$Accuracy,3))})
    }
  })
  
  observeEvent(input$run_fit_XGBCL, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    crit_value = input$XGBCL_binarize_crit_value
    MC_runs = input$MC_runs
    
    if (is.null(ignored_rows)) {
      data0 = data
    } else {
      data0 = data[-ignored_rows,]
    }
    
    if (input$XGBCL_binarize) {
      new_Y = ifelse(test = data0[,rv] >= crit_value, yes = 1, no = 0)
      data0[,rv] = new_Y
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    data0 = data0[!is.na(data0[,rv]), ]
    
    var_list = c(rv,which(colnames(data0) %in% feats_to_use))
    data = data0[,var_list]
    colnames(data) = c(colnames(data0)[[rv]],feats_to_use)
    
    if (input$XGBCL_standard) {
      
      for (i in 1:nrow(data)) {
        for (j in 2:ncol(data)) {
          if (is.numeric(data[i,j])) {
            
            range = (max(na.omit(data[,j])) - min(na.omit(data[,j])))
            
            if (range == 0) {
              data[i,j] = 0
            } else {
              data[i,j]=(data[i,j] - min(na.omit(data[,j]))) / range
            }
          }
        }
      }
    }
    
    withProgress(
      message = 'XGBCL Fitting Progress',
      detail = paste("MC runs:", x = 1),
      value = 0,
      {
        
        temp_fits = matrix(0, nrow = nrow(data), ncol = 2*MC_runs)
        temp_fits = data.frame(temp_fits)
        
        temp_shapes = matrix(0, nrow = length(feats_to_use), ncol = MC_runs+1)
        temp_shapes = data.frame(temp_shapes)
        temp_shapes[,1] = feats_to_use
        
        for (i in 1:MC_runs) {
          
          # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
          if (input$loggy) {
            
            for (j in 1:nrow(data)){
              if (data[j,1]=="TNTC") {
                data[j,1]=log10(runif(1, min = rc_lowval, max = rc_upval))
                ifelse(test = data[j, 1] >= crit_value, yes = 1, no = 0)
              }
              
              if (data[j,1]=="ND") {
                data[j,1]=log10(runif(1, min = lc_lowval, max = lc_upval))
                ifelse(test = data[j, 1] >= crit_value, yes = 1, no = 0)
              }
            }
          } else {
            
            for (j in 1:nrow(data)){
              if (data[j,1]=="TNTC") {
                data[j,1]=(runif(1, min = rc_lowval, max = rc_upval))
                ifelse(test = data[j, 1] >= crit_value, yes = 1, no = 0)
              }
              
              if (data[j,1]=="ND") {
                data[j,1]=(runif(1, min = lc_lowval, max = lc_upval))
                ifelse(test = data[j, 1] >= crit_value, yes = 1, no = 0)
              }
            }
          }
          
          temp_fits[,2*i-1] = data[,1]
          
          if (xgbcl_booster_set() == "dart") {
            
            params = list(
              objective = "binary:logistic",
              eval_metric = input$XGBCL_eval,
              booster = xgbcl_booster_set(),
              rate_drop = ratecl_drop_set(),
              skip_drop = skipcl_drop_set(),
              sample_type = dartcl_sample_type_set(),
              normalize_type = dartcl_normalize_type_set(),
              tree_method = xgbcl_tree_method_set(),
              eta = Optimal_CLHP$eta,
              gamma = Optimal_CLHP$gamma,
              max_depth = Optimal_CLHP$max_depth,
              min_child_weight = Optimal_CLHP$min_child_weight,
              subsample = Optimal_CLHP$subsample,
              colsample_bytree = Optimal_CLHP$colsample_bytree
            )
          } else {
            params = list(
              objective = "binary:logistic",
              eval_metric = input$XGBCL_eval,
              booster = xgbcl_booster_set(),
              tree_method = xgbcl_tree_method_set(),
              eta = Optimal_CLHP$eta,
              gamma = Optimal_CLHP$gamma,
              max_depth = Optimal_CLHP$max_depth,
              min_child_weight = Optimal_CLHP$min_child_weight,
              subsample = Optimal_CLHP$subsample,
              colsample_bytree = Optimal_CLHP$colsample_bytree
            )
          }
          
          xgbcl_model = xgboost(data = as.matrix(data[,-1]),label=data[,1], params=params, early_stopping_rounds=early_stop_set(), nrounds=Optimal_CLHP$nrounds, verbose=0)
          
          fits = predict(xgbcl_model, newdata=as.matrix(data[,-1]))
          temp_fits[,2*i] = fits
          
          if (ncol(data) > 2) {
            shap_values = shap.values(xgb_model = xgbcl_model, X_train = as.matrix(data[,-1]))
            mean_shaps = round(shap_values$mean_shap_score,4)
            shap_names = names(mean_shaps)
            shap = data.frame(cbind(shap_names,mean_shaps))
          } else {
            shap = data.frame("Feature" = colnames(data)[[2]], "mean_shaps" = 0)
          }
          
          for (c in 1:nrow(temp_shapes)) {
            current_feat = temp_shapes[c,1]
            temp_shapes[c,i+1] = as.numeric(shap[which(shap[,1] == current_feat),2])
          }
          
          incProgress(1/MC_runs, detail = paste("MC run: ",i,"/",MC_runs))
        }
      })

    # Must add: fit global/final XGB_model
    
    even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
    odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
    
    obs_mean_values = ifelse(test = rowMeans(odd_columns) >= 0.5, yes = 1, no = 0)
    fit_mean_values = rowMeans(even_columns)
    fits = cbind(obs_mean_values,round(fit_mean_values,3))
    
    xgbcl_results = data.frame(cbind(data0[,1],round(fits[,1],4),round(fits[,2],4),round(data[,-1],4)))
    colnames(xgbcl_results) = c(colnames(data0)[[1]],colnames(data)[[1]],"Fitted_Prob",colnames(data[,-1]))
    
    xgbcl_shapes1 = as.data.frame(temp_shapes[,-1])
    
    xgbcl_shapes2 = rowMeans(xgbcl_shapes1)
    
    xgbcl_shapes = data.frame(cbind(temp_shapes[,1],xgbcl_shapes2))
    
    xgbcl_shapes = xgbcl_shapes[order(xgbcl_shapes[,2],decreasing = T),]
    colnames(xgbcl_shapes) = c("Feature","Mean_SHAP")
    
    output$XGBCL_shapes = DT::renderDataTable(server = T, {data = datatable(xgbcl_shapes,rownames = F,selection =
                list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$XGBCL_fits = DT::renderDataTable(server = T, {data = datatable(xgbcl_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp", buttons = c('copy', 'csv', 'excel'),
                paging = T,pageLength = 17,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    XGBCL_scat_dat <<- xgbcl_results[,1:3]
    
    output$XGBCL_scatplot = renderPlot(ggplot(XGBCL_scat_dat, aes(x=XGBCL_scat_dat[,3], fill=as.factor(XGBCL_scat_dat[,2]))) +
                                      geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
                                      scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                                      geom_vline(xintercept = input$XGBCL_dec_crit, linetype = "dashed", color = "darkgreen") +
                                      labs(x = "Fitted Probability", y = "Density", fill="OBS") +
                                      theme_bw() +
                                      theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                      theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                      theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))

    xgbcl_confuse_results = confuse(XGBCL_scat_dat[,2:3],0.5,input$XGBCL_dec_crit)
    xgbcl_confuse_table = matrix(0,nrow=1,ncol=4)
    
    xgbcl_confuse_table[1,1] = xgbcl_confuse_results$TP
    xgbcl_confuse_table[1,2] = xgbcl_confuse_results$TN
    xgbcl_confuse_table[1,3] = xgbcl_confuse_results$FP
    xgbcl_confuse_table[1,4] = xgbcl_confuse_results$FN
    
    colnames(xgbcl_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$XGBCL_confuse = DT::renderDataTable(server = T, {data = datatable(xgbcl_confuse_table,rownames = F,selection = 
                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                    options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                    columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                    {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$XGBCL_confuse_text = renderText({paste0("Sensitivity = ",round(xgbcl_confuse_results$Sensitivity,3),"; Specificity = ",
                    round(xgbcl_confuse_results$Specificity,3),"; Accuracy = ",round(xgbcl_confuse_results$Accuracy,3))})
    
    output$XGBCL_used_hp = DT::renderDataTable(server=T,{data = datatable(Optimal_CLHP,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                    target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                    pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                    initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGBCL: Fitting')
  })

  # XGB feature selection
  
  observeEvent(input$run_XGB_select, {
    
    if(running())
      return(NULL)
    running(TRUE)
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
      pcax_being_used(feats_to_use)
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
      feats_being_used(feats_to_use)
    }
    
    eta = eta_set()
    gamma = gamma_set()
    max_depth = max_depth_set()
    min_child_weight = min_child_weight_set()
    nrounds = nrounds_set()
    early_stop = early_stop_set()
    subsamp = subsamp_set()
    colsamp = colsamp_set()
    
    if (is.null(ignored_rows)) {
      xgb_select_data = data
    } else {
      xgb_select_data = data[-ignored_rows,]
    }
    
    xgb_tree_method = xgb_tree_method_set()
    xgb_boost = xgb_booster_set()
    dart_normalize_type = dart_normalize_type_set()
    dart_sample_type = dart_sample_type_set()
    rate_drop = rate_drop_set()
    skip_drop = skip_drop_set()
    
    xgb_standardize = input$XGB_standardize
    lc_lowval = input$lc_lowval
    lc_upval = input$lc_upval
    rc_lowval = input$rc_lowval
    rc_upval = input$rc_upval
    train_prop = input$train_pct/100
    MC_runs = input$MC_runs
    loggy = input$loggy
    randomize = input$randomize
    seed = input$model_seed
    
    xgb_select_result(NULL)
    
    xgb_select_calculation <<- future({
      
      xgb_selection(xgb_select_data,seed,rv,feats_to_use,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,
                    xgb_standardize,xgb_tree_method,xgb_boost,dart_normalize_type,dart_sample_type,rate_drop,skip_drop,eta,gamma,max_depth,
                    min_child_weight,subsamp,colsamp,nrounds,early_stop,temp_db)
      
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
    
    xgb_selection_results = dbReadTable(temp_db, "xgb_selection_results")
    xgb_selection_results = xgb_selection_results[,-3]
    xgb_selection_results = xgb_selection_results[,-2]
    
    weighted_mean = round(input$test_weight*as.numeric(xgb_selection_results[,5])+(1-input$test_weight)*as.numeric(xgb_selection_results[,4]),4)
    final_xgb_select_result = cbind(xgb_selection_results,weighted_mean)
    colnames(final_xgb_select_result) = c(colnames(xgb_selection_results),"Weighted Mean RMSE")
    
    output$XGB_select = DT::renderDataTable(server=T,{
      data = datatable(final_xgb_select_result,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
        target = "row",mode="single"),editable=F,extensions="Buttons", options = list(autoWidth=F,dom='tB',paging = F,pageLength = 17,scrollX = F,
        scrollY = TRUE,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
        initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}"))) %>%
        formatRound(columns=c(1,3:6), digits=c(0,4,4,4,4))
      data$x$data[[1]] = as.numeric(data$x$data[[1]])
      data
    })
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Feat Select')
    
    #Return something other than the future so we don't block the UI
    NULL
  })
  
  observeEvent(input$test_weight, {
    
    tables = dbListTables(temp_db)
    
    if ('xgb_selection_results' %in% tables) {
      
      xgb_selection_results = dbReadTable(temp_db, "xgb_selection_results")
      xgb_selection_results = xgb_selection_results[,-3]
      xgb_selection_results = xgb_selection_results[,-2]
      
      weighted_mean = round(input$test_weight*as.numeric(xgb_selection_results[,5])+(1-input$test_weight)*as.numeric(xgb_selection_results[,4]),4)
      final_xgb_select_result = cbind(xgb_selection_results,weighted_mean)
      colnames(final_xgb_select_result) = c(colnames(xgb_selection_results),"Weighted Mean RMSE")
      
      output$XGB_select = DT::renderDataTable(server=T,{
        data = datatable(final_xgb_select_result,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                target = "row",mode="single"),editable=F,extensions="Buttons", options = list(autoWidth=F,dom='tB',paging = F,pageLength = 17,scrollX = F,
                scrollY = TRUE,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}"))) %>%
          formatRound(columns=c(1,3:6), digits=c(0,4,4,4,4))
        data$x$data[[1]] = as.numeric(data$x$data[[1]])
        data
      })
    }
  })
  
  observeEvent(input$XGB_select_cancel, {
    print("Stopping calculation...")
    stopMulticoreFuture(xgb_select_calculation)
  })
  
  observeEvent(input$XGB_select_rows_selected, ignoreInit = T, {
    
    if (input$use_pca_data) {
      all_feats = pca_axes()
    } else {
      all_feats = feat_names()
    }
    
    temp_data = dbReadTable(temp_db, "xgb_selection_results")
    
    crit_val = as.numeric(input$XGB_select_rows_selected[1])
    
    if (crit_val > 1) {
      
      tossed_covar = temp_data[which(as.numeric(temp_data$Iteration) < crit_val),"Lowest_SHAP"]
      remaining = all_feats[-which(all_feats %in% tossed_covar)]
      
    } else {
      remaining = all_feats
    }
    
    if (input$use_pca_data) {
      updateCheckboxGroupInput(session,"pcax_to_use",choices=pca_axes(),selected=remaining,inline=T)
    } else {
      updateCheckboxGroupInput(session,"feats_to_use",choices=feat_names(),selected=remaining,inline=T)
    }
  })
  
  # XGB HP optimization
  
  observeEvent(input$XGB_optimize_HP, {
    
    showModal(modalDialog(title="HP Optimization", card(
      fluidRow(
        column(5,selectInput("XGB_hyper_metric", "Evaluation Metric", choices = c("rmse","mae","mape"), selected = "rmse"))),
      fluidRow(
        column(4,numericInput("pso_max_iter", "Max Iterations", min=5, max=1000, value=20, step = 1)),
        column(2),
        column(4,numericInput("pso_swarm_size", "Swarm Size", min=3, max=200, value=10, step = 1))),
      fluidRow(
        column(4,numericInput("member_exp", "Membership Weight", min=0.25, max=3, value=0.5, step = 0.25)),
        column(2),
        column(4,numericInput("ss_exp", "Sum of Squares Weight", min=0.25, max=3, value=1, step = 0.25)))),
      footer = div(actionButton("run_XGB_optimize_HP", "Run"),modalButton('Close'))))#,actionButton("stop_xgb_HP_and_errors", "Cancel the Calculation"#))
  })
  
  observeEvent(input$run_XGB_optimize_HP, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    xgb_optim_HP_results = xgb_call_optimize_HP(data,rv,id_var,input$model_seed,ignored_rows,feats_to_use,input$lc_lowval,
                            input$lc_upval,input$rc_lowval,input$rc_upval,input$MC_runs,input$num_folds,input$loggy,input$randomize,input$XGB_standardize,
                            input$XGB_hyper_metric,input$pso_max_iter,input$pso_swarm_size,input$member_exp,input$ss_exp)
    
    xgb_optim_HP_results1 = data.frame(xgb_optim_HP_results)
    
    Optimal_HP$max_depth <<- round(xgb_optim_HP_results1[1,1],0)
    Optimal_HP$eta <<- round(xgb_optim_HP_results1[2,1],3)
    Optimal_HP$subsample <<- round(xgb_optim_HP_results1[3,1],2)
    Optimal_HP$colsample_bytree <<- round(xgb_optim_HP_results1[4,1],2)
    Optimal_HP$min_child_weight <<- round(xgb_optim_HP_results1[5,1],0)
    Optimal_HP$gamma <<- round(xgb_optim_HP_results1[6,1],1)
    Optimal_HP$nrounds <<- round(xgb_optim_HP_results1[7,1],0)
    
    output$XGB_optim_hp = DT::renderDataTable(server=T,{data = datatable(Optimal_HP,rownames=F,extensions='Buttons',selection=list(selected =
                list(rows = NULL, cols = NULL),target = "row",mode="single"),editable=F,options = list(autoWidth=F,dom='tB',paging = F,pageLength = 5,scrollX = F,
                scrollY = F,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: HP Optim')
  })
  
  # XGB HP settings
  
  observeEvent(input$XGB_params, {
    
    showModal(modalDialog(title="XGB Hyperparameters",easyClose=F,card(
      fluidRow(
        column(4,numericInput("eta", label="Eta", value = eta_set(),min=0,max=1,step=0.01)),
        column(4,numericInput("gamma", label="Gamma", value = gamma_set(), min=0, max=20, step = 1)),
        column(4,numericInput("nrounds", label="# Rounds", value = nrounds_set(), min=100, max=3000, step = 25))),
      fluidRow(
        column(6,numericInput("max_depth", label="Max Tree Depth", value = max_depth_set(), min=1)),
        column(6,numericInput("min_child_weight", label="Min Leaf Size", value = min_child_weight_set(), min=1))),
      fluidRow(
        column(6,numericInput("subsamp", label="Subsample Proportion", value = subsamp_set(), min=0,max=1, step=0.01)),
        column(6,numericInput("colsamp", label="Column Sample Proportion", value = colsamp_set(), min=0,max=1,step=0.01))),
      fluidRow(
        column(4,selectInput("XGB_tree_method",label = "Tree Method",selected =xgb_tree_method_set(),choices = c("hist","exact","approx"))),
        column(4,selectInput("XGB_booster",label = "Booster",selected =xgb_booster_set(),choices = c("gbtree","gblinear","dart")))),
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
      footer = div(actionButton("save_XGB_hp_settings",label='Save Settings'),modalButton("Close"))))
    
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
  
  observeEvent(input$save_XGB_hp_settings, ignoreInit = T, {
    
    eta_set(input$eta)
    gamma_set(input$gamma)
    max_depth_set(input$max_depth)
    min_child_weight_set(input$min_child_weight)
    nrounds_set(input$nrounds)
    early_stop_set(input$early_stop)
    nfold_set(input$nfold)
    subsamp_set(input$subsamp)
    colsamp_set(input$colsamp)
    
    xgb_tree_method_set(input$XGB_tree_method)
    xgb_booster_set(input$XGB_booster)
    
    dart_normalize_type_set(input$dart_normalize_type)
    dart_sample_type_set(input$dart_sample_type)
    rate_drop_set(input$rate_drop)
    skip_drop_set(input$skip_drop)
    
    Optimal_HP <<- data.frame(max_depth = input$max_depth,eta = input$eta,subsample = input$subsamp,colsample_bytree = input$colsamp,
                              min_child_weight = input$min_child_weight,gamma = input$gamma,nrounds = input$nrounds)
    
    removeModal()
    
  })
  
  observeEvent(input$XGB_booster, {
    if (input$XGB_booster == "dart") {
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
  
  # XGB predictions
  
  observeEvent(input$XGB_pred_stand, {
    
    if (nrow(XGB_pred_scat_dat) != 0) {
      
      iv$add_rule("XGB_pred_stand", sv_between(min(XGB_pred_scat_dat[,2]),max(XGB_pred_scat_dat[,2])))

      output$XGB_pred_scatplot = renderPlotly(scatter_confuse(XGB_pred_scat_dat,input$XGB_pred_stand,input$XGB_pred_dc))
      
      confuse_results = confuse(XGB_pred_scat_dat[,2:3],input$XGB_pred_stand,input$XGB_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGB_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                      options = list(autoWidth = F,dom='tB', paging = F,buttons = c('copy', 'csv', 'excel'),scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGB_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    }
  })
  
  observeEvent(input$XGB_pred_dc, {
    
    if (nrow(XGB_pred_scat_dat) != 0) {
    
      iv$add_rule("XGB_pred_dc", sv_between(min(XGB_pred_scat_dat[,3]),max(XGB_pred_scat_dat[,3])))
      
      output$XGB_pred_scatplot = renderPlotly(scatter_confuse(XGB_pred_scat_dat,input$XGB_pred_stand,input$XGB_pred_dc))
      
      confuse_results = confuse(XGB_pred_scat_dat[,2:3],input$XGB_pred_stand,input$XGB_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGB_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                      orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGB_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    }
  })
  
  observeEvent(input$run_XGB_predict, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    xgb_pred_results = xgb_call_predict(data,rv,id_var,input$model_seed,ignored_rows,feats_to_use,input$lc_lowval,
                            input$lc_upval,input$rc_lowval,input$rc_upval,input$train_pct/100,input$MC_runs,input$num_folds,input$loggy,input$randomize,
                            input$XGB_standardize,Optimal_HP$eta,Optimal_HP$gamma,Optimal_HP$max_depth,
                            Optimal_HP$min_child_weight,Optimal_HP$subsamp,Optimal_HP$colsamp,Optimal_HP$nrounds)
    
    XGB_saved_predictions <<- xgb_pred_results[[1]]
    
    xgb_pred_stepr = round((max(XGB_saved_predictions[,2])-min(XGB_saved_predictions[,2]))/40,2)
    
    updateNumericInput(session, "XGB_pred_stand",
                       value = round(mean(XGB_saved_predictions[,2]),2),
                       max = round(max(XGB_saved_predictions[,2]),2),
                       min = round(min(XGB_saved_predictions[,2]),2),
                       step = xgb_pred_stepr
    )
    
    XGB_pred_shapes = data.frame(xgb_pred_results[[2]])
    colnames(XGB_pred_shapes) = c("Feature","SHAP Value")
    
    xgb_pred_stepdc = round((max(XGB_saved_predictions[,3])-min(XGB_saved_predictions[,3]))/40,2)
    
    updateNumericInput(session, "XGB_pred_dc",
                       value = round(mean(XGB_saved_predictions[,3]),2),
                       max = round(max(XGB_saved_predictions[,3]),2),
                       min = round(min(XGB_saved_predictions[,3]),2),
                       step = xgb_pred_stepdc
    )
    
    output$XGB_predictions = DT::renderDataTable(server = T, {data = datatable(XGB_saved_predictions,rownames = F,selection =
              list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions='Buttons',options = list(autoWidth = F,
              paging = TRUE,pageLength = 17,dom="ltBp",buttons = c('copy', 'csv', 'excel'),scrollX = TRUE,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',
              orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744',
              'color': '#fff'});","}")))#{if (date_format_string != "Other") formatDate(data,1,date_format_string) else .}
    })
    
    output$XGB_pred_shapes = DT::renderDataTable(server = T, {data = datatable(XGB_pred_shapes,rownames = F,selection =
                        list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                        list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                        className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    XGB_pred_scat_dat <<- XGB_saved_predictions[,1:3]
    output$XGB_pred_scatplot = renderPlotly(scatter_confuse(XGB_pred_scat_dat,input$XGB_pred_stand,input$XGB_pred_dc))
    
    confuse_results = confuse(XGB_pred_scat_dat[,2:3],input$XGB_pred_stand,input$XGB_pred_dc)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$XGB_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$XGB_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    
    resid_data = XGB_pred_scat_dat[,c(1,3)] %>% mutate(Residuals = round(XGB_pred_scat_dat[,2]-XGB_pred_scat_dat[,3],3))
    
    output$XGB_pred_resid_scatplot = renderPlotly(scatter(resid_data))
    
    output$XGB_pred_lineplot = renderPlotly(plot_ly(XGB_pred_scat_dat, x = ~XGB_pred_scat_dat[,1], y = ~XGB_pred_scat_dat[,2], name="Observations",
                      type="scatter", mode = "lines",text = ~paste("<b>ID: </b>",XGB_pred_scat_dat[,1],"<br><b>Observed Value:</b> ",
                      XGB_pred_scat_dat[,2],sep=""),hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5)) %>%
                  add_trace(y = ~XGB_pred_scat_dat[,3], name="Predictions", mode = 'lines',
                      text = ~paste("<b>ID: </b>",XGB_pred_scat_dat[,1],"<br><b>Predicted Value:</b> ",round(XGB_pred_scat_dat[,3],3),sep=""),
                      hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                  layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Predictions",font=list(size=20)),
                      range=c(min(0.99*min(XGB_pred_scat_dat[,2],XGB_pred_scat_dat[,3]),1.01*min(XGB_pred_scat_dat[,2],XGB_pred_scat_dat[,3])),
                      max(0.99*max(XGB_pred_scat_dat[,2],XGB_pred_scat_dat[,3]),1.01*max(XGB_pred_scat_dat[,2],XGB_pred_scat_dat[,3]))))))
    
    output$XGB_used_hp_pred = DT::renderDataTable(server=T,{data = datatable(Optimal_HP,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                  pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                  initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    removeModal()
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Predict')
  })
  
  # XGB fitting
  
  observeEvent(input$XGB_stand, {
    
    if (nrow(XGB_scat_dat) != 0) {
      
      iv$add_rule("XGB_stand", sv_between(min(XGB_scat_dat[,2]),max(XGB_scat_dat[,2])))
      
      output$XGB_scatplot = renderPlotly(scatter_confuse(XGB_scat_dat,input$XGB_stand,input$XGB_dec_crit))
      
      xgb_confuse_results = confuse(XGB_scat_dat[,2:3],input$XGB_stand,input$XGB_dec_crit)
      xgb_confuse_table = matrix(0,nrow=1,ncol=4)
      
      xgb_confuse_table[1,1] = xgb_confuse_results$TP
      xgb_confuse_table[1,2] = xgb_confuse_results$TN
      xgb_confuse_table[1,3] = xgb_confuse_results$FP
      xgb_confuse_table[1,4] = xgb_confuse_results$FN
      
      colnames(xgb_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGB_confuse = DT::renderDataTable(server = T, {data = datatable(xgb_confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGB_confuse_text = renderText({paste0("Sensitivity = ",round(xgb_confuse_results$Sensitivity,3),"; Specificity = ",
                                round(xgb_confuse_results$Specificity,3),"; Accuracy = ",round(xgb_confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$XGB_dec_crit, {
    
    if (nrow(XGB_scat_dat) != 0) {
      
      iv$add_rule("XGB_dec_crit", sv_between(min(XGB_scat_dat[,3]),max(XGB_scat_dat[,3])))
      
      output$XGB_scatplot = renderPlotly(scatter_confuse(XGB_scat_dat,input$XGB_stand,input$XGB_dec_crit))
      
      xgb_confuse_results = confuse(XGB_scat_dat[,2:3],input$XGB_stand,input$XGB_dec_crit)
      xgb_confuse_table = matrix(0,nrow=1,ncol=4)
      
      xgb_confuse_table[1,1] = xgb_confuse_results$TP
      xgb_confuse_table[1,2] = xgb_confuse_results$TN
      xgb_confuse_table[1,3] = xgb_confuse_results$FP
      xgb_confuse_table[1,4] = xgb_confuse_results$FN
      
      colnames(xgb_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGB_confuse = DT::renderDataTable(server = T, {data = datatable(xgb_confuse_table,rownames = F,selection = 
                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                    options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                    columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                    {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGB_confuse_text = renderText({paste0("Sensitivity = ",round(xgb_confuse_results$Sensitivity,3),"; Specificity = ",
                    round(xgb_confuse_results$Specificity,3),"; Accuracy = ",round(xgb_confuse_results$Accuracy,3))})
    }
  })
  
  observeEvent(input$XGB_final_fitting, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    if (is.null(ignored_rows)) {
      data0 = data
    } else {
      data0 = data[-ignored_rows,]
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    data0 = data0[!is.na(data0[,rv]), ]
    
    var_list = c(rv,which(colnames(data0) %in% feats_to_use))
    data = data0[,var_list]
    
    MC_runs = input$MC_runs
    
    temp_fits = matrix(0, nrow = nrow(data), ncol = 2*MC_runs)
    temp_fits = data.frame(temp_fits)
    
    temp_shapes = matrix(0, nrow = length(feats_to_use), ncol = MC_runs+1)
    temp_shapes = data.frame(temp_shapes)
    temp_shapes[,1] = feats_to_use
    
    if (input$XGB_standardize) {
      
      for (i in 1:nrow(data)) {
        for (j in 2:ncol(data)) {
          if (is.numeric(data[i,j])) {
            
            range = (max(na.omit(data[,j])) - min(na.omit(data[,j])))
            
            if (range == 0) {
              data[i,j] = 0
            } else {
              data[i,j]=(data[i,j] - min(na.omit(data[,j]))) / range
            }
          }
        }
      }
    }
    
    withProgress(
      message = 'XGB Fitting Progress',
      detail = paste("MC runs:", x = MC_runs),
      value = 1/MC_runs,
      {
        
        for (i in 1:MC_runs) {
          
          # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
          if (input$loggy==TRUE) {
            
            for (j in 1:nrow(data)){
              if (data[j,1]=="TNTC") {
                data[j,1]=log10(runif(1, min = rc_lowval, max = rc_upval))
              }
              
              if (data[j,1]=="ND") {
                data[j,1]=log10(runif(1, min = lc_lowval, max = lc_upval))
              }
            }
          } else {
            
            for (j in 1:nrow(data)){
              if (data[j,1]=="TNTC") {
                data[j,1]=(runif(1, min = rc_lowval, max = rc_upval))
              }
              
              if (data[j,1]=="ND") {
                data[j,1]=(runif(1, min = lc_lowval, max = lc_upval))
              }
            }
          }
          
          temp_fits[,2*i-1] = data[,1]
          
          if (xgb_booster_set() == "dart") {
            
            params = list(
              booster = xgb_booster_set(),
              rate_drop = rate_drop_set(),
              skip_drop = skip_drop_set(),
              sample_type = dart_sample_type_set(),
              normalize_type = dart_normalize_type_set(),
              tree_method = xgb_tree_method_set(),
              eta = Optimal_HP$eta,
              gamma = Optimal_HP$gamma,
              max_depth = Optimal_HP$max_depth,
              min_child_weight = Optimal_HP$min_child_weight,
              subsample = Optimal_HP$subsample,
              colsample_bytree = Optimal_HP$colsample_bytree
            )
          } else {
            params = list(
              booster = xgb_booster_set(),
              tree_method = xgb_tree_method_set(),
              eta = Optimal_HP$eta,
              gamma = Optimal_HP$gamma,
              max_depth = Optimal_HP$max_depth,
              min_child_weight = Optimal_HP$min_child_weight,
              subsample = Optimal_HP$subsample,
              colsample_bytree = Optimal_HP$colsample_bytree
            )
          }
          
          xgb_model = xgboost(data = as.matrix(data[,-1]),label=data[,1], params=params, early_stopping_rounds=early_stop_set(), nrounds=nrounds_set(), verbose=0)
          
          temp_fits[,2*i] = predict(xgb_model, newdata=as.matrix(data[,-1]))
          
          if (ncol(data) > 2) {
            
            shap_values = shap.values(xgb_model = xgb_model, X_train = as.matrix(data[,-1]))
            mean_shaps = shap_values$mean_shap_score
            shap_names = names(mean_shaps)
            shap_temp = data.frame(cbind(shap_names,mean_shaps))
          } else {
            shap_temp = data.frame("Feature" = colnames(data)[[2]], "Mean_SHAP" = 0)
          }
          
          for (c in 1:nrow(temp_shapes)) {
            current_feat = temp_shapes[c,1]
            temp_shapes[c,i+1] = as.numeric(shap_temp[shap_temp[,1] == current_feat,2])
          }
          
          incProgress(1/MC_runs, detail = paste("MC run: ",i,"/",MC_runs))
        }
        
      })
    
    # Must add: Fit global/final XGB_model
    
    even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
    odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
    
    obs_mean_values = rowMeans(odd_columns)
    fits_mean_values = rowMeans(even_columns)
    fits = cbind(obs_mean_values,round(fits_mean_values,3))
    
    temp_shapes1 = data.frame(temp_shapes[,-1])
    
    XGB_shapes0 = rowMeans(temp_shapes1)
    
    XGB_results = cbind(data0[,1],fits[,1:2],round(data[,2:ncol(data)],4))
    colnames(XGB_results) = c(colnames(data0)[[1]],colnames(data)[[1]],"Fitted_Values",colnames(data[,-1]))
    
    XGB_shapes = data.frame(cbind(temp_shapes[,1],format(round(XGB_shapes0,4),scientific=FALSE)))
    colnames(XGB_shapes) = c("Feature","Mean_SHAP")
    XGB_shapes = XGB_shapes[order(XGB_shapes[,2],decreasing=TRUE),]
    
    
    xgb_stepr = round((max(XGB_results[,2])-min(XGB_results[,2]))/40,2)
    
    updateNumericInput(session, "XGB_stand",
                       value = round(mean(XGB_results[,2]),2),
                       max = round(max(XGB_results[,2]),2),
                       min = round(min(XGB_results[,2]),2),
                       step = xgb_stepr
    )
    
    xgb_stepdc = round((max(XGB_results[,3])-min(XGB_results[,3]))/40,2)
    
    updateNumericInput(session, "XGB_dec_crit",
                       value = round(mean(XGB_results[,3]),2),
                       max = round(max(XGB_results[,3]),2),
                       min = round(min(XGB_results[,3]),2),
                       step = xgb_stepdc
    )
    
    output$XGB_shapes = DT::renderDataTable(server = T, {data = datatable(XGB_shapes,rownames = F,selection =
                  list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                  list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                  className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$XGB_fits = DT::renderDataTable(server = T, {data = datatable(XGB_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp", buttons = c('copy', 'csv', 'excel'),
                  paging = T,pageLength = 17,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    XGB_scat_dat <<- XGB_results[,1:3]
    
    output$XGB_scatplot = renderPlotly(scatter_confuse(XGB_scat_dat,input$XGB_stand,input$XGB_dec_crit))
    
    xgb_resid_data = XGB_scat_dat[,c(1,3)] %>% mutate(Residuals = round(XGB_scat_dat[,2]-XGB_scat_dat[,3],3))
    output$XGB_resid_scatplot = renderPlotly(scatter(xgb_resid_data))
    
    output$XGB_lineplot = renderPlotly(plot_ly(XGB_scat_dat, x = ~XGB_scat_dat[,1], y = ~XGB_scat_dat[,2], name="Observations", type="scatter", mode = "lines",
                        text = ~paste("<b>ID: </b>",XGB_scat_dat[,1],"<br><b>Observed Value:</b> ",XGB_scat_dat[,2],sep=""),hoveron = 'points',hoverinfo='text',
                        line = list(color = "#2c3e50", width = 1.5)) %>%
                  add_trace(y = ~XGB_scat_dat[,3], name="Fitted_Values", mode = 'lines',text = ~paste("<b>ID: </b>",XGB_scat_dat[,1],"<br><b>Fitted Value:</b> ",
                        round(XGB_scat_dat[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                  layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Fitted Values",font=list(size=20)),
                        range=c(min(0.99*min(XGB_scat_dat[,2],XGB_scat_dat[,3]),1.01*min(XGB_scat_dat[,2],XGB_scat_dat[,3])),max(0.99*max(XGB_scat_dat[,2],
                        XGB_scat_dat[,3]),1.01*max(XGB_scat_dat[,2],XGB_scat_dat[,3]))))))
    
    xgb_confuse_results = confuse(XGB_scat_dat[,2:3],input$XGB_stand,input$XGB_dec_crit)
    xgb_confuse_table = matrix(0,nrow=1,ncol=4)
    
    xgb_confuse_table[1,1] = xgb_confuse_results$TP
    xgb_confuse_table[1,2] = xgb_confuse_results$TN
    xgb_confuse_table[1,3] = xgb_confuse_results$FP
    xgb_confuse_table[1,4] = xgb_confuse_results$FN
    
    colnames(xgb_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$XGB_confuse = DT::renderDataTable(server = T, {data = datatable(xgb_confuse_table,rownames = F,selection = 
                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                    options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                    columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                    {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$XGB_confuse_text = renderText({paste0("Sensitivity = ",round(xgb_confuse_results$Sensitivity,3),"; Specificity = ",
                    round(xgb_confuse_results$Specificity,3),"; Accuracy = ",round(xgb_confuse_results$Accuracy,3))})
    
    output$XGB_used_hp = DT::renderDataTable(server=T,{data = datatable(Optimal_HP,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                    target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                    pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                    initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Fitting')
  })
  
  # Elastic Net predictions
  
  observeEvent(input$EN_pred_stand, {
    
    if (nrow(EN_pred_scat_dat) != 0) {
      
      iv$add_rule("EN_pred_stand", sv_between(min(EN_pred_scat_dat[,2]),max(EN_pred_scat_dat[,2])))
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_pred_scat_dat,input$EN_pred_stand,input$EN_pred_dc))
      
      confuse_results = confuse(EN_pred_scat_dat[,2:3],input$EN_pred_stand,input$EN_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                        list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                        options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                        columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                        {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                    round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$EN_pred_dc, {
    
    if (nrow(EN_pred_scat_dat) != 0) {
      
      iv$add_rule("EN_pred_dc", sv_between(min(EN_pred_scat_dat[,3]),max(EN_pred_scat_dat[,3])))
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_pred_scat_dat,input$EN_pred_stand,input$EN_pred_dc))
      
      confuse_results = confuse(EN_pred_scat_dat[,2:3],input$EN_pred_stand,input$EN_pred_dc)
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = confuse_results$TP
      confuse_table[1,2] = confuse_results$TN
      confuse_table[1,3] = confuse_results$FP
      confuse_table[1,4] = confuse_results$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$EN_pred, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    set.seed(input$model_seed)
    
    MC_runs = input$MC_runs
    
    if (is.null(ignored_rows)) {
      EN_data0 = data
    } else {
      EN_data0 = data[-ignored_rows,]
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    EN_data0 = EN_data0[!is.na(EN_data0[,rv]),]
    
    var_list = c(1,rv,which(colnames(EN_data0) %in% feats_to_use))
    EN_data = EN_data0[,var_list]
    colnames(EN_data) = c(colnames(data)[[1]],"Response",feats_to_use)
    
    # RANDOMIZE DATA
    if (input$randomize==TRUE) {
      random_index = sample(1:nrow(EN_data), nrow(EN_data))
      EN_data = EN_data[random_index, ]
    }
    
    using_data = EN_data[,-1]
    
    if (any(is.na(using_data[,-1]))) {
      
      showModal(modalDialog(paste("Elastic Net does not tolerate missing feature values. You can either Impute these
                (on the Data tab) or Disable rows/columns with missing values."),footer = modalButton("Close")))
      
    } else {
      
      #Create n folds
      tot_folds = input$num_folds
      folds = cut(seq(1, nrow(using_data)), breaks = tot_folds, labels = FALSE)
      
      fold_predictions = matrix(0, nrow = 0, ncol = 2)
      fold_predictions = as.data.frame(fold_predictions)
      
      coeff_folds = matrix(0, nrow = ncol(using_data), ncol = tot_folds+1)
      coeff_folds = as.data.frame(coeff_folds)
      coeff_folds[,1] = c("(Intercept)",feats_to_use)
      
      #Perform cross validation
      for (f in 1:tot_folds) {
        
        testIndices = which(folds == f, arr.ind = TRUE)
        testData = using_data[testIndices, ]
        trainData = using_data[-testIndices, ]
        
        temp_preds = matrix(0, nrow = nrow(testData), ncol = 2*MC_runs)
        temp_preds = data.frame(temp_preds)
        
        temp_coeffs = matrix(0, nrow = ncol(using_data), ncol = MC_runs+1)
        temp_coeffs = data.frame(temp_coeffs)
        temp_coeffs[,1] = c("(Intercept)",feats_to_use)
        
        withProgress(
          message = 'EN Prediction Progress',
          detail = paste("MC runs:", x = MC_runs,"; Fold:",y = f),
          value = (1-1/tot_folds) - (1/tot_folds)*(tot_folds-f),
          {
            
            for (i in 1:MC_runs) {
              
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
              
              # determine best alpha and lambda
              fit_mod = cva.glmnet(x=as.matrix(trainData[,-1]),y=trainData[,1],nfolds=input$num_folds,na.action="na.omit",
                                   standardize=input$EN_standard,intercept=TRUE)
              
              get_model_params <- function(fit) {
                alpha <- fit$alpha
                lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
                lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
                error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
                best <- which.min(error)
                data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
                           lambdaSE = lambdaSE[best], eror = error[best])
              }
              
              alpha = get_model_params(fit_mod)$alpha
              lambda = get_model_params(fit_mod)$lambdaMin
              
              # fit final model
              model = glmnet(x=as.matrix(trainData[,-1]),trainData[,1],lambda=lambda, alpha=alpha, na.action="na.omit",
                             standardize=input$EN_standard,intercept=TRUE)
              
              coeffs = as.matrix(coef(model, s=lambda))
              coeffs = as.data.frame(coeffs)
              temp_coeffs[,i+1] = coeffs
              
              preds = predict(model, newx = as.matrix(testData[,-1]))
              
              temp_preds[,2*i] = preds
              
              incProgress(1/(MC_runs*tot_folds), detail = paste("MC run:",i,"/",MC_runs,"; Fold:",f,"/",tot_folds))
              
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
      
      prediction_results = data.frame(cbind(EN_data[,1],round(fold_predictions[,1],3),round(fold_predictions[,2],3),round(using_data[,-1],4)))
      colnames(prediction_results) = c(colnames(EN_data)[[1]],colnames(using_data)[[1]],"Predictions",colnames(using_data[,-1]))
      
      prediction_results = prediction_results[order(prediction_results[,1]),]
      
      final_coeffs = data.frame(cbind(coeff_folds[,1],format(round(rowMeans(coeff_folds[,-1]),4),scientific=FALSE)))
      colnames(final_coeffs) = c("Feature","Coefficient")
      
      en_stepr = round((max(prediction_results[,2])-min(prediction_results[,2]))/40,2)
      
      updateNumericInput(session, "EN_pred_stand",
                         value = round(mean(prediction_results[,2]),2),
                         max = round(max(prediction_results[,2]),2),
                         min = round(min(prediction_results[,2]),2),
                         step = en_stepr
      )
      
      en_stepdc = round((max(prediction_results[,3])-min(prediction_results[,3]))/40,2)
      
      updateNumericInput(session, "EN_pred_dc",
                         value = round(mean(prediction_results[,3]),2),
                         max = round(max(prediction_results[,3]),2),
                         min = round(min(prediction_results[,3]),2),
                         step = en_stepdc
      )
      
      output$EN_preds = DT::renderDataTable(server = T, {data = datatable(prediction_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp",buttons = c('copy', 'csv', 'excel'),paging = T,
                pageLength = 17,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_coeffs = DT::renderDataTable(server = T, {data = datatable(final_coeffs,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),
                  paging = F,scrollX = F,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                  initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      EN_pred_scat_dat <<- prediction_results[,1:3]
      output$EN_pred_scatplot = renderPlotly(scatter_confuse(EN_pred_scat_dat,input$EN_pred_stand,input$EN_pred_dc))
      
      resid_data = prediction_results[,c(1,3)]
      resid_data = resid_data %>% mutate(Residuals = round(prediction_results[,2]-prediction_results[,3],3))
      output$EN_pred_resid_scatter = renderPlotly(scatter(resid_data))
      
      output$EN_pred_lineplot = renderPlotly(plot_ly(EN_pred_scat_dat, x = ~EN_pred_scat_dat[,1], y = ~EN_pred_scat_dat[,2], name="Observations", type="scatter",
                      mode = "lines",text = ~paste("<b>ID: </b>",EN_pred_scat_dat[,1],"<br><b>Observed Value:</b> ",EN_pred_scat_dat[,2],sep=""),
                      hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5)) %>%
                  add_trace(y = ~EN_pred_scat_dat[,3], name="Predictions", mode = 'lines',text = ~paste("<b>ID: </b>",EN_pred_scat_dat[,1],"<br><b>Prediction:</b> ",
                      round(EN_pred_scat_dat[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                  layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Predictions",
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
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                  round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'EN: Predict')
    }
  })
  
  # Elastic Net fitting
  
  observeEvent(input$EN_stand, {
    
    if (nrow(EN_scat_dat) != 0) {
      
      iv$add_rule("EN_stand", sv_between(min(EN_scat_dat[,2]),max(EN_scat_dat[,2])))
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_scat_dat,input$EN_stand,input$EN_dec_crit))
      
      EN_confuse_results = confuse(EN_scat_dat[,2:3],input$EN_stand,input$EN_dec_crit)
      EN_confuse_table = matrix(0,nrow=1,ncol=4)
      
      EN_confuse_table[1,1] = EN_confuse_results$TP
      EN_confuse_table[1,2] = EN_confuse_results$TN
      EN_confuse_table[1,3] = EN_confuse_results$FP
      EN_confuse_table[1,4] = EN_confuse_results$FN
      
      colnames(EN_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_confuse = DT::renderDataTable(server = T, {data = datatable(EN_confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_confuse_text = renderText({paste0("Sensitivity = ",round(EN_confuse_results$Sensitivity,3),"; Specificity = ",
                  round(EN_confuse_results$Specificity,3),"; Accuracy = ",round(EN_confuse_results$Accuracy,3))})
      
    }
  })
  
  observeEvent(input$EN_dec_crit, {
    
    if (nrow(EN_scat_dat) != 0) {
      
      iv$add_rule("EN_dec_crit", sv_between(min(EN_scat_dat[,3]),max(EN_scat_dat[,3])))
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_scat_dat,input$EN_stand,input$EN_dec_crit))
      
      EN_confuse_results = confuse(EN_scat_dat[,2:3],input$EN_stand,input$EN_dec_crit)
      EN_confuse_table = matrix(0,nrow=1,ncol=4)
      
      EN_confuse_table[1,1] = EN_confuse_results$TP
      EN_confuse_table[1,2] = EN_confuse_results$TN
      EN_confuse_table[1,3] = EN_confuse_results$FP
      EN_confuse_table[1,4] = EN_confuse_results$FN
      
      colnames(EN_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_confuse = DT::renderDataTable(server = T, {data = datatable(EN_confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_confuse_text = renderText({paste0("Sensitivity = ",round(EN_confuse_results$Sensitivity,3),"; Specificity = ",
                                                  round(EN_confuse_results$Specificity,3),"; Accuracy = ",round(EN_confuse_results$Accuracy,3))})
    }
  })
  
  observeEvent(input$EN_fit, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
    }
    
    set.seed(input$model_seed)
    
    MC_runs=input$MC_runs
    
    if (is.null(ignored_rows)) {
      EN_data0 = data
    } else {
      EN_data0 = data[-ignored_rows,]
    }
    
    # REMOVE NA'S FROM RESPONSE VARIABLE
    EN_data0 = EN_data0[!is.na(EN_data0[,rv]),]
    
    var_list = c(rv,which(colnames(EN_data0) %in% feats_to_use))
    EN_data = EN_data0[,var_list]
    colnames(EN_data) = c("Response",feats_to_use)
    
    if (any(is.na(EN_data[,-1]))) {
      
      showModal(modalDialog(paste("Elastic Net does not tolerate missing feature values. You can either Impute these
                (on the Data tab) or Disable rows/columns with missing values."),footer = modalButton("Close")))
      
    } else {
      
      temp_fits = matrix(0, nrow = nrow(EN_data), ncol = 2*MC_runs)
      temp_fits = data.frame(temp_fits)
      
      temp_coeffs = matrix(0, nrow = ncol(EN_data), ncol = MC_runs+1)
      temp_coeffs = data.frame(temp_coeffs)
      temp_coeffs[,1] = c("(Intercept)",feats_to_use)
      
      withProgress(
        message = 'EN Fitting Progress',
        detail = paste("MC runs: ", x = MC_runs),
        value = 0,
        {
          for (i in 1:MC_runs) {
            
            if (input$loggy) {
              
              for (j in 1:nrow(EN_data)){
                if (EN_data[j,1]=="TNTC") {
                  EN_data[j,1]=log10(runif(1, min = rc_lowval, max = rc_upval))
                }
                
                if (EN_data[j,1]=="ND") {
                  EN_data[j,1]=log10(runif(1, min = lc_lowval, max = lc_upval))
                }
              }
            } else {
              
              for (j in 1:nrow(EN_data)){
                if (EN_data[j,1]=="TNTC") {
                  EN_data[j,1]=(runif(1, min = rc_lowval, max = rc_upval))
                }
                
                if (EN_data[j,1]=="ND") {
                  EN_data[j,1]=(runif(1, min = lc_lowval, max = lc_upval))
                }
              }
            }
            
            temp_fits[,2*i-1] = EN_data[,1]
            
            # determine best alpha and lambda
            fit_mod = cva.glmnet(x=as.matrix(EN_data[,-1]),y=EN_data[,1],nfolds=input$num_folds,na.action="na.omit",
                                 standardize=input$EN_standard,intercept=TRUE)
            
            get_model_params <- function(fit) {
              alpha <- fit$alpha
              lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
              lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
              error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
              best <- which.min(error)
              data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
                         lambdaSE = lambdaSE[best], eror = error[best])
            }
            
            alpha = get_model_params(fit_mod)$alpha
            lambda = get_model_params(fit_mod)$lambdaMin
            
            en_model <<- glmnet(x=as.matrix(EN_data[,-1]),EN_data[,1],lambda=lambda, alpha=alpha, na.action="na.omit",
                           standardize=input$EN_standard,intercept=TRUE)
            
            coeffs = as.matrix(coef(en_model,s=lambda))
            coeffs = as.data.frame(coeffs)
            temp_coeffs[,i+1] = coeffs
            
            fits = predict(en_model, newx = as.matrix(EN_data[,-1]))
            temp_fits[,2*i] = round(fits,3)
            
            incProgress(1/MC_runs, detail = paste("MC run:",i,"/",MC_runs))
          }
      })
      
      #Must add: Fit final/global EN_model
      
      mean_coeffs = round(rowMeans(temp_coeffs[,-1]),4)
      EN_coeffs = data.frame(cbind(temp_coeffs[,1],mean_coeffs))
      colnames(EN_coeffs) = c("Feature","Coefficient")
      
      even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
      odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
      
      obs_mean_values = rowMeans(odd_columns)
      fit_mean_values = rowMeans(even_columns)
      EN_results = data.frame(cbind(EN_data0[,1],round(obs_mean_values,3),round(fit_mean_values,3),round(EN_data[,-1],4)))
      colnames(EN_results) = c(colnames(EN_data0)[[1]],colnames(EN_data)[[1]],"Fitted_Value",colnames(EN_data[,-1]))
      
      en_stepr = round((max(EN_results[,2])-min(EN_results[,2]))/40,2)
      
      updateNumericInput(session, "EN_stand",
                         value = round(mean(EN_results[,2]),2),
                         max = round(max(EN_results[,2]),2),
                         min = round(min(EN_results[,2]),2),
                         step = en_stepr)
      
      en_stepdc = round((max(EN_results[,3])-min(EN_results[,3]))/40,2)
      
      updateNumericInput(session, "EN_dec_crit",
                         value = round(mean(EN_results[,3]),2),
                         max = round(max(EN_results[,3]),2),
                         min = round(min(EN_results[,3]),2),
                         step = en_stepdc)
      
      output$EN_fits = DT::renderDataTable(server = T, {data = datatable(EN_results,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F,dom="ltBp", buttons = c('copy', 'csv', 'excel'),
                  paging = T,pageLength = 17,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_coeffs = DT::renderDataTable(server = T, {data = datatable(EN_coeffs,rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),
                  paging = F,scrollX = F,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      EN_scat_dat <<- EN_results[,1:3]
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_scat_dat,input$EN_stand,input$EN_dec_crit))
      
      resid_data = EN_results[,c(1,3)]
      resid_data = resid_data %>% mutate(Residuals = round(EN_results[,2]-EN_results[,3],3))
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
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_confuse_text = renderText({paste0("Sensitivity = ",round(EN_confuse_results$Sensitivity,3),"; Specificity = ",
                  round(EN_confuse_results$Specificity,3),"; Accuracy = ",round(EN_confuse_results$Accuracy,3))})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'EN: Fitting')
    }
  })
}

shinyApp(ui, server)