library(shiny)
library(bslib)
library(bsplus)
library(cluster)
library(corrplot)
library(future)
library(ggdist)
library(ggplot2)
library(glmnetUtils)
library(grid)
library(hash)
library(iml)
library(ipc)
library(isotree)
library(leaflet)
library(lubridate)
library(magrittr)
library(Matrix)
library(Metrics)
library(missForest)
library(Nmisc)
library(openxlsx)
library(plotly)
library(plyr)
library(promises)
library(pso)
library(purrr)
library(RSQLite)
library(SHAPforxgboost)
library(shinyjs)
library(shinythemes)
library(shinyvalidate)
library(shinyWidgets)
library(stringr)
library(tidyverse)
library(xgboost)
library(DT)

# library(NCmisc)
# library(here)

plan(multicore)

source("global_functions.R")
source("ui.R")
source("app_variables.R")
source("createAO.R")
source("confusion.R")
source("input_validation.R")
source("lineplot.R")
source("map_click.R")
source("rain.R")
source("renderdata.R")
source("renderPCAdata.R")
source("renderpreddata.R")
source("resid_scatter.R")
source("scatter.R")
source("scatter_confuse.R")
source("xgb_call_predict.R")
source("xgb_feature_selection.R")
source("xgb_pred_errors.R")
source("xgb_pso.R")
source("xgbcl_call_predict.R")
source("xgbcl_feature_selection.R")
source("xgbcl_pred_errors.R")
source("xgbcl_pso.R")

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
  # print(packages)
  
  iv = InputValidator$new()
  add_validation_rules(iv)
  iv$enable()

  # Prediction tab text directions
  output$directions = renderUI({
    req(bo())
    tagList(
      tags$h5("IMPORTANT INFO:"),
      tags$ul(
        style = "list-style-type: disc; padding-left: 1.2em;",
        tags$li("Left-most columns of imported data file MUST be 1) Unique ID and 2) Response Variable. These 2 columns can have any name and can contain missing data."),
        tags$li("Imported data file MUST include column names EXACTLY equal to the name of features present in the chosen model."),
        tags$li("Ordering of feature columns in the imported data file not important; all unmatched feature columns will be ignored."),
        tags$li("If the chosen model contains A/O components, enter magnitude/direction data; the needed A/O components will be computed by VB."),
        tags$li("If the chosen model contains transformed features, enter only the raw, untransformed feature data; the needed transform will be done by VB.")
        ))
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
      
    } else if (any(sapply(data.frame(init_data[,-1]), function(col) !is.numeric(col)))) {
      
      showModal(modalDialog(paste("This dataset contains non-numeric data. Please remedy prior to data importation."),
                            easyClose = F,footer = div(modalButton('Close'))))
      
    } else {
      col_props_temp = hash()
      
      for (i in 1:ncol(init_data)) {
        .set(col_props_temp,keys=colnames(init_data)[i],values=2)
        #.set(col_props_temp,keys=colnames(init_data)[i],values=c(prop1=2,prop2=NA,prop3=NA,prop4=NA))
      }
      
      init_column_props <<- col_props_temp
      column_props <<- col_props_temp
      ignored_rows <<- NULL
      PCA_dataset(NULL)
      changed_model(TRUE)
      
      if (input$IDasDate == "YMD") {
        init_ID_format <<- "YMD"
        init_data[,1] = ymd(init_data[,1])
        date_format_string <<- "toLocaleDateString"
      } else if (input$IDasDate == "MDY") {
        init_ID_format <<- "MDY"
        init_data[,1] = mdy(init_data[,1])
        date_format_string <<- "toLocaleDateString"
      } else if (input$IDasDate == "MDYHM") {
        init_ID_format <<- "MDYHM"
        init_data[,1] = parse_date_time(init_data[,1],c('%m/%d/%y %H:%M'),exact=TRUE)
        date_format_string <<- "toLocaleString"
      } else if (input$IDasDate == "Character") {
        init_ID_format <<- "Character"
        date_format_string <<- "Character"
      } else if (input$IDasDate == "Numeric") {
        init_ID_format <<- "Numeric"
        date_format_string <<- "Numeric"
      }
      
      current_data(init_data)
      col_names(colnames(init_data[,-1]))
      
      clear_trans_table(drop_transforms = TRUE, drop_interactions = TRUE, drop_AO = TRUE)
      
      # Filter the response variable to exclude left and right-censored tags
      exclude_values = c(input$lc_val, input$rc_val)
      real_responses = na.omit(init_data[!init_data[,response_var()] %in% exclude_values,response_var()])
      
      updateNumericInput(session, "LG_binarize_crit_value",value = round(median(real_responses),2),
                         min=min(real_responses),max=max(real_responses))
      updateNumericInput(session, "XGBCL_binarize_crit_value",value = round(median(real_responses),2),
                         min=min(real_responses),max=max(real_responses))
      
      updateNumericInput(session,"lc_replace",value = round(min(real_responses),3))
      updateNumericInput(session,"rc_replace",value = round(max(real_responses),3))
      
      iv$add_rule("LG_binarize_crit_value", sv_between(min(real_responses),max(real_responses)))
      iv$add_rule("XGBCL_binarize_crit_value", sv_between(min(real_responses),max(real_responses)))
      
      feat_data = init_data[,3:ncol(init_data)]
      
      feats_being_used(colnames(feat_data))
      feat_names(colnames(feat_data))
      
      enable("restore")
      enable("set_column_props")
      enable("corr_check")
      enable("transforms")
      enable("interacts")
      enable("pca_check")
      enable("run_iso_forest")
      
      pca_axes_max(ncol(init_data)-2)
      
      updateNumericInput(session, "num_axes",
                         value = pca_axes_max(),
                         max = pca_axes_max())
      
      updateSelectInput(session,"set_column_props",choices=c("-",col_names()))
      updateSelectInput(session,"rainplot",choices=c("-",col_names()))
      updateSelectInput(session,"lineplot",choices=c("-",col_names()))
      updateSelectInput(session,"scatterx",choices=c("-",col_names()))
      updateSelectInput(session,"scattery",choices=c("-",col_names()))
      updateSelectInput(session,"speed",choices=c("-",col_names()))
      updateSelectInput(session,"direct",choices=c("-",col_names()))
      
      current_data_page(1)
      
      renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
      
      clear_modeling(TRUE)

      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = "Data Table")
      
    }
  })
  
  # Create a temporary SQL database
  temp_db = dbConnect(RSQLite::SQLite(), ":memory:")
  
  # Render leaflet map
  output$map = renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(270, 40, zoom = 5)
  })
  
  # Compute beach orientation based on map clicks
  observeEvent(input$map_click,  ignoreInit = T, {map_click(input$map_click,map_clicks,bo)})
  
  # Add/Remove shoreline markers
  observeEvent(input$show_shorelines, ignoreInit = T, {
    
    if (input$show_shorelines) {
      
      leafletProxy("map") %>% clearGroup('shore_markers')
      
      zoom = input$map_zoom
      zoom_threshold = 12
      bounds = input$map_bounds
      
      if (zoom < zoom_threshold) {
        
        updateSwitchInput(session,"show_shorelines",value = FALSE)
        showModal(modalDialog(tags$h4("Zoom to at least level 12 to show beach shorelines."),easyClose = FALSE))
        
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
    } else {
      
      leafletProxy("map") %>% clearGroup('shore_markers')
    }
  })
  
  # Add/Remove monitoring station markers
  observeEvent(input$show_stations, ignoreInit = T, {
    
    if (input$show_stations) {
      
      leafletProxy("map") %>% clearGroup('monitor_stations')
      
      zoom = input$map_zoom
      zoom_threshold = 11
      bounds = input$map_bounds
      
      if (zoom < zoom_threshold) {
        
        updateSwitchInput(session,"show_stations",value = FALSE)
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
    } else {
      
      leafletProxy("map") %>% clearGroup('monitor_stations')
    }
  })
  
  # Manually change beach orientation
  observeEvent(input$beach_angle, ignoreInit = T, {
    bo(input$beach_angle)
    output$beach_orient = renderText({
      req(bo())
      sprintf("%d", as.integer(round(bo(), 0)))
    })
  })
  
  # Other map stuff
  observeEvent(input$map_marker_click, ignoreInit = T, {
    
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
      A third click, <b>made in the water</b>, calculates/saves the site orientation. A fourth click re-starts the process.
         <br><br><i>Note: A newly-calculated orientation replaces the previous one.</i>")
  })
  
  output$zoom_level = renderText({input$map_zoom})
  
  observeEvent(input$map_zoom, ignoreInit = T, {
    
    zoom_shoreline_thresh = 12
    zoom_marker_thresh = 11
    
    if (input$map_zoom < zoom_shoreline_thresh) {
      updateSwitchInput(session,"show_shorelines",value = FALSE)
      leafletProxy("map") %>% clearGroup('shore_markers')
    }
    
    if (input$map_zoom < zoom_marker_thresh) {
      updateSwitchInput(session,"show_stations",value = FALSE)
      leafletProxy("map") %>% clearGroup('monitor_stations')
    }
  })
  
  output$beach_orient = renderText({
    req(bo())
    sprintf("%d", as.integer(round(bo(), 0)))
  })
  
  # Save Project File
  observeEvent(c(input$save_project_data,input$save_project_modeling,input$save_project_prediction), ignoreInit=T, {
    
    if (is.null(init_data)) {
      showModal(modalDialog(
        title = NULL,
        "No data have been imported yet.",
        footer = NULL,
        easyClose = TRUE
      ))
      
    } else {

      tempFile = tempfile(fileext = ".RData")
      
      save_list = list(
        type = "Project",
        Version = version,
        temp_db = temp_db,
        bo = bo(),
        current_data = current_data(),
        response_var = response_var(),
        col_names = col_names(),
        feat_names = feat_names(),
        feats_being_used = feats_being_used(),
        fs_feats_used = fs_feats_used(),
        init_data = init_data,
        ignored_rows = ignored_rows,
        init_ID_format = init_ID_format,
        date_format_string = date_format_string,
        saved_lc_val = input$lc_val,
        saved_rc_val = input$rc_val,
        saved_num_axes = input$num_axes,
        init_column_props = init_column_props,
        column_props = column_props,
        PCA_scaling_mean = PCA_scaling_mean(),
        PCA_scaling_sd   = PCA_scaling_sd(),
        PCA_dataset = PCA_dataset(),
        PCA_summary_df = PCA_summary_df(),
        PCA_coefficients = PCA_coefficients(),
        pca_axes_max = pca_axes_max(),
        pca_axes = pca_axes(),
        pcax_being_used = pcax_being_used(),
        fs_pcax_used = fs_pcax_used(),
        final_model_PCA = final_model_PCA(),
        rv_ao_map = shiny::reactiveValuesToList(rv_ao_map, all.names = TRUE),
        rv_pred = { p <- shiny::reactiveValuesToList(rv_pred, all.names = TRUE); p$pending <- NULL; p },
        poly_coeffs = as.list(POLY_COEFFS),
        
        LG_pred_results = LG_pred_results(),
        LG_pred_coeffs = LG_pred_coeffs(),
        LG_pred_confuse_results = LG_pred_confuse_results(),
        LG_pred_scat_dat = LG_pred_scat_dat(),
        LG_pred_standardize = LG_pred_standardize(),
        LG_pred_thresh = LG_pred_thresh(),
        LG_results = LG_results(),
        LG_coeffs = LG_coeffs(),
        LG_confuse_results = LG_confuse_results(),
        LG_scat_dat = LG_scat_dat(),
        LG_model = LG_model,
        LG_thresh = LG_thresh(),
        LG_crit_prob = LG_crit_prob(),
        LG_standardize = LG_pred_standardize(),
        LG_model_PCA = LG_model_PCA(),
        LG_final_features = LG_final_features(),
        LG_pred_data = LG_pred_data(),
        
        XGBCL_pred_results = XGBCL_pred_results(),
        XGBCL_pred_coeffs = XGBCL_pred_coeffs(),
        XGBCL_pred_confuse_results = XGBCL_pred_confuse_results(),
        XGBCL_pred_scat_dat = XGBCL_pred_scat_dat(),
        XGBCL_pred_standardize = XGBCL_pred_standardize(),
        XGBCL_pred_thresh = XGBCL_pred_thresh(),
        XGBCL_selection_results = XGBCL_selection_results(),
        XGBCL_results = XGBCL_results(),
        XGBCL_coeffs = XGBCL_coeffs(),
        XGBCL_confuse_results = XGBCL_confuse_results(),
        XGBCL_scat_dat = XGBCL_scat_dat(),
        XGBCL_model = XGBCL_model,
        XGBCL_thresh = XGBCL_thresh(),
        XGBCL_crit_prob = XGBCL_crit_prob(),
        XGBCL_standardize = XGBCL_standardize(),
        XGBCL_model_PCA = XGBCL_model_PCA(),
        XGBCL_final_features = XGBCL_final_features(),
        XGBCL_pred_data = XGBCL_pred_data(),
        XGBCL_final_data = XGBCL_final_data(),
        Optimal_CLHP = Optimal_CLHP,
        
        XGB_pred_results = XGB_pred_results(),
        XGB_pred_coeffs = XGB_pred_coeffs(),
        XGB_pred_confuse_results = XGB_pred_confuse_results(),
        XGB_pred_scat_dat = XGB_pred_scat_dat(),
        XGB_pred_standardize = XGB_pred_standardize(),
        XGB_selection_results = XGB_selection_results(),
        XGB_results = XGB_results(),
        XGB_coeffs = XGB_coeffs(),
        XGB_confuse_results = XGB_confuse_results(),
        XGB_stand = input$XGB_stand,
        XGB_dec_crit = input$XGB_dec_crit,
        XGB_scat_dat = XGB_scat_dat(),
        XGB_model = XGB_model,
        XGB_standardize = XGB_standardize(),
        XGB_model_PCA = XGB_model_PCA(),
        XGB_final_features = XGB_final_features(),
        XGB_pred_data = XGB_pred_data(),
        XGB_final_data = XGB_final_data(),
        Optimal_HP = Optimal_HP,
        
        EN_pred_results = EN_pred_results(),
        EN_pred_coeffs = EN_pred_coeffs(),
        EN_pred_confuse_results = EN_pred_confuse_results(),
        EN_pred_scat_dat = EN_pred_scat_dat(),
        EN_pred_standardize = EN_pred_standardize(),
        EN_results = EN_results(),
        EN_coeffs = EN_coeffs(),
        EN_confuse_results = EN_confuse_results(),
        EN_stand = input$EN_stand,
        EN_dec_crit = input$EN_dec_crit,
        EN_scat_dat = EN_scat_dat(),
        EN_model = EN_model,
        EN_standardize = EN_standardize(),
        EN_model_PCA = EN_model_PCA(),
        EN_final_features = EN_final_features(),
        EN_pred_data = EN_pred_data()
      )
      
      save(save_list, file = tempFile)
      
      resourcePath = paste0("download_", basename(tempFile))
      addResourcePath(resourcePath, dirname(tempFile))
      
      url = paste0(
        session$clientData$url_protocol, "//",
        session$clientData$url_hostname, ":",
        session$clientData$url_port, "/",
        resourcePath, "/", basename(tempFile)
      )
      
      session$sendCustomMessage(type = 'download', list(
        filename = "Project_File.RData",
        url = url
      ))
    }
  })
  
  # Save Prediction File
  output$save_prediction = downloadHandler(filename = function() {paste("Prediction_File.RData")}, content = function(file) {
    
    save_list = list(
      type = "Prediction",
      Version = version,
      temp_db = temp_db,
      bo = bo(),
      current_data = current_data(),
      response_var = response_var(),
      col_names = col_names(),
      feat_names = feat_names(),
      feats_being_used = feats_being_used(),
      fs_feats_used = fs_feats_used(),
      init_data = init_data,
      ignored_rows = ignored_rows,
      init_ID_format = init_ID_format,
      date_format_string = date_format_string,
      saved_lc_val = input$lc_val,
      saved_rc_val = input$rc_val,
      saved_num_axes = input$num_axes,
      init_column_props = init_column_props,
      column_props = column_props,
      PCA_scaling_mean = PCA_scaling_mean(),
      PCA_scaling_sd   = PCA_scaling_sd(),
      PCA_dataset = PCA_dataset(),
      PCA_summary_df = PCA_summary_df(),
      PCA_coefficients = PCA_coefficients(),
      pca_axes_max = pca_axes_max(),
      pca_axes = pca_axes(),
      pcax_being_used = pcax_being_used(),
      fs_pcax_used = fs_pcax_used(),
      final_model_PCA = final_model_PCA(),
      rv_ao_map = shiny::reactiveValuesToList(rv_ao_map, all.names = TRUE),
      rv_pred = { p <- shiny::reactiveValuesToList(rv_pred, all.names = TRUE); p$pending <- NULL; p },
      poly_coeffs = as.list(POLY_COEFFS),
      
      LG_pred_results = LG_pred_results(),
      LG_pred_coeffs = LG_pred_coeffs(),
      LG_pred_confuse_results = LG_pred_confuse_results(),
      LG_pred_scat_dat = LG_pred_scat_dat(),
      LG_pred_standardize = LG_pred_standardize(),
      LG_pred_thresh = LG_pred_thresh(),
      LG_results = LG_results(),
      LG_coeffs = LG_coeffs(),
      LG_confuse_results = LG_confuse_results(),
      LG_scat_dat = LG_scat_dat(),
      LG_model = LG_model,
      LG_thresh = LG_thresh(),
      LG_crit_prob = LG_crit_prob(),
      LG_standardize = LG_pred_standardize(),
      LG_model_PCA = LG_model_PCA(),
      LG_final_features = LG_final_features(),
      LG_pred_data = LG_pred_data(),
      
      XGBCL_pred_results = XGBCL_pred_results(),
      XGBCL_pred_coeffs = XGBCL_pred_coeffs(),
      XGBCL_pred_confuse_results = XGBCL_pred_confuse_results(),
      XGBCL_pred_scat_dat = XGBCL_pred_scat_dat(),
      XGBCL_pred_standardize = XGBCL_pred_standardize(),
      XGBCL_pred_thresh = XGBCL_pred_thresh(),
      XGBCL_selection_results = XGBCL_selection_results(),
      XGBCL_results = XGBCL_results(),
      XGBCL_coeffs = XGBCL_coeffs(),
      XGBCL_confuse_results = XGBCL_confuse_results(),
      XGBCL_scat_dat = XGBCL_scat_dat(),
      XGBCL_model = XGBCL_model,
      XGBCL_thresh = XGBCL_thresh(),
      XGBCL_crit_prob = XGBCL_crit_prob(),
      XGBCL_standardize = XGBCL_standardize(),
      XGBCL_model_PCA = XGBCL_model_PCA(),
      XGBCL_final_features = XGBCL_final_features(),
      XGBCL_pred_data = XGBCL_pred_data(),
      XGBCL_final_data = XGBCL_final_data(),
      Optimal_CLHP = Optimal_CLHP,
      
      XGB_pred_results = XGB_pred_results(),
      XGB_pred_coeffs = XGB_pred_coeffs(),
      XGB_pred_confuse_results = XGB_pred_confuse_results(),
      XGB_pred_scat_dat = XGB_pred_scat_dat(),
      XGB_pred_standardize = XGB_pred_standardize(),
      XGB_selection_results = XGB_selection_results(),
      XGB_results = XGB_results(),
      XGB_coeffs = XGB_coeffs(),
      XGB_confuse_results = XGB_confuse_results(),
      XGB_stand = input$XGB_stand,
      XGB_dec_crit = input$XGB_dec_crit,
      XGB_scat_dat = XGB_scat_dat(),
      XGB_model = XGB_model,
      XGB_standardize = XGB_standardize(),
      XGB_model_PCA = XGB_model_PCA(),
      XGB_final_features = XGB_final_features(),
      XGB_pred_data = XGB_pred_data(),
      XGB_final_data = XGB_final_data(),
      Optimal_HP = Optimal_HP,
      
      EN_pred_results = EN_pred_results(),
      EN_pred_coeffs = EN_pred_coeffs(),
      EN_pred_confuse_results = EN_pred_confuse_results(),
      EN_pred_scat_dat = EN_pred_scat_dat(),
      EN_pred_standardize = EN_pred_standardize(),
      EN_results = EN_results(),
      EN_coeffs = EN_coeffs(),
      EN_confuse_results = EN_confuse_results(),
      EN_stand = input$EN_stand,
      EN_dec_crit = input$EN_dec_crit,
      EN_scat_dat = EN_scat_dat(),
      EN_model = EN_model,
      EN_standardize = EN_standardize(),
      EN_model_PCA = EN_model_PCA(),
      EN_final_features = EN_final_features(),
      EN_pred_data = EN_pred_data()
    )
    
    save(save_list, file = file)
  })
  
  # Load project/prediction file
  observeEvent(input$load_file, ignoreInit = TRUE, {
    req(input$load_file)
    
    load_element <- input$load_file
    temp_env <- new.env()
    load(load_element$datapath, envir = temp_env)
    
    if (!exists("save_list", envir = temp_env)) {
      showModal(modalDialog(title = NULL, "Invalid project file.", footer = NULL, easyClose = TRUE))
      return()
    }
    
    sl <- temp_env$save_list
    
    if (!identical(sl$Version, "1.0.0")) {
      showModal(modalDialog(title = NULL, "Unsupported project file version.", footer = NULL, easyClose = TRUE))
      return()
    }
    
    # Restore polynomial coefficients (remove duplicate block using save_list)
    if (!is.null(sl$poly_coeffs)) {
      rm(list = ls(envir = POLY_COEFFS), envir = POLY_COEFFS)
      for (feat in names(sl$poly_coeffs)) {
        vals <- sl$poly_coeffs[[feat]]
        if (is.numeric(vals) && length(vals) == 3L) {
          set_poly_coeffs(feat, vals[1], vals[2], vals[3])
        }
      }
    }
    
      temp_db <<- sl$temp_db
      bo(sl$bo)
      current_data(sl$current_data)
      response_var(sl$response_var)
      col_names(sl$col_names)
      feat_names(sl$feat_names)
      feats_being_used(sl$feats_being_used)
      fs_feats_used(sl$fs_feats_used)
      init_data <<- sl$init_data
      ignored_rows <<- sl$ignored_rows
      init_ID_format <<- sl$init_ID_format
      date_format_string <<- sl$date_format_string
      saved_lc_val = sl$saved_lc_val
      saved_rc_val = sl$saved_rc_val
      saved_num_axes = sl$saved_num_axes
      init_column_props <<- sl$init_column_props
      column_props <<- sl$column_props
      PCA_scaling_mean(sl$PCA_scaling_mean)
      PCA_scaling_sd(sl$PCA_scaling_sd)
      PCA_dataset(sl$PCA_dataset)
      PCA_summary_df(sl$PCA_summary_df)
      PCA_coefficients(sl$PCA_coefficients)
      pca_axes_max(sl$pca_axes_max)
      pca_axes(sl$pca_axes)
      pcax_being_used(sl$pcax_being_used)
      fs_pcax_used(sl$fs_pcax_used)
      final_model_PCA(sl$final_model_PCA)
      
      if (!is.null(sl$rv_pred) && is.list(sl$rv_pred)) {
        pred_state = sl$rv_pred
        for (nm in names(pred_state)) {
          rv_pred[[nm]] = pred_state[[nm]]
        }
      }
      
      if (!is.null(sl$rv_ao_map) && is.list(sl$rv_ao_map)) {
        ao_map = sl$rv_ao_map
        for (nm in names(ao_map)) {
          rv_ao_map[[nm]] = ao_map[[nm]]
        }
      }
      
      LG_pred_results(sl$LG_pred_results)
      LG_pred_coeffs(sl$LG_pred_coeffs)
      LG_pred_confuse_results(sl$LG_pred_confuse_results)
      LG_pred_scat_dat(sl$LG_pred_scat_dat)
      LG_pred_standardize(sl$LG_pred_standardize)
      LG_pred_thresh(sl$LG_pred_thresh)
      LG_results(sl$LG_results)
      LG_coeffs(sl$LG_coeffs)
      LG_confuse_results(sl$LG_confuse_results)
      LG_scat_dat(sl$LG_scat_dat)
      LG_model <<- sl$LG_model
      LG_thresh(sl$LG_thresh)
      LG_crit_prob(sl$LG_crit_prob)
      LG_standardize(sl$LG_standardize)
      LG_model_PCA(sl$LG_model_PCA)
      LG_final_features(sl$LG_final_features)
      LG_pred_data(sl$LG_pred_data)
      
      XGBCL_pred_results(sl$XGBCL_pred_results)
      XGBCL_pred_coeffs(sl$XGBCL_pred_coeffs)
      XGBCL_pred_confuse_results(sl$XGBCL_pred_confuse_results)
      XGBCL_pred_scat_dat(sl$XGBCL_pred_scat_dat)
      XGBCL_pred_standardize(sl$XGBCL_pred_standardize)
      XGBCL_pred_thresh(sl$XGBCL_pred_thresh)
      XGBCL_selection_results(sl$XGBCL_selection_results)
      XGBCL_results(sl$XGBCL_results)
      XGBCL_coeffs(sl$XGBCL_coeffs)
      XGBCL_confuse_results(sl$XGBCL_confuse_results)
      XGBCL_scat_dat(sl$XGBCL_scat_dat)
      XGBCL_model <<- sl$XGBCL_model
      XGBCL_thresh(sl$XGBCL_thresh)
      XGBCL_crit_prob(sl$XGBCL_crit_prob)
      XGBCL_standardize(sl$XGBCL_standardize)
      XGBCL_model_PCA(sl$XGBCL_model_PCA)
      XGBCL_final_features(sl$XGBCL_final_features)
      XGBCL_pred_data(sl$XGBCL_pred_data)
      XGBCL_final_data(sl$XGBCL_final_data)
      Optimal_CLHP <<- sl$Optimal_CLHP
      
      XGB_pred_results(sl$XGB_pred_results)
      XGB_pred_coeffs(sl$XGB_pred_coeffs)
      XGB_pred_confuse_results(sl$XGB_pred_confuse_results)
      XGB_pred_scat_dat(sl$XGB_pred_scat_dat)
      XGB_pred_standardize(sl$XGB_pred_standardize)
      XGB_selection_results(sl$XGB_selection_results)
      XGB_results(sl$XGB_results)
      XGB_coeffs(sl$XGB_coeffs)
      XGB_confuse_results(sl$XGB_confuse_results)
      XGB_scat_dat(sl$XGB_scat_dat)
      XGB_model <<- sl$XGB_model
      XGB_standardize(sl$XGB_standardize)
      XGB_model_PCA(sl$XGB_model_PCA)
      XGB_final_features(sl$XGB_final_features)
      XGB_pred_data(sl$XGB_pred_data)
      XGB_final_data(sl$XGB_final_data)
      Optimal_HP <<- sl$Optimal_HP
      
      EN_pred_results(sl$EN_pred_results)
      EN_pred_coeffs(sl$EN_pred_coeffs)
      EN_pred_confuse_results(sl$EN_pred_confuse_results)
      EN_pred_scat_dat(sl$EN_pred_scat_dat)
      EN_pred_standardize(sl$EN_pred_standardize)
      EN_results(sl$EN_results)
      EN_coeffs(sl$EN_coeffs)
      EN_confuse_results(sl$EN_confuse_results)
      EN_scat_dat(sl$EN_scat_dat)
      EN_model <<- sl$EN_model
      EN_model_PCA(sl$EN_model_PCA)
      EN_standardize(sl$EN_standardize)
      EN_final_features(sl$EN_final_features)
      EN_pred_data(sl$EN_pred_data)
      
      if (init_ID_format == "YMD") {
        init_data[,1] = ymd(init_data[,1])
        date_format_string <<- "toLocaleDateString"
      } else if (init_ID_format == "MDY") {
        init_data[,1] = mdy(init_data[,1])
        date_format_string <<- "toLocaleDateString"
      } else if (init_ID_format == "MDYHM") {
        init_data[,1] = parse_date_time(init_data[,1],c('%m/%d/%y %H:%M'),exact=TRUE)
        date_format_string <<- "toLocaleString"
      } else if (init_ID_format == "Character") {
        date_format_string <<- "Character"
      } else if (init_ID_format == "Numeric") {
        date_format_string <<- "Numeric"
      }
      
      temp_data = current_data()
      temp_data[,1] = init_data[,1]
      current_data(temp_data)
      
      names = c("Logistic_Regression","XGB_Classifier","XGBoost","Elastic_Net")
      created = list(LG_results(),XGBCL_results(),XGB_results(),EN_results())
      available = c()
      
      for (i in 1:4) {
        if (!is.null(created[[i]])) {
          available = c(available,names[i])
        } else {
          available = available
        }
      }
      
      models_created(available)
      updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
      
      if (final_model_PCA()) {
        output$pca_model_text = renderText({HTML("NOTE: PCA axes being used as features.")})
      } else {
        output$pca_model_text = NULL
      }
      
      refresh_trigger(TRUE)
      
      if (sl$type == "Project") {
        
        session$sendCustomMessage(type = 'enableTabs', message = list(action = 'enable'))
        updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Prediction')
        
        # Update Non-Prediction Tab components
        
        if (!is.null(current_data())) {
          
          exclude_values = c(saved_lc_val, saved_rc_val)
          real_responses = na.omit(init_data[!init_data[,response_var()] %in% exclude_values,response_var()])
          
          updateNumericInput(session, "LG_binarize_crit_value",value = round(median(real_responses),2),
                             min=min(real_responses),max=max(real_responses))
          updateNumericInput(session, "XGBCL_binarize_crit_value",value = round(median(real_responses),2),
                             min=min(real_responses),max=max(real_responses))
          
          updateNumericInput(session,"lc_replace",value = round(min(real_responses),3))
          updateNumericInput(session,"rc_replace",value = round(max(real_responses),3))
          
          enable("restore")
          enable("set_column_props")
          enable("corr_check")
          enable("transforms")
          enable("pca_check")
          enable("run_iso_forest")
          enable("save_project")
          
          updateNumericInput(session, "num_axes",value = pca_axes_max(),max = pca_axes_max())
          updateSelectInput(session,"set_column_props",choices=c("-",col_names()))
          updateSelectInput(session,"rainplot",choices=c("-",col_names()))
          updateSelectInput(session,"lineplot",choices=c("-",col_names()))
          updateSelectInput(session,"scatterx",choices=c("-",col_names()))
          updateSelectInput(session,"scattery",choices=c("-",col_names()))
          updateSelectInput(session,"speed",choices=c("-",col_names()))
          updateSelectInput(session,"direct",choices=c("-",col_names()))
          
          # Render the main data table
          current_data_page(1)
          renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
          
          output$XGBCL_optim_hp = DT::renderDataTable(server=T,{data = datatable(Optimal_CLHP,rownames=F,extensions='Buttons',selection=list(selected =
                            list(rows = NULL, cols = NULL),target = "row",mode="single"),editable=F,options = list(autoWidth=F,dom='tB',paging = F,pageLength = 5,scrollX = F,
                            scrollY = F,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                            initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
          
          output$XGB_optim_hp = DT::renderDataTable(server=T,{data = datatable(Optimal_HP,rownames=F,extensions='Buttons',selection=list(selected =
                            list(rows = NULL, cols = NULL),target = "row",mode="single"),editable=F,options = list(autoWidth=F,dom='tB',paging = F,pageLength = 5,scrollX = F,
                            scrollY = F,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                            initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
          
          updateCheckboxGroupButtons(session,"feats_to_use",choices=feat_names(),selected=feats_being_used(),size="xs",status = "custom")
          updateCheckboxGroupButtons(session,"feats_to_corr",choices=feat_names(),selected=NULL,size="xs",status = "custom")
          updateCheckboxGroupButtons(session,"pcax_to_use",choices=pca_axes(),selected=pcax_being_used(),size="xs",status = "custom")
          
        }
        
        updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
        updateTabsetPanel(session, inputId = 'data_tabs', selected = "Data Table")
        
        session$sendCustomMessage(type = 'enableTabs', message = list(action = 'enable'))
        
      } else if (sl$type == "Prediction") {
        
        session$sendCustomMessage(type = 'disableTabs', message = list(action = 'disable'))
        updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Prediction')
      }
  })
  
  # Create a feature correlation matrix
  observeEvent(input$corr_check, ignoreInit = T, {
    
    showModal(modalDialog(title="Choose Features to Examine", card(
      
      checkboxGroupButtons(
        inputId = "feats_to_corr",
        label = NULL,
        choices = feat_names(),
        size = "sm",
        selected = NULL,
        status = "custom"
      )),
      footer = div(align="center",actionButton("run_corr", "Generate Correlations"),modalButton('Close'))))
  })
  
  observeEvent(input$run_corr, ignoreInit = T, {
    removeModal()
    
    if (is.null(ignored_rows)) {
      corr_data = current_data()[,input$feats_to_corr,drop = FALSE]
    } else {
      corr_data = current_data()[,input$feats_to_corr,drop = FALSE][-ignored_rows,]
    }
    
    data_corrs = cor(corr_data,use="pairwise.complete.obs")
    
    output$corrplot = renderPlot({corrplot(data_corrs, addCoef.col = 'black', method="circle", cl.pos = 'n',is.corr = FALSE,
                                           type="lower",col.lim = c(-1.4, 1.4),col = COL2('PRGn'), tl.col="black", tl.srt= 45)},height = 900, width = 900)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Correlations")
  })
  
  # Create a feature transformation data table
  observeEvent(input$transforms, ignoreInit = TRUE, {
    # IMPORTANT: clear only transformed columns; KEEP interactions and A/O so they can be transformed
    clear_trans_table(drop_transforms = TRUE, drop_interactions = FALSE, drop_AO = FALSE)
    
    # Transform with index assignment to avoid evaluating logs on invalid values
    transform_feature <- function(x, type) {
      n   <- length(x)
      out <- rep(NA_real_, n)
      if (!is.numeric(x)) return(out)
      
      if (type == "None") {
        out <- x
        
      } else if (type == "Log10") {
        idx_pos  <- which(is.finite(x) & x > 0)
        idx_neg  <- which(is.finite(x) & x < 0)
        idx_zero <- which(is.finite(x) & x == 0)
        if (length(idx_pos))  out[idx_pos]  <- log10(x[idx_pos])
        if (length(idx_neg))  out[idx_neg]  <- -log10(abs(x[idx_neg]))
        if (length(idx_zero)) out[idx_zero] <- 0
        
      } else if (type == "Inverse") {
        x2 <- x
        idx_fin <- is.finite(x2)
        idx_nz  <- which(idx_fin & x2 != 0)
        if (length(idx_nz)) {
          min_nz <- min(abs(x2[idx_nz]), na.rm = TRUE)
          idx0   <- which(idx_fin & x2 == 0)
          if (length(idx0)) x2[idx0] <- 0.5 * min_nz
          idx_inv <- which(is.finite(x2) & x2 != 0)
          if (length(idx_inv)) out[idx_inv] <- 1 / x2[idx_inv]
        }
        
      } else if (type == "Square") {
        out <- x^2
        
      } else if (type == "Square Root") {
        idx_fin <- is.finite(x)
        out[idx_fin] <- sign(x[idx_fin]) * sqrt(abs(x[idx_fin]))
        
      } else if (type == "Quad Root") {
        idx_fin <- is.finite(x)
        out[idx_fin] <- sign(x[idx_fin]) * (abs(x[idx_fin])^(1/4))
      }
      
      out
    }
    
    # Prepare data (exclude ignored rows)
    cd <- current_data()
    if (!is.null(ignored_rows) && length(ignored_rows) > 0) {
      cd <- cd[-ignored_rows, , drop = FALSE]
    }
    
    # Response variable (exclude by name, not index)
    rv_name <- colnames(cd)[response_var()]
    y <- cd[[rv_name]]
    
    # Numeric feature candidates (exclude ID and the current response by name)
    num_cols <- vapply(cd, is.numeric, logical(1))
    id_name  <- colnames(cd)[1]
    feature_names <- setdiff(names(cd)[num_cols], c(id_name, rv_name))
    
    # EXPLICIT: exclude transformed columns by TRANS_PREFIXES; KEEP interactions (Int..FeatA__FeatB)
    # Uses your global TRANS_PREFIXES (e.g., Log.., Inverse.., etc.)
    if (exists("TRANS_PREFIXES", inherits = FALSE)) {
      trans_pat <- sprintf("^(%s)", paste(escape_regex(TRANS_PREFIXES), collapse = "|"))
      feature_names <- feature_names[!grepl(trans_pat, feature_names, perl = TRUE)]
    }
    
    # At this point, interactions remain in feature_names by design
    
    transforms <- c("None", "Log10", "Inverse", "Square", "Square Root", "Quad Root", "Polynomial")
    
    results <- vector("list", length = length(feature_names) * length(transforms))
    k <- 1L
    
    for (fname in feature_names) {
      x <- cd[[fname]]
      
      for (tf in transforms) {
        corr <- NA_real_
        pval <- NA_real_
        
        if (tf == "Polynomial") {
          # Quadratic: correlate y with yhat from y ~ x + x^2
          mask <- is.finite(x) & is.finite(y)
          if (sum(mask) >= 3) {
            xt <- x[mask]; yt <- y[mask]
            if (stats::sd(xt) > 0 && stats::sd(yt) > 0) {
              fit <- try(stats::lm(yt ~ stats::poly(xt, degree = 2, raw = TRUE)), silent = TRUE)
              if (!inherits(fit, "try-error")) {
                yhat <- as.numeric(stats::fitted(fit))
                if (length(yhat) >= 3 && stats::sd(yhat) > 0) {
                  ct <- suppressWarnings(stats::cor.test(yt, yhat, method = "pearson"))
                  corr <- unname(ct$estimate)
                  pval <- unname(ct$p.value)
                }
              }
            }
          }
        } else {
          xt <- transform_feature(x, tf)  # must return numeric
          mask <- is.finite(xt) & is.finite(y)
          if (sum(mask) >= 3 && stats::sd(xt[mask]) > 0 && stats::sd(y[mask]) > 0) {
            ct <- suppressWarnings(stats::cor.test(xt[mask], y[mask], method = "pearson"))
            corr <- unname(ct$estimate)
            pval <- unname(ct$p.value)
          }
        }
        
        # Store numeric p-value as p_val; format later into p_Value
        results[[k]] <- data.frame(
          Feature     = fname,
          Transform   = tf,
          Correlation = corr,
          p_val       = pval,
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
    
    # Bind results
    trans_table <- if (length(results)) {
      do.call(rbind, results)
    } else {
      data.frame(
        Feature     = character(0),
        Transform   = character(0),
        Correlation = numeric(0),
        p_val       = numeric(0),
        stringsAsFactors = FALSE
      )
    }
    
    # Ensure numeric types and apply formatting
    trans_table$Correlation <- round(as.numeric(trans_table$Correlation), 4)
    trans_table$p_val       <- as.numeric(trans_table$p_val)  # keep raw numeric; do not round
    
    # Add a formatted p-value column for display (keep numeric p_val for sorting/calcs)
    trans_table$p_Value <- format_pval(trans_table$p_val)
    
    # Order transforms and group rows so Feature is shown once (for "None"), with a blank row before each feature
    trans_table$Transform <- factor(trans_table$Transform, levels = transforms)
    trans_table <- trans_table[order(trans_table$Feature, trans_table$Transform), ]
    
    feat_order <- unique(trans_table$Feature)
    final_list <- lapply(seq_along(feat_order), function(i) {
      f  <- feat_order[i]
      df <- trans_table[trans_table$Feature == f, , drop = FALSE]
      df$Feature     <- as.character(df$Feature)
      df$Feature[-1] <- ""          # show feature name only on the first row
      df$FeatureKey  <- f           # hidden key for all rows of this feature
      df$IsSep       <- FALSE
      if (i > 1) {
        blank <- data.frame(
          Feature     = "",
          Transform   = "",
          Correlation = NA_real_,
          p_val       = NA_real_,     # keep numeric for consistency
          p_Value     = NA_character_,
          FeatureKey  = f,
          IsSep       = TRUE,
          stringsAsFactors = FALSE
        )
        rbind(blank, df)
      } else {
        df
      }
    })
    
    final_table <- do.call(rbind, final_list)
    
    # For display: visible columns only; make blank separator rows truly blank
    display_table <- final_table[, c("Feature", "Transform", "Correlation", "p_Value")]
    display_table$Correlation[is.na(display_table$Correlation)] <- ""
    display_table$p_Value[is.na(display_table$p_Value)]         <- ""
    
    # Save full table (with hidden metadata) for apply button handler
    rv_trans$full <- final_table
    
    output$trans_table <- DT::renderDataTable(server = FALSE, {
      # Bind hidden metadata columns so JS can enforce one selection per feature
      dt_data <- cbind(
        display_table,
        FeatureKey = final_table$FeatureKey,
        IsSep      = final_table$IsSep
      )
      
      DT::datatable(
        dt_data,
        rownames   = FALSE,
        selection  = "none",        # DT’s own selection disabled; we manage via JS
        extensions = "Select",
        colnames   = c("Feature", "Transform", "Correlation", "P-Value", "FeatureKey", "IsSep"),
        width      = "100%",
        options = list(
          autoWidth  = FALSE,
          paging     = TRUE,
          dom        = 'tip',
          pageLength = 100,
          ordering   = FALSE,
          scrollX    = TRUE,
          select     = list(style = "api"),  # use API-only selection
          columnDefs = list(
            list(targets = 0:3, className = "dt-center"),
            list(targets = 4, visible = FALSE),  # FeatureKey
            list(targets = 5, visible = FALSE)   # IsSep
          ),
          rowCallback = DT::JS(
            "function(row, data){",
            "  if (String(data[5]) === 'true' || data[5] === true) {",
            "    $(row).addClass('sep-row');",
            "  } else {",
            "    $(row).removeClass('sep-row');",
            "  }",
            "}"
          ),
          initComplete = DT::JS(
            "function(settings){",
            "  var dt = this.api();",
            "  var $node = $(dt.table().node());",
            "  // Remove previous handlers to avoid duplicates on re-render",
            "  $node.off('.singleSel');",
            "  // Clear selection and preselect 'None' per feature (skip separators)",
            "  dt.rows().deselect();",
            "  dt.rows().every(function(){",
            "    var d = this.data();",
            "    if (!(String(d[5]) === 'true' || d[5] === true) && d[1] === 'None') { this.select(); }",
            "  });",
            "  // Block DT default user-select; we control selection ourselves",
            "  $node.on('user-select.dt.singleSel', function(e){ e.preventDefault(); });",
            "  // Click to select exactly one row per FeatureKey",
            "  $node.on('click.singleSel', 'tbody tr', function(){",
            "    var r = dt.row(this);",
            "    var d = r.data();",
            "    if (!d) return;",
            "    if (String(d[5]) === 'true' || d[5] === true) return; // separator row",
            "    var key = d[4];",
            "    // Deselect all rows for this feature, then select the clicked row",
            "    dt.rows(function(i, dd){ return dd && dd[4] === key; }).deselect();",
            "    r.select();",
            "    // Push selected indices (1-based) to Shiny",
            "    var sel = dt.rows({selected:true}).indexes().toArray().map(function(i){ return i + 1; });",
            "    Shiny.setInputValue('trans_table_rows_selected', sel, {priority:'event'});",
            "  });",
            "  // Push initial selection ('None' rows) to Shiny",
            "  var initial = dt.rows({selected:true}).indexes().toArray().map(function(i){ return i + 1; });",
            "  Shiny.setInputValue('trans_table_rows_selected', initial, {priority:'event'});",
            "}"
          ),
          columns = list(
            list(width = "235px"),
            list(width = "90px"),
            list(width = "90px"),
            list(width = "90px")
          )
        )
      )
    })
    
    # Navigate to Transformations tab
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Transformations")
  })
  
  #Make new columns of selected feature transformations
  observeEvent(input$apply_transforms, {
    req(rv_trans$full)
    full_tbl <- rv_trans$full
    
    sel <- input$trans_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("No rows selected. Please choose one transform per feature.", type = "warning")
      return()
    }
    
    chosen <- full_tbl[sel, , drop = FALSE]
    chosen$Transform <- as.character(chosen$Transform)
    chosen <- chosen[!chosen$IsSep & nzchar(chosen$Transform), , drop = FALSE]
    if (nrow(chosen) == 0) {
      showNotification("No valid transform rows selected.", type = "warning")
      return()
    }
    chosen <- chosen[!duplicated(chosen$FeatureKey), , drop = FALSE]
    
    # Transform rules
    transform_apply <- function(x, y, type) {
      n <- length(x)
      out <- rep(NA_real_, n)
      if (!is.numeric(x)) return(out)
      
      if (type %in% c("Log10")) {
        idx_pos  <- which(is.finite(x) & x > 0)
        idx_neg  <- which(is.finite(x) & x < 0)
        idx_zero <- which(is.finite(x) & x == 0)
        if (type == "Log10") {
          if (length(idx_pos))  out[idx_pos]  <- log10(x[idx_pos])
          if (length(idx_neg))  out[idx_neg]  <- -log10(abs(x[idx_neg]))
        } else {
          if (length(idx_pos))  out[idx_pos]  <- log(x[idx_pos])
          if (length(idx_neg))  out[idx_neg]  <- -log(abs(x[idx_neg]))
        }
        if (length(idx_zero)) out[idx_zero] <- 0
        
      } else if (type == "Inverse") {
        x2 <- x
        idx_fin <- is.finite(x2)
        idx_nz <- which(idx_fin & x2 != 0)
        if (length(idx_nz)) {
          min_nz <- min(abs(x2[idx_nz]), na.rm = TRUE)
          idx_zero <- which(idx_fin & x2 == 0)
          if (length(idx_zero)) x2[idx_zero] <- 0.5 * min_nz
          idx_inv <- which(is.finite(x2) & x2 != 0)
          if (length(idx_inv)) out[idx_inv] <- 1 / x2[idx_inv]
        }
        
      } else if (type == "Square") {
        out <- x^2
        
      } else if (type == "Square Root") {
        idx_fin <- is.finite(x)
        out[idx_fin] <- sign(x[idx_fin]) * sqrt(abs(x[idx_fin]))
        
      } else if (type == "Quad Root") {
        idx_fin <- is.finite(x)
        out[idx_fin] <- sign(x[idx_fin]) * (abs(x[idx_fin])^(1/4))
        
      } else if (type == "Polynomial") {
        out[] <- NA_real_
        if (!is.null(y)) {
          mask <- is.finite(x) & is.finite(y)
          if (sum(mask) >= 3 && sd(x[mask]) > 0 && sd(y[mask]) > 0) {
            fit <- try(lm(y[mask] ~ poly(x[mask], degree = 2, raw = TRUE)), silent = TRUE)
            if (!inherits(fit, "try-error")) {
              idx_fin <- is.finite(x)
              if (any(idx_fin)) {
                co <- coef(fit)
                if (length(co) >= 3) {
                  out[idx_fin] <- co[1] + co[2] * x[idx_fin] + co[3] * x[idx_fin]^2
                }
              }
            }
          }
        }
      }
      out
    }
    
    df <- current_data()
    rv_name <- colnames(df)[response_var()]
    y_full  <- df[[rv_name]]
    
    # Add/overwrite each new column and register it in column_props; place it after the source feature
    for (i in seq_len(nrow(chosen))) {
      feat <- chosen$FeatureKey[i]
      tf   <- chosen$Transform[i]
      if (tf == "None") next
      
      # Validate transform name against global prefix_map
      if (!tf %in% names(prefix_map)) {
        showNotification(sprintf("Unknown transform '%s' for feature '%s'. Skipping.", tf, feat), type = "warning")
        next
      }
      
      vals <- transform_apply(df[[feat]], y_full, tf)
      
      # Build new column name using global prefix_map
      new_name <- paste0(prefix_map[[tf]], feat)
      
      # Replace any existing transformed column for this feature, keep position
      existing_old <- intersect(paste0(TRANS_PREFIXES, feat), names(df))
      if (length(existing_old)) {
        pos_target <- which(names(df) %in% existing_old)[1]
        df <- df[, setdiff(names(df), existing_old), drop = FALSE]
        if (exists(".del", mode = "function")) {
          for (nm_old in existing_old) try(.del(column_props, keys = nm_old), silent = TRUE)
        }
      } else {
        pos_feat  <- match(feat, names(df))
        pos_target <- if (!is.na(pos_feat)) pos_feat + 1L else length(names(df)) + 1L
      }
      
      # Add/overwrite new column and register props
      df[[new_name]] <- vals
      .set(column_props, keys = new_name, values = 2)
      
      # Reorder to place new column immediately after target position
      nm <- names(df)
      idx_new   <- match(new_name, nm)
      nm_wo_new <- nm[-idx_new]
      pos_target <- max(1L, min(pos_target, length(nm_wo_new) + 1L))
      df <- df[, append(nm_wo_new, new_name, after = pos_target - 1L), drop = FALSE]
    }
    
    current_data(df)
    
    # Refresh selectable column lists (optional but recommended)
    new_column_names <- colnames(current_data())[-1]  # exclude ID
    rv <- response_var() - 1
    feat_names(new_column_names[-rv])
    feats_being_used(feat_names())
    
    updateSelectInput(session, "set_column_props", choices = c("-", new_column_names))
    updateSelectInput(session, "rainplot",         choices = c("-", new_column_names))
    updateSelectInput(session, "lineplot",         choices = c("-", new_column_names))
    updateSelectInput(session, "scatterx", selected = input$scatterx, choices = c("-", new_column_names))
    updateSelectInput(session, "scattery", selected = input$scattery, choices = c("-", new_column_names))
    
    renderdata(current_data(), response_var(), id_var, input$select_choice,
               date_format_string, column_props, ignored_rows, current_data_page(), output)
    
    showNotification("Selected transforms added to the dataset.", type = "message")
  })
  
  # Create an interactions data table
  observeEvent(input$interacts, {
    
    # Empty skeleton with new column names
    output$interactions_table <- DT::renderDataTable(server = FALSE, {
      DT::datatable(
        data.frame(Feat1 = character(0), Feat2 = character(0),
                   Correlation = numeric(0), p_Value = character(0)),
        rownames = FALSE, selection = "none",
        options = list(dom = 't', paging = FALSE)
      )
    })
    
    df <- current_data()
    if (is.null(df) || ncol(df) < 3L) return()
    
    # Exclude rows the user ignored
    rows_keep <- seq_len(nrow(df))
    if (!is.null(ignored_rows) && length(ignored_rows) > 0) {
      rows_keep <- setdiff(rows_keep, ignored_rows)
    }
    df <- df[rows_keep, , drop = FALSE]
    
    # Identify response and candidate numeric features by name (exclude ID + response + derived)
    rv_name <- colnames(df)[response_var()]
    y <- df[[rv_name]]
    id_name <- colnames(df)[1L]
    
    num_names <- names(df)[vapply(df, is.numeric, logical(1))]
    base_feats <- get_interaction_candidates(df, include_AO = TRUE)
    
    if (length(base_feats) < 2L) {
      showNotification("Not enough numeric features to compute interactions.", type = "warning")
      return()
    }
    
    # Threshold
    thr <- input$r_thresh %||% 0.5
    
    # Compute correlations for all pairs i < j
    res <- list()
    k <- 1L
    for (ii in seq_len(length(base_feats) - 1L)) {
      for (jj in (ii + 1L):length(base_feats)) {
        a <- base_feats[ii]; b <- base_feats[jj]
        prod <- df[[a]] * df[[b]]
        mask <- is.finite(prod) & is.finite(y)
        if (sum(mask) >= 3 && sd(prod[mask]) > 0 && sd(y[mask]) > 0) {
          ct <- suppressWarnings(cor.test(prod[mask], y[mask], method = "pearson"))
          r  <- unname(ct$estimate)
          p  <- unname(ct$p.value)
          if (abs(r) >= thr) {
            res[[k]] <- data.frame(
              Feat1      = a,
              Feat2      = b,
              Correlation = round(r, 4),
              p_Value     = ifelse(
                is.na(p),
                NA_character_,
                ifelse(p < 1e-4, "<0.0001", format(signif(p, 4), scientific = FALSE, trim = TRUE))
              ),
              stringsAsFactors = FALSE
            )
            k <- k + 1L
          }
        }
      }
    }
    
    inter_tbl <- if (length(res)) do.call(rbind, res) else NULL
    rv_inter$table <- inter_tbl
    
    output$interactions_table <- DT::renderDataTable(server = FALSE, {
      if (is.null(inter_tbl) || nrow(inter_tbl) == 0) {
        DT::datatable(
          data.frame(Message = "No interactions exceed the threshold."),
          rownames = FALSE, selection = "none",
          options = list(dom = 't', paging = FALSE)
        )
      } else {
        DT::datatable(
          inter_tbl,
          rownames  = FALSE,
          selection = "multiple",
          options = list(
            autoWidth  = FALSE,
            dom        = 'tip',
            paging     = TRUE,
            pageLength = 50,
            ordering   = FALSE,
            scrollX    = TRUE,
            columnDefs = list(list(targets = 0:3, className = "dt-center"))
          )
        )
      }
    })
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Interactions")
  })
  
  # Add selected interactions to the data table
  observeEvent(input$add_interactions, {
    inter_tbl <- rv_inter$table
    sel <- input$interactions_table_rows_selected
    if (is.null(inter_tbl) || length(sel) == 0) {
      showNotification("No interactions selected.", type = "warning")
      return()
    }
    
    df <- current_data()
    if (is.null(df) || !is.data.frame(df)) {
      showNotification("No active dataset.", type = "error")
      return()
    }
    cols <- names(df)
    
    for (i in sel) {
      a <- inter_tbl$Feat1[i]
      b <- inter_tbl$Feat2[i]
      
      # Validate presence and numeric types
      if (!a %in% cols || !b %in% cols) next
      if (!is.numeric(df[[a]]) || !is.numeric(df[[b]])) next
      
      # Create a canonical interaction name (e.g., "A*B"); adjust make_inter_name if needed
      name <- make_inter_name(a, b)
      
      # Compute product on full current_data
      v <- df[[a]] * df[[b]]
      
      # Replace if exists; keep position if replacing
      if (name %in% cols) {
        # Overwrite values in place
        df[[name]] <- v
      } else {
        # Place after the later of a/b
        pos_target <- max(match(a, cols), match(b, cols), na.rm = TRUE) + 1L
        
        # Add column and register props
        df[[name]] <- v
        if (exists(".set", mode = "function")) {
          try(.set(column_props, keys = name, values = 2), silent = TRUE)
        }
        
        # Reorder to target position
        nm <- names(df)
        idx_new <- match(name, nm)
        nm_wo_new <- nm[-idx_new]
        pos_target <- max(1L, min(pos_target, length(nm_wo_new) + 1L))
        df <- df[, append(nm_wo_new, name, after = pos_target - 1L), drop = FALSE]
        cols <- names(df)
      }
    }
    
    current_data(df)
    
    # Refresh selectors (exclude ID and response)
    new_column_names <- colnames(df)[-1]
    rv_idx <- response_var()
    if (!is.null(rv_idx) && rv_idx > 1 && rv_idx <= ncol(df)) {
      feat_names(new_column_names[-(rv_idx - 1L)])
    } else {
      feat_names(new_column_names)
    }
    feats_being_used(feat_names())
    
    choices_vec <- c("-", new_column_names)
    updateSelectInput(session, "set_column_props", choices = choices_vec)
    updateSelectInput(session, "rainplot",         choices = choices_vec)
    updateSelectInput(session, "lineplot",         choices = choices_vec)
    updateSelectInput(session, "scatterx", selected = input$scatterx, choices = choices_vec)
    updateSelectInput(session, "scattery", selected = input$scattery, choices = choices_vec)
    
    renderdata(current_data(), response_var(), id_var, input$select_choice,
               date_format_string, column_props, ignored_rows, current_data_page(), output)
    
    showNotification("Selected interactions added to the dataset.", type = "message")
  })
  
  # Create PCA dataset for later analysis
  observeEvent(input$pca_check, ignoreInit = T, {
    
    clear_trans_table(drop_transforms = TRUE, drop_interactions = FALSE, drop_AO = TRUE)
    data = current_data()
    feats = feats_being_used()
    feat_data = data[,feats,drop = FALSE]
    
    pca_axes_max(ncol(feat_data))
    
    updateNumericInput(session, "num_axes",max = pca_axes_max())
    
    if (is.null(ignored_rows)) {
      feat_data = feat_data
    } else {
      feat_data = feat_data[-ignored_rows,]
    }
    
    if (any(is.na(feat_data))) {
      feat_data = data.frame(missForest(feat_data)$ximp)
    }
    
    n_axes = input$num_axes
    
    # Run PCA on feature data
    pca_result = prcomp(feat_data, scale. = TRUE)
    
    PCA_scaling_mean(setNames(pca_result$center, colnames(feat_data)))
    PCA_scaling_sd(setNames(pca_result$scale, colnames(feat_data)))
    
    pca_summary = summary(pca_result)
    
    if (date_format_string == "Character") {
      PCA_data = data.frame(cbind(data[,1],data[,response_var()]))
      PCA_data[,3:(n_axes+2)] = pca_result$x[,1:n_axes]
    } else {
      PCA_data = data.frame(cbind(data[,1],data[,response_var()],pca_result$x[,1:n_axes]))
    }
    
    colnames(PCA_data) = c(colnames(data)[1],colnames(data)[response_var()],paste0("PC",seq(1,n_axes)))
    
    PCA_dataset(PCA_data)
    
    pca_axes(colnames(PCA_dataset())[3:ncol(PCA_dataset())])
    pcax_being_used(pca_axes())
    
    PCA_coefficients0 = data.frame(round(pca_result$rotation[,1:n_axes],4))
    PCA_coefficients(cbind(Feature = rownames(PCA_coefficients0), PCA_coefficients0))
    
    PCA_summary_df0 = data.frame(rbind(round(pca_summary$importance[1,1:n_axes],3),pca_summary$importance[2,1:n_axes],pca_summary$importance[3,1:n_axes]))
    summary_rownames= c("Std. Dev.","Variance Explained","Cumulative Var Explained")
    PCA_summary_df1 = cbind(summary_rownames,PCA_summary_df0)
    colnames(PCA_summary_df1)[1] = "Metric"
    PCA_summary_df(PCA_summary_df1)
    
    clear_modeling(TRUE)
    changed_model(TRUE)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "PCA Results")
    
  })
  
  observeEvent(c(PCA_dataset(),refresh_trigger()), ignoreInit = TRUE, {
    if (!is.null(PCA_dataset())) {
      
      output$PCA_coeffs = DT::renderDataTable(server = T, {data = datatable(PCA_coefficients(),rownames = F,selection =
                    list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                    list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                    className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$PCA_summary = DT::renderDataTable(server = T, {data = datatable(PCA_summary_df(),rownames = F,selection =
                    list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                    list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                    className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      renderPCAdata(PCA_dataset(),date_format_string,output)
      
      refresh_trigger(FALSE)
      
    } else if (is.null(PCA_dataset())) {
      
      output$PCA_coeffs = NULL
      output$PCA_summary = NULL
      renderPCAdata(PCA_dataset(),date_format_string,output)
      refresh_trigger(FALSE)
    }
  })
  
  observeEvent(pca_axes_max(), ignoreInit = T, {
    
    iv$remove_rules("num_axes")
    iv$add_rule("num_axes", sv_between(2,pca_axes_max()))
    
  })
  
  # Toggling dataset manipulation options
  observeEvent(input$select_choice, ignoreInit = T, {
    
    if (input$select_choice == "D/E_Rows") {
      enable("ignore_rows")
      enable("enable_rows")
    } else {
      disable("ignore_rows")
      disable("enable_rows")
    }
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
  })
  
  # Provide dataset cell editing
  observeEvent(input$data_cell_edit, ignoreInit = T, {
    
    clear_trans_table(drop_transforms = TRUE, drop_interactions = FALSE, drop_AO = FALSE)
    
    info = input$data_cell_edit
    temp_data=current_data()
    
    i = info$row
    j = info$col + 1
    
    temp_data = editData(temp_data, input$data_cell_edit, "data", rownames = FALSE)
    
    current_data(temp_data)
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
  })
  
  # Change the response variable
  observeEvent(input$data_columns_selected, ignoreInit = TRUE, {
    if (!identical(input$select_choice, "Change_Response")) return()
    
    df <- current_data()
    if (is.null(df) || !is.data.frame(df) || ncol(df) < 2L) return()
    
    all_cols <- colnames(df)
    id_name  <- all_cols[1L]
    
    # Selected feature index is 1-based over features (excluding ID)
    sel_idx_ui <- as.integer(input$data_columns_selected)
    if (!is.finite(sel_idx_ui)) return()
    
    # Map to absolute column index in df: add 1 for the ID offset
    new_rv_idx <- sel_idx_ui + 1L
    
    cur_rv_idx <- response_var()
    if (new_rv_idx == cur_rv_idx || new_rv_idx < 2L || new_rv_idx > ncol(df)) return()
    
    # Preserve previous response for this clear operation (Option A)
    options(ShinyVB.prev_resp_name = all_cols[cur_rv_idx])
    
    # Set the new response and clear transforms + interactions + A/O immediately
    response_var(new_rv_idx)
    clear_trans_table(drop_transforms = TRUE, drop_interactions = TRUE, drop_AO = FALSE)
    
    # Fallback: explicitly drop star-named interactions if they weren't prefixed
    df <- current_data()
    inter_cols <- find_interaction_cols(df)
    if (length(inter_cols)) {
      # Protect current and previous response names
      rv_name   <- colnames(df)[response_var()]
      prev_resp <- getOption("ShinyVB.prev_resp_name", NULL)
      options(ShinyVB.prev_resp_name = NULL)  # clear once used
      protect <- unique(stats::na.omit(as.character(c(rv_name, prev_resp))))
      
      inter_cols <- setdiff(inter_cols, protect)
      if (length(inter_cols)) {
        df[inter_cols] <- NULL
        if (exists(".del", mode = "function")) {
          for (nm in inter_cols) try(.del(column_props, keys = nm), silent = TRUE)
        }
        current_data(df)
      }
    }
    
    # Rebuild feature list by name (exclude ID and the NEW response)
    all_cols <- colnames(current_data())
    rv_name  <- all_cols[response_var()]
    feature_names_now <- setdiff(all_cols, c(id_name, rv_name))
    
    feats_being_used(feature_names_now)
    feat_names(feature_names_now)
    updateCheckboxGroupButtons(
      session, "feats_to_use",
      choices  = feat_names(),
      selected = feats_being_used(),
      size = "xs", status = "custom"
    )
    
    # Response stats excluding left/right-censored tags
    exclude_values <- c(input$lc_val, input$rc_val)
    resp_vec <- current_data()[[rv_name]]
    real_responses <- stats::na.omit(resp_vec[!(resp_vec %in% exclude_values)])
    
    safe_min    <- function(x) if (length(x)) min(x) else NA_real_
    safe_max    <- function(x) if (length(x)) max(x) else NA_real_
    safe_median <- function(x) if (length(x)) stats::median(x) else NA_real_
    
    rmin <- round(safe_min(real_responses), 3)
    rmax <- round(safe_max(real_responses), 3)
    rmed <- round(safe_median(real_responses), 2)
    
    updateNumericInput(session, "LG_binarize_crit_value",   value = rmed, min = rmin, max = rmax)
    updateNumericInput(session, "XGBCL_binarize_crit_value", value = rmed, min = rmin, max = rmax)
    
    iv$remove_rules("LG_binarize_crit_value")
    iv$remove_rules("XGBCL_binarize_crit_value")
    if (is.finite(rmin) && is.finite(rmax) && rmin <= rmax) {
      iv$add_rule("LG_binarize_crit_value",    sv_between(rmin, rmax))
      iv$add_rule("XGBCL_binarize_crit_value", sv_between(rmin, rmax))
    }
    
    updateNumericInput(session, "lc_replace", value = rmin)
    updateNumericInput(session, "rc_replace", value = rmax)
    
    renderdata(current_data(), response_var(), id_var, input$select_choice,
               date_format_string, column_props, ignored_rows, current_data_page(), output)
    
    # Reset PCA-related state
    pca_axes_max(max(0L, ncol(init_data) - 2L))
    pca_axes(NULL)
    pcax_being_used(NULL)
    PCA_dataset(NULL)
    
    clear_modeling(TRUE)
    changed_model(TRUE)
    
    updateNumericInput(session, "num_axes", value = pca_axes_max(), max = pca_axes_max())
  })
  
  # Enabling/disabling rows
  observeEvent(input$ignore_rows, ignoreInit = TRUE, {
    # Drop transformed + interaction + A/O columns
    clear_trans_table(drop_transforms = TRUE, drop_interactions = TRUE, drop_AO = FALSE)
    
    # Determine which rows to add to ignored set based on active tab
    add_in <- integer(0)
    if (identical(input$data_tabs, "Data Table")) {
      sel <- input$data_rows_selected %||% integer(0)
      add_in <- if (length(sel) > 1L) sel[-1L] else integer(0)  # guard for empty/1-length
    } else if (identical(input$data_tabs, "IsoForest Leverage")) {
      add_in <- input$iso_leverage_rows_selected %||% integer(0)
    }
    
    # Update ignored_rows
    new_ignored <- unique(c(ignored_rows %||% integer(0), add_in))
    ignored_rows <<- if (length(new_ignored)) new_ignored else NULL
    
    # Re-render
    renderdata(current_data(), response_var(), id_var, input$select_choice,
               date_format_string, column_props, ignored_rows, current_data_page(), output)
  })
  
  observeEvent(input$enable_rows, ignoreInit = TRUE, {
    # Drop transformed + interaction + A/O columns
    clear_trans_table(drop_transforms = TRUE, drop_interactions = TRUE, drop_AO = FALSE)
    
    # Determine which rows to remove from ignored set based on active tab
    add_back <- integer(0)
    if (identical(input$data_tabs, "Data Table")) {
      sel <- input$data_rows_selected %||% integer(0)
      add_back <- if (length(sel) > 1L) sel[-1L] else integer(0)
    } else if (identical(input$data_tabs, "IsoForest Leverage")) {
      add_back <- input$iso_leverage_rows_selected %||% integer(0)
    }
    
    # Update ignored_rows
    cur_ignored <- ignored_rows %||% integer(0)
    new_ignored <- cur_ignored[!(cur_ignored %in% add_back)]
    ignored_rows <<- if (length(new_ignored)) new_ignored else NULL
    
    # Re-render
    renderdata(current_data(), response_var(), id_var, input$select_choice,
               date_format_string, column_props, ignored_rows, current_data_page(), output)
  })
  
  # Restore the original dataset
  observeEvent(input$restore, ignoreInit = T, {
    
    response_var(2)
    
    clear_trans_table(drop_transforms = TRUE, drop_interactions = TRUE, drop_AO = TRUE)
    
    if (init_ID_format == "YMD") {
      init_data[,1] = ymd(init_data[,1])
      date_format_string <<- "toLocaleDateString"
    } else if (init_ID_format == "MDY") {
      init_data[,1] = mdy(init_data[,1])
      date_format_string <<- "toLocaleDateString"
    } else if (init_ID_format == "MDYHM") {
      init_data[,1] = parse_date_time(init_data[,1],c('%m/%d/%y %H:%M'),exact=TRUE)
      date_format_string <<- "toLocaleString"
    } else if (init_ID_format == "Character") {
      date_format_string <<- "Character"
    } else if (init_ID_format == "Numeric") {
      date_format_string <<- "Numeric"
    }
    
    temp_data = init_data[,3:ncol(init_data)]
    
    feat_names(colnames(temp_data))
    feats_being_used(colnames(temp_data))
    pca_axes(NULL)
    pcax_being_used(NULL)
    PCA_dataset(NULL)
    pca_axes_max(ncol(init_data)-2)
    col_names(colnames(init_data[,-1]))
    current_data(init_data)
    ignored_rows <<- NULL
    column_props <<- init_column_props
    
    updateNumericInput(session, "num_axes",value = pca_axes_max(),max = pca_axes_max())
    updateSelectInput(session,"speed",selected="-")
    updateSelectInput(session,"direct",selected="-")
    updateSelectInput(session,"select_choice",selected="Change_Response")
    updateSelectInput(session,"set_column_props",selected='-', choices=c("-",col_names()))
    updateSelectInput(session,"rainplot",selected='-', choices=c("-",col_names()))
    updateSelectInput(session,"lineplot",selected='-', choices=c("-",col_names()))
    updateSelectInput(session,"scatterx",selected="-",choices=c("-",col_names()))
    updateSelectInput(session,"scattery",selected="-",choices=c("-",col_names()))
    
    current_data_page(1)
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
    renderPCAdata(PCA_dataset(),date_format_string,output)
    
    clear_modeling(TRUE)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = "Data Table")
    
  })
  
  # Clear all modeling results
  observeEvent(clear_modeling(), ignoreInit = T, {
    
    if (clear_modeling()) {
      
      # Logistic Regression Prediction Results
      LG_pred_results(NULL)
      LG_pred_coeffs(NULL)
      LG_pred_confuse_results(NULL)
      LG_pred_scat_dat(NULL)
      LG_pred_thresh(NULL)
      
      # Logistic Regression Results
      LG_results(NULL)
      LG_coeffs(NULL)
      LG_confuse_results(NULL)
      LG_scat_dat(NULL)
      LG_model <<- NULL
      LG_thresh(NULL)
      LG_crit_prob(NULL)
      LG_final_features(NULL)
      LG_pred_data(NULL)
      
      # XGBoost Classifier Prediction Results
      XGBCL_pred_results(NULL)
      XGBCL_pred_coeffs(NULL)
      XGBCL_pred_confuse_results(NULL)
      XGBCL_pred_scat_dat(NULL)
      XGBCL_pred_thresh(NULL)
      
      # XGBoost Classifier Other Results
      XGBCL_selection_results(NULL)
      
      # XGBoost Classifier Results
      XGBCL_results(NULL)
      XGBCL_coeffs(NULL)
      XGBCL_confuse_results(NULL)
      XGBCL_scat_dat(NULL)
      XGBCL_model <<- NULL
      XGBCL_thresh(NULL)
      XGBCL_crit_prob(NULL)
      XGBCL_final_data(NULL)
      XGBCL_final_features(NULL)
      XGBCL_pred_data(NULL)
      
      # XGBoost Prediction Results
      XGB_pred_results(NULL)
      XGB_pred_coeffs(NULL)
      XGB_pred_confuse_results(NULL)
      XGB_pred_scat_dat(NULL)
      
      # XGBoost Other Results
      XGB_selection_results(NULL)
      
      # XGBoost Results
      XGB_results(NULL)
      XGB_coeffs(NULL)
      XGB_confuse_results(NULL)
      XGB_scat_dat(NULL)
      XGB_model <<- NULL
      XGB_final_data(NULL)
      XGB_final_features(NULL)
      XGB_pred_data(NULL)
      
      # Elastic Net Prediction Results
      EN_pred_results(NULL)
      EN_pred_coeffs(NULL)
      EN_pred_confuse_results(NULL)
      EN_pred_scat_dat(NULL)
      
      # Elastic Net Results
      EN_results(NULL)
      EN_coeffs(NULL)
      EN_confuse_results(NULL)
      EN_scat_dat(NULL)
      EN_model <<- NULL
      EN_final_features(NULL)
      EN_pred_data(NULL)
      
      # General Modeling Results
      model_to_use(NULL)
      pred_data(NULL)
      pred_residuals(NULL)
      models_created(NULL)
      
      clear_modeling(FALSE)
      changed_model(TRUE)
      refresh_trigger(TRUE)
    }
  })
  
  # Input column properties into the hash table
  observeEvent(input$set_column_props, ignoreInit = T,  {
    
    if (input$set_column_props != "-") {
      
      showModal(modalDialog(title=paste0(input$set_column_props," Column Properties"),card(
        fluidRow(
          column(4,numericInput("sig_digies",  label="Significant Digits", value = values(column_props,keys=input$set_column_props)[1], min=0,max=12,step=1)))),
        # column(3,numericInput("prop2",  label="Prop2", value = values(column_props,keys=input$col_props)[2], min=0,max=1,step=0.05)),
        # column(3,textInput("prop3",  label="Prop3", value = values(column_props,keys=input$col_props)[3])),
        # column(3,textInput("prop4",  label="Prop4", value = values(column_props,keys=input$col_props)[4])))),
        footer = div(actionButton("props_close",'Close'))))
    }
  })
  
  observeEvent(input$props_close, ignoreInit = T, {
    removeModal()
    updateSelectInput(session,"set_column_props",selected="-")
  })
  
  observeEvent(input$sig_digies, ignoreInit = T, {
    
    .set(column_props,keys=input$set_column_props,values=c(input$sig_digies))
    
    # .set(column_props,keys=input$set_column_props,values=c(input$sig_digies,values(column_props,keys=input$set_column_props)[2],
    #         values(column_props,keys=input$set_column_props)[3],values(column_props,keys=input$set_column_props)[4]))
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
    
  })
  
  # Isolation Forest analysis for high leverage detection
  observeEvent(input$run_iso_forest, ignoreInit = T, {
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
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
                                   standardize_data = std_data,scoring_metric = techs[[i]],output_score = TRUE)
      
      iso_results[,i+1] = round(isoforest$scores,3)
    }
    
    iso_results[,6] = round((iso_results[,2] * iso_results[,3] * iso_results[,4] * iso_results[,5])^0.25 - 0.7071,3)
    
    output$iso_leverage = DT::renderDataTable(server = T, {data = datatable(iso_results,rownames = F,selection = list(selection = "multiple",
                      selected = list(rows = NULL),target = "row"),editable = F,extensions="Buttons",options = list(paging = TRUE,dom="ltBp",
                      buttons = c('copy', 'csv', 'excel'),pageLength = num_rows_per_page,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                      initComplete =JS("function(settings, json) {","$(this.api().table().header()).css({'background-color':'#073744', 'color': '#fff'});","}")))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = 'IsoForest Leverage')
    
  })
  
  # Keep track of the current page in the data table
  observeEvent(input$data_state, ignoreInit=T, {
    
    x = input$data_state$start
    y = input$data_state$length
    current_page = 1 + x/y
    
    if (current_page != current_data_page()) {
      current_data_page(current_page)
      renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
    }
  })
  
  # Plotting functions
  observeEvent(input$cens_choice, ignoreInit = T, {
    
    if (input$rainplot == colnames(current_data())[response_var()] && last_plot() == "raincloud") {
      redraw_rainplot(TRUE)
    } else if ((input$scatterx == colnames(current_data())[response_var()] || input$scattery == colnames(current_data())[response_var()]) && last_plot() == "scatter") {
      redraw_scatplot(TRUE)
    } else if (input$lineplot == colnames(current_data())[response_var()] && last_plot() == "lineplot") {
      redraw_lineplot(TRUE)
    } else {
      return()
    }
  })
  
  observeEvent(input$lc_replace, ignoreInit = T, {
    
    if (input$rainplot == colnames(current_data())[response_var()] && last_plot() == "raincloud") {
      redraw_rainplot(TRUE)
    } else if ((input$scatterx == colnames(current_data())[response_var()] || input$scattery == colnames(current_data())[response_var()]) && last_plot() == "scatter") {
      redraw_scatplot(TRUE)
    } else if (input$lineplot == colnames(current_data())[response_var()] && last_plot() == "lineplot") {
      redraw_lineplot(TRUE)
    } else {
      return()
    }
  })
  
  observeEvent(input$rc_replace, ignoreInit = T, {
    
    if (input$rainplot == colnames(current_data())[response_var()] && last_plot() == "raincloud") {
      redraw_rainplot(TRUE)
    } else if ((input$scatterx == colnames(current_data())[response_var()] || input$scattery == colnames(current_data())[response_var()]) && last_plot() == "scatter") {
      redraw_scatplot(TRUE)
    } else if (input$lineplot == colnames(current_data())[response_var()] && last_plot() == "lineplot") {
      redraw_lineplot(TRUE)
    } else {
      return()
    }
  })
  
  observeEvent(input$rainplot, ignoreInit = T, {
    
    if (input$rainplot != "-") {
      
      if (is.null(ignored_rows)) {
        rain_data0 = current_data()
      } else {
        rain_data0 = current_data()[-ignored_rows,]
      }
      
      if (input$cens_choice == 'hide') {
        
        rain_data0 = rain_data0[!rain_data0[,response_var()] %in% c(input$lc_val, input$rc_val),]
        
      } else if (input$cens_choice == 'replace') {
        
        rain_data0[rain_data0[,response_var()] == input$lc_val,response_var()] = input$lc_replace
        rain_data0[rain_data0[,response_var()] == input$rc_val,response_var()] = input$rc_replace
      }
      
      rain_data1 = cbind(rain_data0[,id_var],rain_data0[,input$rainplot])
      colnames(rain_data1) = c("ID",input$rainplot)
      
      output$rainplot = renderPlot({raincloud(rain_data1,date_format_string)})
      
      last_plot("raincloud")
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = "Raincloud")
    }
  })
  
  observeEvent(redraw_rainplot(), ignoreInit = T, {
    
    if (input$rainplot != "-") {
      
      if (is.null(ignored_rows)) {
        rain_data0 = current_data()
      } else {
        rain_data0 = current_data()[-ignored_rows,]
      }
      
      if (input$cens_choice == 'hide') {
        
        rain_data0 = rain_data0[!rain_data0[,response_var()] %in% c(input$lc_val, input$rc_val),]
        
      } else if (input$cens_choice == 'replace') {
        
        rain_data0[rain_data0[,response_var()] == input$lc_val,response_var()] = input$lc_replace
        rain_data0[rain_data0[,response_var()] == input$rc_val,response_var()] = input$rc_replace
      }
      
      rain_data1 = cbind(rain_data0[,id_var],rain_data0[,input$rainplot])
      colnames(rain_data1) = c("ID",input$rainplot)
      
      output$rainplot = renderPlot({raincloud(rain_data1,date_format_string)})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = "Raincloud")
      
      redraw_rainplot(FALSE)
      last_plot("raincloud")
    }
  })
  
  observeEvent(input$lineplot, ignoreInit = T, {
    
    if (input$lineplot != "-") {
      
      if (is.null(ignored_rows)) {
        line_data0 = current_data()
      } else {
        line_data0 = current_data()[-ignored_rows,]
      }
      
      if (input$cens_choice == 'hide') {
        
        line_data0 = line_data0[!line_data0[,response_var()] %in% c(input$lc_val, input$rc_val),]
        
      } else if (input$cens_choice == 'replace') {
        
        line_data0[line_data0[,response_var()] == input$lc_val,response_var()] = input$lc_replace
        line_data0[line_data0[,response_var()] == input$rc_val,response_var()] = input$rc_replace
      }
      
      var_list = c(1,which(colnames(line_data0) == input$lineplot))
      line_data1 = line_data0[,var_list]
      
      output$lineplott = renderPlotly({lineplot(line_data1,input$lineplot,date_format_string)})
      
      last_plot("lineplot")
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Line Plot')
    }
  })
  
  observeEvent(redraw_lineplot(), ignoreInit = T, {
    
    if (input$lineplot != "-") {
      
      if (is.null(ignored_rows)) {
        line_data0 = current_data()
      } else {
        line_data0 = current_data()[-ignored_rows,]
      }
      
      if (input$cens_choice == 'hide') {
        
        line_data0 = line_data0[!line_data0[,response_var()] %in% c(input$lc_val, input$rc_val),]
        
      } else if (input$cens_choice == 'replace') {
        
        line_data0[line_data0[,response_var()] == input$lc_val,response_var()] = input$lc_replace
        line_data0[line_data0[,response_var()] == input$rc_val,response_var()] = input$rc_replace
      }
      
      var_list = c(1,which(colnames(line_data0) == input$lineplot))
      line_data1 = line_data0[,var_list]
      
      output$lineplott = renderPlotly({lineplot(line_data1,input$lineplot,date_format_string)})
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Line Plot')
      
      redraw_lineplot(FALSE)
      last_plot("lineplot")
    }
  })
  
  observeEvent(input$scatterx, ignoreInit = T, {
    
    if (input$scatterx != "-" & input$scattery!= "-") {
      
      if (is.null(ignored_rows)) {
        scatter_data0 = current_data()
      } else {
        scatter_data0 = current_data()[-ignored_rows,]
      }
      
      if (input$cens_choice == 'hide') {
        
        scatter_data0 = scatter_data0[!scatter_data0[,response_var()] %in% c(input$lc_val, input$rc_val),]
        
      } else if (input$cens_choice == 'replace') {
        
        scatter_data0[scatter_data0[,response_var()] == input$lc_val,response_var()] = input$lc_replace
        scatter_data0[scatter_data0[,response_var()] == input$rc_val,response_var()] = input$rc_replace
      }
      
      var_list = c(1,which(colnames(scatter_data0) == input$scatterx),which(colnames(scatter_data0) == input$scattery))
      scatter_data1 = scatter_data0[,var_list]
      
      output$scatplot = renderPlotly(scatter(scatter_data1))
      
      last_plot("scatter")
      
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
      
      if (input$cens_choice == 'hide') {
        
        scatter_data0 = scatter_data0[!scatter_data0[,response_var()] %in% c(input$lc_val, input$rc_val),]
        
      } else if (input$cens_choice == 'replace') {
        
        scatter_data0[scatter_data0[,response_var()] == input$lc_val,response_var()] = input$lc_replace
        scatter_data0[scatter_data0[,response_var()] == input$rc_val,response_var()] = input$rc_replace
      }
      
      var_list = c(1,which(colnames(scatter_data0) == input$scatterx),which(colnames(scatter_data0) == input$scattery))
      scatter_data1 = scatter_data0[,var_list]
      
      output$scatplot = renderPlotly(scatter(scatter_data1))
      
      last_plot("scatter")
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Scatterplot')
    }
  })
  
  observeEvent(redraw_scatplot(), ignoreInit = T, {
    
    if (input$scatterx != "-" & input$scattery!= "-") {
      
      if (is.null(ignored_rows)) {
        scatter_data0 = current_data()
      } else {
        scatter_data0 = current_data()[-ignored_rows,]
      }
      
      if (input$cens_choice == 'hide') {
        
        scatter_data0 = scatter_data0[!scatter_data0[,response_var()] %in% c(input$lc_val, input$rc_val),]
        
      } else if (input$cens_choice == 'replace') {
        
        scatter_data0[scatter_data0[,response_var()] == input$lc_val,response_var()] = input$lc_replace
        scatter_data0[scatter_data0[,response_var()] == input$rc_val,response_var()] = input$rc_replace
      }
      
      var_list = c(1,which(colnames(scatter_data0) == input$scatterx),which(colnames(scatter_data0) == input$scattery))
      scatter_data1 = scatter_data0[,var_list]
      
      output$scatplot = renderPlotly(scatter(scatter_data1))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
      updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Scatterplot')
      
      last_plot("scatter")
      redraw_scatplot(FALSE)
    }
  })
  
  # Update A/O component column names
  observeEvent(input$component_type, {
    vals = switch(
      input$component_type,
      "Wind"     = c(A = "WindA",    O = "WindO"),
      "Currents" = c(A = "CurrentA", O = "CurrentO"),
      "Waves"    = c(A = "WaveA",    O = "WaveO")
    )
    updateTextInput(session, "A_name", value = vals[["A"]])
    updateTextInput(session, "O_name", value = vals[["O"]])
  }, ignoreInit = TRUE)
  
  # Create wind/wave/current A/O components
  observeEvent(input$create_ao, ignoreInit = TRUE, {
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Data Table')
    
    if (input$speed != "-" & input$direct != "-") {
      
      # Determine A/O names from radio button
      nm <- switch(
        input$component_type,
        "Wind"     = c(A = "WindA",    O = "WindO"),
        "Currents" = c(A = "CurrentA", O = "CurrentO"),
        "Waves"    = c(A = "WaveA",    O = "WaveO"),
        c(A = "A_comp", O = "O_comp") # fallback
      )
      Aname = nm[["A"]]
      Oname = nm[["O"]]
      
      df = current_data()
      column_names = names(df)
      
      # Save the mapping of what the user selected to create A/O
      if (input$component_type == "Wind") {
        rv_ao_map$wind_speed = input$speed
        rv_ao_map$wind_dir   = input$direct
      } else if (input$component_type == "Currents") {
        rv_ao_map$current_speed = input$speed
        rv_ao_map$current_dir   = input$direct
      } else if (input$component_type == "Waves") {
        # For waves the magnitude is height; we still use the same select for magnitude
        rv_ao_map$wave_height = input$speed
        rv_ao_map$wave_dir    = input$direct
      }
      
      # Compute A/O
      speed_dat = df[, input$speed]
      dir_dat   = df[, input$direct]
      A_comp = -speed_dat * cos((dir_dat - bo()) * pi/180)
      O_comp =  speed_dat * sin((dir_dat - bo()) * pi/180)
      
      hasA = Aname %in% column_names
      hasO = Oname %in% column_names
      
      # Overwrite if exists; append if not
      if (hasA) {
        df[[Aname]] = A_comp
      } else {
        df[[Aname]] = A_comp
        .set(column_props, keys = Aname, values = 2)
      }
      if (hasO) {
        df[[Oname]] = O_comp
      } else {
        df[[Oname]] = O_comp
        .set(column_props, keys = Oname, values = 2)
      }
      
      current_data(df)
      
      rv = response_var() - 1
      
      new_column_names = names(current_data())[-1]
      feat_names(new_column_names[-rv])
      feats_being_used(feat_names())
      
      updateSelectInput(session, "set_column_props", choices = c("-", new_column_names))
      updateSelectInput(session, "rainplot", choices = c("-", new_column_names))
      updateSelectInput(session, "lineplot", choices = c("-", new_column_names))
      updateSelectInput(session, "scatterx", selected = input$scatterx, choices = c("-", new_column_names))
      updateSelectInput(session, "scattery", selected = input$scattery, choices = c("-", new_column_names))
      
      pca_axes_max(ncol(current_data()) - 2)
      PCA_dataset(NULL)
      changed_model(TRUE)
      
      updateNumericInput(session, "num_axes",
                         value = pca_axes_max(),
                         max   = pca_axes_max())
      
      clear_modeling(TRUE)
      
      renderdata(current_data(), response_var(), id_var, input$select_choice, date_format_string, column_props, ignored_rows, current_data_page(), output)
      
    } else {
      showModal(modalDialog(
        div("ERROR: A speed and direction data column must be specified.", style = "font-size:160%"),
        easyClose = TRUE
      ))
    }
  })
  
  # Toggle between regular features and PCA datasets
  observeEvent(input$use_pca_data, ignoreInit = T, {
    
    changed_model(TRUE)
    
    if (input$use_pca_data) {
      
      shinyjs::disable("feats_to_use")
      shinyjs::enable("pcax_to_use")
      
    } else {
      
      shinyjs::enable("feats_to_use")
      shinyjs::disable("pcax_to_use")
    }
    
    # clear_modeling(TRUE)
    
  })
  
  # Perform book-keeping functions when the "Modeling" tab is selected
  observeEvent(input$shinyVB, ignoreInit = T, {
    
    if (input$shinyVB == "Modeling") {
      
      if (is.null(current_data())) {
        return()
        
      } else {
        
        shinyjs::enable("save_project")
        updateCheckboxGroupButtons(session,"feats_to_use",choices=feat_names(),selected=feats_being_used(),size="xs",status = "custom")
        
        if (input$use_pca_data) {
          delay(1,disable("feats_to_use"))
        }
        
        # Filter the response variable to exclude left and right-censored tags
        exclude_values = c(input$lc_val, input$rc_val)
        
        real_responses = na.omit(current_data()[!current_data()[,response_var()] %in% exclude_values,response_var()])
        
        updateNumericInput(session, "LG_binarize_crit_value",value = round(median(real_responses),2),
                           min=min(real_responses),max=max(real_responses))
        updateNumericInput(session, "XGBCL_binarize_crit_value",value = round(median(real_responses),2),
                           min=min(real_responses),max=max(real_responses))
        
        iv$remove_rules("LG_binarize_crit_value")
        iv$remove_rules("XGBCL_binarize_crit_value")
        
        iv$add_rule("LG_binarize_crit_value", sv_between(min(real_responses),max(real_responses)))
        iv$add_rule("XGBCL_binarize_crit_value", sv_between(min(real_responses),max(real_responses)))
      }
      
      if (is.null(PCA_dataset())) {
        return()
        
      } else {
        
        tmp_data = PCA_dataset()
        tmp_data1 = tmp_data[,-1]
        tmp_data2 = tmp_data1[,-1]
        
        pca_axes(colnames(tmp_data2))
        
        updateCheckboxGroupButtons(session,"pcax_to_use",choices=pca_axes(),selected=pcax_being_used(),size="xs",status = "custom")
        
        if (!input$use_pca_data) {
          delay(1,disable("pcax_to_use"))
        }
      }
      
    } else {
      return()
    }
  })
  
  # Catch the selection of less than two features/PCA axes
  observeEvent(input$feats_to_use, ignoreInit = T, {
    
    if (length(input$feats_to_use) < 2) {
      showModal(modalDialog(paste("NOTE: Model pipelines require at least 2 Features."),
                            footer = div(modalButton('Close'))))
      
      updateCheckboxGroupButtons(session,"feats_to_use",choices=feat_names(),selected=feats_being_used(),size="xs",status = "custom")
    }
    else {
      
      changed_model(TRUE)
      
      feats_being_used(input$feats_to_use)
      
      pca_axes_max(length(input$feats_to_use))
      
      updateNumericInput(session, "num_axes",
                         value = pca_axes_max(),
                         max = pca_axes_max()
      )
    }
  })
  
  observeEvent(input$pcax_to_use, ignoreInit = T, {
    
    if (length(input$pcax_to_use) < 2) {
      
      showModal(modalDialog(paste("NOTE: Model pipelines require at least 2 PCA axes."),
                            footer = div(modalButton('Close'))))
      
      updateCheckboxGroupButtons(session,"pcax_to_use",choices=pca_axes(),selected=pcax_being_used(),size="xs",status = "custom")
    } else {
      changed_model(TRUE)
      pcax_being_used(input$pcax_to_use)
    }
    
  })
  
  # Clear output based on non-reactive variables
  observeEvent(refresh_trigger(), ignoreInit = TRUE, {
    output$XGBCL_optim_hp = NULL
    output$XGB_optim_hp = NULL
    refresh_trigger(FALSE)
  })
  
  # LG predictions
  debounced_LG_pred_dc = debounce(reactive(input$LG_pred_dc), plot_delay)
  
  observeEvent(debounced_LG_pred_dc(), ignoreInit = TRUE, {
    if (nrow(LG_pred_scat_dat()) != 0) {
      
      if (is.numeric(debounced_LG_pred_dc())) {
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(input$run_pred_LG, ignoreInit = T, {
    
    response_data = as.numeric(current_data()[,response_var()])
    response_data = response_data[!is.na(response_data)]
    
    if (length(unique(response_data)) > 2 && input$LG_binarize == FALSE) {
      
      showModal(modalDialog(paste("Response Variable must be binarized for this analysis. Choose to 'Binarize' using the button above."),footer = modalButton("Close")))
      
    } else {
      
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
      LG_pred_thresh(crit_value)
      
      data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,FALSE)
      data = data1[,-1]
      
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
        test_data = data[testIndices, ]
        train_data = data[-testIndices, ]
        
        # Create imputed training/testing data
        train_X = train_data[,-1]
        test_X = test_data[,-1]
        
        imp_train_X=missForest(train_X)$ximp
        
        train_test_X = rbind(test_X, imp_train_X)
        imp_test_X = missForest(train_test_X)$ximp[1:nrow(test_X), ]
        
        train_data1 = cbind(train_data[,1], imp_train_X)
        test_data1 = cbind(test_data[,1], imp_test_X)
        
        colnames(train_data1) = colnames(train_data)
        colnames(test_data1) = colnames(test_data)
        
        temp_preds = matrix(0, nrow = nrow(test_data1), ncol = 2*MC_runs)
        temp_preds = data.frame(temp_preds)
        
        temp_coeffs = matrix(0, nrow = ncol(data), ncol = MC_runs+1)
        temp_coeffs = data.frame(temp_coeffs)
        temp_coeffs[,1] = c("(Intercept)",feats_to_use)
        
        withProgress(session=getDefaultReactiveDomain(),
                     message = 'Logistic Prediction Progress',
                     detail = paste("MC run:", x=1,"/",MC_runs,"; Fold:", y=1,"/",tot_folds),
                     value = (f/tot_folds-1/tot_folds),
                     {
                       
                       for (i in 1:MC_runs) {
                         
                         trainData = MC_subbin(train_data1, input$loggy, input$lc_val, input$lc_lowval, input$lc_upval, input$rc_val,
                                               input$rc_lowval, input$rc_upval)
                         
                         testData = MC_subbin(test_data1, input$loggy, input$lc_val, input$lc_lowval, input$lc_upval, input$rc_val,
                                              input$rc_lowval, input$rc_upval)
                         
                         if (input$LG_binarize) {
                           for (j in 1:nrow(trainData)) {
                             trainData[j, 1] = ifelse(test = trainData[j, 1] >= crit_value, yes = 1, no = 0)
                           }
                           for (j in 1:nrow(testData)) {
                             testData[j, 1] = ifelse(test = testData[j, 1] >= crit_value, yes = 1, no = 0)
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
                         
                         tmp_coeffs = coef(model, s = lambda)
                         coeffs = data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = round(tmp_coeffs@x,4))
                         
                         for (h in 1:nrow(coeffs)) {
                           if(temp_coeffs[h,1] %in% coeffs[,1]) {
                             temp_coeffs[h,i+1] = coeffs[which(coeffs[,1] == temp_coeffs[h,1]),2]
                           } else {
                             temp_coeffs[h,i+1] = 0
                           }
                         }
                         
                         temp_preds[,2*i] = round(preds,3)
                         
                         incProgress(1/(MC_runs*tot_folds), detail = paste("MC run:",i,"/",MC_runs,"; Fold:",f,"/",tot_folds))
                         
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
        prediction_results = data.frame(cbind(data1[,1],fold_predictions[,1],fold_predictions[,2],round(data[,-1],4)))
      } else {
        prediction_results = data.frame(cbind(data1[,1],fold_predictions[,1],fold_predictions[,2],data[,-1]))
      }
      
      colnames(prediction_results) = c(colnames(data0)[1],colnames(data0)[rv],"Predictions",feats_to_use)
      
      prediction_results = prediction_results[order(prediction_results[,1]),]
      
      LG_pred_results(prediction_results)
      
      final_coeffs = data.frame(cbind(coeff_folds[,1],round(rowMeans(coeff_folds[,-1]),4),round(exp(rowMeans(coeff_folds[,-1])),4)))
      colnames(final_coeffs) = c("Feature","Coefficient","Odds Ratio")
      
      LG_pred_coeffs(final_coeffs)
      
      LG_pred_scat_dat(LG_pred_results()[,1:3])
      
      LG_pred_confuse_results(confuse(LG_pred_scat_dat()[,2:3],0.5,debounced_LG_pred_dc()))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'LG: Predict')
      
      LG_pred_standardize(input$LG_standard)
      
      updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
    }
  })
  
  observeEvent(c(LG_pred_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(LG_pred_results())) {
      
      output$LG_preds = DT::renderDataTable(server = T, {data = datatable(LG_pred_results(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                        target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp",buttons = c('copy', 'csv', 'excel'),paging = T,
                        pageLength = num_rows_per_page,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                        JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_pred_coeffs = DT::renderDataTable(server = T, {data = datatable(LG_pred_coeffs(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                      target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),
                      paging = F,scrollX = F,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                      initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      x_name = colnames(LG_pred_scat_dat())[[2]]
      y_name = colnames(LG_pred_scat_dat())[[3]]
      
      output$LG_pred_scatplot = renderPlot(ggplot(LG_pred_scat_dat(), aes(x=LG_pred_scat_dat()[,3], fill=as.factor(LG_pred_scat_dat()[,2]))) +
                                             geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
                                             scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                                             geom_vline(xintercept = debounced_LG_pred_dc(), linetype = "dashed", color = "darkgreen") +
                                             labs(x = "Predicted Probability", y = "Density", fill="OBS") +
                                             theme_bw() +
                                             theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                             theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                             theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      LG_pred_confuse_results(confuse(LG_pred_scat_dat()[,2:3],0.5,debounced_LG_pred_dc()))
      
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = LG_pred_confuse_results()$TP
      confuse_table[1,2] = LG_pred_confuse_results()$TN
      confuse_table[1,3] = LG_pred_confuse_results()$FP
      confuse_table[1,4] = LG_pred_confuse_results()$FN
      
      precision = LG_pred_confuse_results()$TP/(LG_pred_confuse_results()$TP+LG_pred_confuse_results()$FP)
      sensitivity = LG_pred_confuse_results()$TP/(LG_pred_confuse_results()$TP+LG_pred_confuse_results()$FN)
      NPV = LG_pred_confuse_results()$TN/(LG_pred_confuse_results()$TN+LG_pred_confuse_results()$FN)
      specificity = LG_pred_confuse_results()$TN/(LG_pred_confuse_results()$TN+LG_pred_confuse_results()$FP)
      PF1_score = 2*(precision * sensitivity)/(precision + sensitivity)
      NF1_score = 2*(NPV * specificity)/(NPV + specificity)
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$LG_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_pred_confuse_text = renderText({paste0("Sensitivity = ",round(LG_pred_confuse_results()$Sensitivity,3),"; Specificity = ",
                  round(LG_pred_confuse_results()$Specificity,3),"; Accuracy = ",round(LG_pred_confuse_results()$Accuracy,3),
                  "; Positive F1 = ",round(PF1_score,3),
                  "; Negative F1 = ",round(NF1_score,3))})
      
      refresh_trigger(FALSE)
      
    } else if (is.null(LG_pred_results())) {
      
      output$LG_preds = NULL
      output$LG_pred_coeffs = NULL
      output$LG_pred_scatplot = NULL
      output$LG_pred_confuse = NULL
      output$LG_pred_confuse_text = NULL
      refresh_trigger(FALSE)
    }
  })
  
  # LG fitting
  debounced_LG_fit_dc = debounce(reactive(input$LG_fit_dc), plot_delay)
  
  observeEvent(debounced_LG_fit_dc(), ignoreInit = T, {
    
    if (nrow(LG_scat_dat()) != 0) {
      
      if (is.numeric(debounced_LG_fit_dc())) {
        LG_crit_prob(debounced_LG_fit_dc())
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(input$run_fitted_LG, ignoreInit = T, {
    
    response_data = as.numeric(current_data()[,response_var()])
    response_data = response_data[!is.na(response_data)]
    LG_pred_data(NULL)
    
    if (length(unique(response_data)) > 2 && input$LG_binarize == FALSE) {
      
      showModal(modalDialog(paste("Response Variable must be binarized for this analysis. Choose to 'Binarize' using the button above."),footer = modalButton("Close")))
      
    } else {
      
      req(iv$is_valid())
      
      updateNumericInput(session, "num_preds",value = 2)
      changed_model(TRUE)
      
      if (input$use_pca_data) {
        data0 = PCA_dataset()
        rv=2
        feats_to_use = input$pcax_to_use
        LG_final_features(PCA_coefficients()[,1])
        ignored_rows = NULL
        LG_model_PCA(TRUE)
      } else {
        data0 = current_data()
        rv=response_var()
        feats_to_use = input$feats_to_use
        LG_final_features(feats_to_use)
        LG_model_PCA(FALSE)
      }
      
      set.seed(input$model_seed)
      crit_value = input$LG_binarize_crit_value
      LG_thresh(crit_value)
      
      data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,FALSE)
      data = data1[,-1]
      
      # Create imputed data
      imp_X=missForest(data[,-1])$ximp
      imp_data = cbind(data[,1], imp_X)
      colnames(imp_data) = colnames(data)
      
      fitted_coeffs = matrix(0, nrow = ncol(imp_data), ncol = 3)
      fitted_coeffs = data.frame(fitted_coeffs)
      fitted_coeffs[,1] = c("(Intercept)",feats_to_use)
      
      fitted_values = matrix(0, nrow = 0, ncol = 2)
      fitted_values = data.frame(fitted_values)
      
      modeling_data = MC_final_subbin(imp_data,input$loggy,input$lc_val,input$rc_val,0.5,1.5)
      
      # Binarize Response
      
      if (input$LG_binarize) {
        for (j in 1:nrow(modeling_data)) {
          modeling_data[j, 1] = ifelse(test = modeling_data[j, 1] >= crit_value, yes = 1, no = 0)
        }
      }
      
      # determine best alpha and lambda
      fit_mod = cva.glmnet(x=as.matrix(modeling_data[,-1]),y=modeling_data[,1],nfolds=input$num_folds,family="binomial",
                           type.measure=input$LG_eval,na.action="na.omit",standardize=input$LG_standard,intercept=TRUE)
      
      alpha = get_model_params(fit_mod)$alpha
      lambda = get_model_params(fit_mod)$lambdaMin
      
      # fit training model
      logreg_model = glmnet(x=as.matrix(modeling_data[,-1]),modeling_data[,1],lambda=lambda, alpha=alpha, na.action="na.omit",family="binomial",
                     type.measure=input$LG_eval,standardize=input$LG_standard,intercept=TRUE)
      
      LG_model <<- logreg_model
      
      fits = round(predict(logreg_model, newx = as.matrix(modeling_data[,-1]), type = "response"),3)
      
      tmp_coeffs = coef(logreg_model, s = lambda)
      coeffs = data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = round(tmp_coeffs@x,4))
      
      for (h in 1:nrow(fitted_coeffs)) {
        if(fitted_coeffs[h,1] %in% coeffs[,1]) {
          fitted_coeffs[h,2] = coeffs[which(coeffs[,1] == fitted_coeffs[h,1]),2]
        } else {
          fitted_coeffs[h,2] = 0
        }
      }

      fitted_coeffs[,3] = round(exp(fitted_coeffs[,2]),4)
      # fitted_coeffs[1,3] = NA
      colnames(fitted_coeffs) = c("Feature","Coefficient","Odds Ratio")
      LG_coeffs(fitted_coeffs)
      
      model_results = cbind(modeling_data[,1],fits)
      
      if (input$use_pca_data) {
        fitted_model_results = data.frame(cbind(data1[,1],model_results[,1],model_results[,2],round(data[,-1],4)))
      } else {
        fitted_model_results = data.frame(cbind(data1[,1],model_results[,1],model_results[,2],data[,-1]))
      }
      
      colnames(fitted_model_results) = c(colnames(data0)[1],colnames(data0)[rv],"Fitted_Prob",feats_to_use)
      LG_results(fitted_model_results[order(fitted_model_results[,1]),])

      LG_scat_dat(LG_results()[,1:3])
      
      LG_confuse_results(confuse(LG_scat_dat()[,2:3],0.5,input$LG_fit_dc))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'LG: Fitting')
      
      LG_standardize(input$LG_standard)
      
      updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
      
    }
    
  })
  
  observeEvent(c(LG_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(LG_results())) {
      
      output$LG_fits = DT::renderDataTable(server = T, {data = datatable(LG_results(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp",buttons = c('copy', 'csv', 'excel'),paging = T,
                  pageLength = num_rows_per_page,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_coeffs = DT::renderDataTable(server = T, {data = datatable(LG_coeffs(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),
                  paging = F,scrollX = F,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                  initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      x_name = colnames(LG_scat_dat())[[2]]
      y_name = colnames(LG_scat_dat())[[3]]
      
      output$LG_scatplot = renderPlot(ggplot(LG_scat_dat(), aes(x=LG_scat_dat()[,3], fill=as.factor(LG_scat_dat()[,2]))) +
                                        geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
                                        scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                                        geom_vline(xintercept = debounced_LG_fit_dc(), linetype = "dashed", color = "darkgreen") +
                                        labs(x = "Fitted Probability", y = "Density", fill="OBS") +
                                        theme_bw() +
                                        theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                        theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                        theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      
      LG_confuse_results(confuse(LG_scat_dat()[,2:3],debounced_LG_fit_dc(),debounced_LG_fit_dc()))
      
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = LG_confuse_results()$TP
      confuse_table[1,2] = LG_confuse_results()$TN
      confuse_table[1,3] = LG_confuse_results()$FP
      confuse_table[1,4] = LG_confuse_results()$FN
      
      precision = LG_confuse_results()$TP/(LG_confuse_results()$TP+LG_confuse_results()$FP)
      sensitivity = LG_confuse_results()$TP/(LG_confuse_results()$TP+LG_confuse_results()$FN)
      NPV = LG_confuse_results()$TN/(LG_confuse_results()$TN+LG_confuse_results()$FN)
      specificity = LG_confuse_results()$TN/(LG_confuse_results()$TN+LG_confuse_results()$FP)
      PF1_score = 2*(precision * sensitivity)/(precision + sensitivity)
      NF1_score = 2*(NPV * specificity)/(NPV + specificity)
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$LG_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_confuse_text = renderText({paste0("Sensitivity = ",round(LG_confuse_results()$Sensitivity,3),"; Specificity = ",
                  round(LG_confuse_results()$Specificity,3),"; Accuracy = ",round(LG_confuse_results()$Accuracy,3),
                  "; Positive F1 = ",round(PF1_score,3),
                  "; Negative F1 = ",round(NF1_score,3))})
      
      refresh_trigger(FALSE)
      
    } else if (is.null(LG_results())) {
      
      output$LG_fits = NULL
      output$LG_coeffs = NULL
      output$LG_scatplot = NULL
      output$LG_confuse = NULL
      output$LG_confuse_text = NULL
      refresh_trigger(FALSE)
    }
  })
  
  # XGBCL HP optimization
  observeEvent(c(input$XGBCL_optimize_HP,refresh_XGBCL_Optim_HP()), ignoreInit = T, {
    
    output$XGBCL_optim_hp = DT::renderDataTable(server=T,{data = datatable(t(Optimal_CLHP),rownames=T,colnames=NULL,extensions='Buttons',selection=list(selected =
          list(rows = NULL, cols = NULL),target = "row",mode="single"),editable=F,options = list(autoWidth=F,dom='t',paging = F,scrollX = F,
          scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
          initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    showModal(modalDialog(title="HP Optimization", card(
      fluidRow(
        column(4,numericInput("psocl_max_iter", "Max Iterations", min=5, max=1000, value=20, step = 1)),
        column(2),
        column(4,numericInput("psocl_swarm_size", "Swarm Size", min=3, max=200, value=10, step = 1))),
      fluidRow(
        column(4,numericInput("membercl_exp", "Membership Weight", min=0.25, max=3, value=0.5, step = 0.25)),
        column(2),
        column(4,numericInput("sscl_exp", "Sum of Squares Weight", min=0.25, max=3, value=1, step = 0.25))),
      fluidRow(fluidRow(DT::dataTableOutput('XGBCL_optim_hp')))),
      footer = div(actionButton("run_XGBCL_optimize_HP", "Run"),modalButton('Close'))))
    
    refresh_XGBCL_Optim_HP(FALSE)
  })
  
  observeEvent(input$run_XGBCL_optimize_HP, ignoreInit = T, {
    
    response_data = as.numeric(current_data()[,response_var()])
    response_data = response_data[!is.na(response_data)]
    
    if (length(unique(response_data)) > 2 && input$XGBCL_binarize == FALSE) {
      
      showModal(modalDialog(paste("Response Variable must be binarized for this analysis. Choose to 'Binarize' using the button above."),footer = modalButton("Close")))
      
    } else {
      
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
      
      data = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,input$XGBCL_standard)
      dataset = data[,-1]
      
      xgbcl_optim_HP_results = xgbcl_pso(dataset,rv,feats_to_use,input$lc_val,input$rc_val,input$lc_lowval,input$lc_upval,input$rc_lowval,
                                         input$rc_upval,input$MC_runs,input$num_folds,input$loggy,input$XGBCL_eval,input$psocl_max_iter,
                                         input$psocl_swarm_size,input$membercl_exp,input$sscl_exp,input$XGBCL_binarize,input$XGBCL_binarize_crit_value,MC_subbin)
      
      xgbcl_optim_HP_results1 = data.frame(xgbcl_optim_HP_results)
      
      Optimal_CLHP$max_depth <<- round(xgbcl_optim_HP_results1[1,1],0)
      Optimal_CLHP$eta <<- round(xgbcl_optim_HP_results1[2,1],3)
      Optimal_CLHP$subsample <<- round(xgbcl_optim_HP_results1[3,1],2)
      Optimal_CLHP$colsample_bytree <<- round(xgbcl_optim_HP_results1[4,1],2)
      Optimal_CLHP$min_child_weight <<- round(xgbcl_optim_HP_results1[5,1],0)
      Optimal_CLHP$gamma <<- round(xgbcl_optim_HP_results1[6,1],1)
      Optimal_CLHP$nrounds <<- round(xgbcl_optim_HP_results1[7,1],0)
      
      refresh_XGBCL_Optim_HP(TRUE)
    }
  })
  
  # XGBCL set HP values
  observeEvent(input$XGBCL_params, ignoreInit = T, {
    
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
  
  observeEvent(input$XGBCL_booster, ignoreInit = T, {
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
  observeEvent(input$run_XGBCL_select, ignoreInit = T, {
    
    response_data = as.numeric(current_data()[,response_var()])
    response_data = response_data[!is.na(response_data)]
    
    if (length(unique(response_data)) > 2 && input$XGBCL_binarize == FALSE) {
      
      showModal(modalDialog(paste("Response Variable must be binarized for this analysis. Choose to 'Binarize' using the button above."),footer = modalButton("Close")))
      
    } else {
      
      if(running())
        return(NULL)
      running(TRUE)
      
      req(iv$is_valid())
      
      if (input$use_pca_data) {
        data = PCA_dataset()
        rv=2
        feats_to_use = input$pcax_to_use
        ignored_rows = NULL
        fs_pcax_used(feats_to_use)
      } else {
        data = current_data()
        rv=response_var()
        feats_to_use = input$feats_to_use
        fs_feats_used(feats_to_use)
      }
      
      crit_value = input$XGBCL_binarize_crit_value
      eval_metric = input$XGBCL_eval
      
      eta = etacl_set()
      gamma = gammacl_set()
      max_depth = max_depthcl_set()
      min_child_weight = min_child_weightcl_set()
      nrounds = nroundscl_set()
      early_stop = early_stop_set()
      subsamp = subsampcl_set()
      colsamp = colsampcl_set()
      
      xgb_tree_method = xgbcl_tree_method_set()
      xgb_booster = xgbcl_booster_set()
      dart_normalize_type = dartcl_normalize_type_set()
      dart_sample_type = dartcl_sample_type_set()
      rate_drop = ratecl_drop_set()
      skip_drop = skipcl_drop_set()
      
      standardize = input$XGBCL_standard
      binarize = input$XGBCL_binarize
      lc_val = input$lc_val
      rc_val = input$rc_val
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
        
        xgbcl_selection(data,seed,rv,feats_to_use,ignored_rows,crit_value,eval_metric,lc_val,rc_val,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,
                        standardize,binarize,xgb_tree_method,xgb_booster,dart_normalize_type,dart_sample_type,rate_drop,skip_drop,eta,gamma,max_depth,
                        min_child_weight,subsamp,colsamp,nrounds,early_stop,temp_db,MC_subbin,create_data)
        
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
      colnames(final_xgbcl_select_result) = c(colnames(xgbcl_selection_results),"Wght Mean LnLoss")
      
      XGBCL_selection_results(final_xgbcl_select_result)
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGBCL: Feat Select')
      
      #Return something other than the future so we don't block the UI
      NULL
    }
  })
  
  observeEvent(c(XGBCL_selection_results(),refresh_trigger()), ignoreInit = T, {
    
    if (!is.null(XGBCL_selection_results())) {
      
      output$XGBCL_select = DT::renderDataTable(server=T,{
        data = datatable(XGBCL_selection_results(),rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode="single"),editable=F,extensions="Buttons", options = list(autoWidth=F,dom='tB',paging = F,pageLength = num_rows_per_page,scrollX = F,
                  scrollY = TRUE,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                  initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}"))) %>%
        formatRound(columns=c(1,3:6), digits=c(0,4,4,4,4))
        
        data$x$data[[1]] = as.numeric(data$x$data[[1]])
        data$x$data[[3]] = as.numeric(data$x$data[[3]])
        data$x$data[[4]] = as.numeric(data$x$data[[4]])
        data$x$data[[5]] = as.numeric(data$x$data[[5]])
        data$x$data[[6]] = as.numeric(data$x$data[[6]])
        data
        
      })
      
      refresh_trigger(FALSE)
      
    } else if (is.null(XGBCL_selection_results())) {
      
      output$XGBCL_select = NULL
      refresh_trigger(FALSE)
    }
  })
  
  observeEvent(input$testcl_weight, ignoreInit = T, {
    
    tables = dbListTables(temp_db)
    
    if ('xgbcl_selection_results' %in% tables) {
      
      xgbcl_selection_results = dbReadTable(temp_db, "xgbcl_selection_results")
      xgbcl_selection_results = xgbcl_selection_results[,-3]
      xgbcl_selection_results = xgbcl_selection_results[,-2]
      
      weighted_mean = round(input$testcl_weight*as.numeric(xgbcl_selection_results[,5])+(1-input$testcl_weight)*as.numeric(xgbcl_selection_results[,4]),4)
      final_xgbcl_select_result = cbind(xgbcl_selection_results,weighted_mean)
      colnames(final_xgbcl_select_result) = c(colnames(xgbcl_selection_results),"Weighted Mean LnLoss")
      
      XGBCL_selection_results(final_xgbcl_select_result)
      
    }
  })
  
  observeEvent(input$XGBCL_select_cancel, ignoreInit = T, {
    print("Stopping calculation...")
    stopMulticoreFuture(xgbcl_select_calculation)
  })
  
  observeEvent(input$XGBCL_select_rows_selected, ignoreInit = T, {
    
    if (input$use_pca_data) {
      all_feats = fs_pcax_used()
    } else {
      all_feats = fs_feats_used()
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
      updateCheckboxGroupButtons(session,"pcax_to_use",choices=pca_axes(),selected=remaining,size="xs",status = "custom")
    } else {
      updateCheckboxGroupButtons(session,"feats_to_use",choices=feat_names(),selected=remaining,size="xs",status = "custom")
    }
  })
  
  # XGBCL predictions
  debounced_XGBCL_pred_dc = debounce(reactive(input$XGBCL_pred_dc), plot_delay)
  
  observeEvent(debounced_XGBCL_pred_dc(), ignoreInit = T, {
    
    if (nrow(XGBCL_pred_scat_dat()) != 0) {
      
      if (is.numeric(debounced_XGBCL_pred_dc())) {
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(input$run_pred_XGBCL, ignoreInit = T, {
    
    response_data = as.numeric(current_data()[,response_var()])
    response_data = response_data[!is.na(response_data)]
    
    if (length(unique(response_data)) > 2 && input$XGBCL_binarize == FALSE) {
      
      showModal(modalDialog(paste("Response Variable must be binarized for this analysis. Choose to 'Binarize' using the button above."),footer = modalButton("Close")))
      
    } else {
      
      req(iv$is_valid())
      
      XGBCL_pred_thresh(input$XGBCL_binarize_crit_value)
      
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
      
      xgbcl_pred_results = xgbcl_call_predict(data,rv,id_var,input$model_seed,ignored_rows,feats_to_use,input$XGBCL_eval,input$lc_val,input$rc_val,input$lc_lowval,
                                              input$lc_upval,input$rc_lowval,input$rc_upval,input$train_pct/100,input$MC_runs,input$num_folds,input$loggy,input$randomize,
                                              input$XGBCL_standard,xgbcl_tree_method_set(),xgbcl_booster_set(),dartcl_normalize_type_set(),dartcl_sample_type_set(),
                                              ratecl_drop_set(),skipcl_drop_set(),Optimal_CLHP$eta,Optimal_CLHP$gamma,Optimal_CLHP$max_depth,
                                              Optimal_CLHP$min_child_weight,Optimal_CLHP$subsample,Optimal_CLHP$colsample_bytree,Optimal_CLHP$nrounds,
                                              input$XGBCL_binarize,input$XGBCL_binarize_crit_value)
      
      XGBCL_pred_results(data.frame(xgbcl_pred_results[[1]]))
      
      xgbcl_pred_coeffs= data.frame(xgbcl_pred_results[[2]])
      colnames(xgbcl_pred_coeffs) = c("Feature","SHAP Value")
      XGBCL_pred_coeffs(xgbcl_pred_coeffs)
      
      XGBCL_pred_scat_dat(XGBCL_pred_results()[,1:3])
      
      XGBCL_pred_confuse_results(confuse(XGBCL_pred_scat_dat()[,2:3],debounced_XGBCL_pred_dc(),debounced_XGBCL_pred_dc()))
      
      removeModal()
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGBCL: Predict')
    }
    
    XGBCL_pred_standardize(input$XGBCL_standard)
    
    updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
    
  })
  
  observeEvent(c(XGBCL_pred_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(XGBCL_pred_results())) {
      
      output$XGBCL_predictions = DT::renderDataTable(server = T, {data = datatable(XGBCL_pred_results(),rownames = F,selection =
              list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions='Buttons',options = list(autoWidth = F,
              paging = TRUE,pageLength = num_rows_per_page,dom="ltBp",buttons = c('copy', 'csv', 'excel'),scrollX = TRUE,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',
              orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744',
              'color': '#fff'});","}")))})
      
      output$XGBCL_pred_shapes = DT::renderDataTable(server = T, {data = datatable(XGBCL_pred_coeffs(),rownames = F,selection =
              list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
              list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
              className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGBCL_pred_scatplot = renderPlot(ggplot(XGBCL_pred_scat_dat(), aes(x=XGBCL_pred_scat_dat()[,3], fill=as.factor(XGBCL_pred_scat_dat()[,2]))) +
                                                geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
                                                scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                                                geom_vline(xintercept = debounced_XGBCL_pred_dc(), linetype = "dashed", color = "darkgreen") +
                                                labs(x = "Predicted Probability", y = "Density", fill="OBS") +
                                                theme_bw() +
                                                theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                                theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                                theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      XGBCL_pred_confuse_results(confuse(XGBCL_pred_scat_dat()[,2:3],debounced_XGBCL_pred_dc(),debounced_XGBCL_pred_dc()))
      
      confuse_table = data.frame(matrix(0,nrow=1,ncol=4))
      
      confuse_table[1,1] = XGBCL_pred_confuse_results()$TP
      confuse_table[1,2] = XGBCL_pred_confuse_results()$TN
      confuse_table[1,3] = XGBCL_pred_confuse_results()$FP
      confuse_table[1,4] = XGBCL_pred_confuse_results()$FN
      
      precision = XGBCL_pred_confuse_results()$TP/(XGBCL_pred_confuse_results()$TP+XGBCL_pred_confuse_results()$FP)
      sensitivity = XGBCL_pred_confuse_results()$TP/(XGBCL_pred_confuse_results()$TP+XGBCL_pred_confuse_results()$FN)
      NPV = XGBCL_pred_confuse_results()$TN/(XGBCL_pred_confuse_results()$TN+XGBCL_pred_confuse_results()$FN)
      specificity = XGBCL_pred_confuse_results()$TN/(XGBCL_pred_confuse_results()$TN+XGBCL_pred_confuse_results()$FP)
      PF1_score = 2*(precision * sensitivity)/(precision + sensitivity)
      NF1_score = 2*(NPV * specificity)/(NPV + specificity)
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGBCL_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGBCL_pred_confuse_text = renderText({paste0("Sensitivity = ",round(XGBCL_pred_confuse_results()$Sensitivity,3),"; Specificity = ",
                      round(XGBCL_pred_confuse_results()$Specificity,3),"; Accuracy = ",round(XGBCL_pred_confuse_results()$Accuracy,3),
                      "; Positive F1 = ",round(PF1_score,3),
                      "; Negative F1 = ",round(NF1_score,3))})
      
      output$XGBCL_used_hp_pred = DT::renderDataTable(server=T,{data = datatable(Optimal_CLHP,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                      target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                      pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                      initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      refresh_trigger(FALSE)
      
    } else if (is.null(XGBCL_pred_results())) {
      
      output$XGBCL_predictions = NULL
      output$XGBCL_pred_shapes = NULL
      output$XGBCL_pred_scatplot = NULL
      output$XGBCL_pred_confuse = NULL
      output$XGBCL_pred_confuse_text = NULL
      output$XGBCL_used_hp_pred = NULL
      refresh_trigger(FALSE)
    }
  })
  
  # XGBCL fitting
  debounced_XGBCL_dec_crit = debounce(reactive(input$XGBCL_dec_crit), plot_delay)
  
  observeEvent(debounced_XGBCL_dec_crit(), ignoreInit = T, {
    
    if (nrow(XGBCL_scat_dat()) != 0) {
      
      if (is.numeric(debounced_XGBCL_dec_crit())) {
        XGBCL_crit_prob(debounced_XGBCL_dec_crit())
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(input$run_fit_XGBCL, ignoreInit = T, {
    
    response_data = as.numeric(current_data()[,response_var()])
    response_data = response_data[!is.na(response_data)]
    XGBCL_pred_data(NULL)
    
    if (length(unique(response_data)) > 2 && input$XGBCL_binarize == FALSE) {
      
      showModal(modalDialog(paste("Response Variable must be binarized for this analysis. Choose to 'Binarize' using the button above."),footer = modalButton("Close")))
      
    } else {
      
      req(iv$is_valid())
      
      updateNumericInput(session, "num_preds",value = 2)
      changed_model(TRUE)
      
      if (input$use_pca_data) {
        data0 = PCA_dataset()
        rv=2
        XGBCL_final_features(PCA_coefficients()[,1])
        feats_to_use = input$pcax_to_use
        ignored_rows = NULL
        XGBCL_model_PCA(TRUE)
      } else {
        data0 = current_data()
        rv=response_var()
        XGBCL_final_features(input$feats_to_use)
        feats_to_use = input$feats_to_use
        XGBCL_model_PCA(FALSE)
      }
      
      crit_value = input$XGBCL_binarize_crit_value
      XGBCL_thresh(crit_value)
      
      data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,input$XGBCL_standard)
      data = data1[,-1]
      
      modeling_data = MC_final_subbin(data,input$loggy,input$lc_val,input$rc_val,0.5,1.5)
      
      if (input$XGBCL_binarize) {
        for (j in 1:nrow(modeling_data)) {
          modeling_data[j, 1] = ifelse(test = modeling_data[j, 1] >= crit_value, yes = 1, no = 0)
        }
      }
      
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
      
      xgbcl_model = xgboost(data = as.matrix(modeling_data[,-1]),label=modeling_data[,1], params=params, early_stopping_rounds=early_stop_set(), nrounds=1000, verbose=0)
      
      XGBCL_model <<- xgbcl_model
      
      shap_values = shap.values(xgb_model = xgbcl_model, X_train = as.matrix(modeling_data[,-1]))
      mean_shaps = shap_values$mean_shap_score
      shap_names = names(mean_shaps)
      shap_temp = data.frame(cbind(shap_names,mean_shaps))
      
      XGBCL_shapes = data.frame(shap_temp,row.names = NULL)
      XGBCL_shapes$mean_shaps = as.numeric(XGBCL_shapes$mean_shaps)
      XGBCL_shapes$mean_shaps = format(XGBCL_shapes$mean_shaps,scientific=FALSE,digits=3)
      XGBCL_shapes[order(XGBCL_shapes$mean_shaps,decreasing=TRUE),]
      colnames(XGBCL_shapes) = c("Feature","Mean SHAP")
      
      XGBCL_coeffs(XGBCL_shapes)
      
      fitted = predict(xgbcl_model, newdata=as.matrix(modeling_data[,-1]))
      fits = cbind(modeling_data[,1],round(fitted,3))
      
      if (input$use_pca_data) {
        xgbcl_results = data.frame(cbind(data1[,1],fits[,1],fits[,2],round(data[,-1],4)))
      } else {
        xgbcl_results = data.frame(cbind(data1[,1],fits[,1],fits[,2],data[,-1]))
      }
      
      colnames(xgbcl_results) = c(colnames(data0)[[1]],colnames(data0)[[rv]],"Fitted_Prob",colnames(data[,-1]))
      
      xgbcl_results = xgbcl_results[order(xgbcl_results[,1]),]
      XGBCL_results(xgbcl_results)
      
      XGBCL_scat_dat(XGBCL_results()[,1:3])
      
      XGBCL_confuse_results(confuse(XGBCL_scat_dat()[,2:3],input$XGBCL_dec_crit,input$XGBCL_dec_crit))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGBCL: Fitting')
      
      XGBCL_standardize(input$XGBCL_standard)
      
      updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
    }
  })
  
  observeEvent(c(XGBCL_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(XGBCL_results())) {
      
      output$XGBCL_shapes = DT::renderDataTable(server = T, {data = datatable(XGBCL_coeffs(),rownames = F,selection =
                    list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                    list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                    className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGBCL_fits = DT::renderDataTable(server = T, {data = datatable(XGBCL_results(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                    target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp", buttons = c('copy', 'csv', 'excel'),
                    paging = T,pageLength = num_rows_per_page,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                    JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGBCL_scatplot = renderPlot(ggplot(XGBCL_scat_dat(), aes(x=XGBCL_scat_dat()[,3], fill=as.factor(XGBCL_scat_dat()[,2]))) +
                                           geom_density(alpha = 0.6, color = "black", linewidth = 0.5) +
                                           scale_fill_manual(values = c("0" = "gray", "1" = "cadetblue")) +
                                           geom_vline(xintercept = debounced_XGBCL_dec_crit(), linetype = "dashed", color = "darkgreen") +
                                           labs(x = "Fitted Probability", y = "Density", fill="OBS") +
                                           theme_bw() +
                                           theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                           theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                           theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      XGBCL_confuse_results(confuse(XGBCL_scat_dat()[,2:3],debounced_XGBCL_dec_crit(),debounced_XGBCL_dec_crit()))
      
      xgbcl_confuse_table = matrix(0,nrow=1,ncol=4)
      
      xgbcl_confuse_table[1,1] = XGBCL_confuse_results()$TP
      xgbcl_confuse_table[1,2] = XGBCL_confuse_results()$TN
      xgbcl_confuse_table[1,3] = XGBCL_confuse_results()$FP
      xgbcl_confuse_table[1,4] = XGBCL_confuse_results()$FN
      
      precision = XGBCL_confuse_results()$TP/(XGBCL_confuse_results()$TP+XGBCL_confuse_results()$FP)
      sensitivity = XGBCL_confuse_results()$TP/(XGBCL_confuse_results()$TP+XGBCL_confuse_results()$FN)
      NPV = XGBCL_confuse_results()$TN/(XGBCL_confuse_results()$TN+XGBCL_confuse_results()$FN)
      specificity = XGBCL_confuse_results()$TN/(XGBCL_confuse_results()$TN+XGBCL_confuse_results()$FP)
      PF1_score = 2*(precision * sensitivity)/(precision + sensitivity)
      NF1_score = 2*(NPV * specificity)/(NPV + specificity)
      
      colnames(xgbcl_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGBCL_confuse = DT::renderDataTable(server = T, {data = datatable(xgbcl_confuse_table,rownames = F,selection = 
                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                    options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                    columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                    {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGBCL_confuse_text = renderText({paste0("Sensitivity = ",round(XGBCL_confuse_results()$Sensitivity,3),"; Specificity = ",
                    round(XGBCL_confuse_results()$Specificity,3),"; Accuracy = ",round(XGBCL_confuse_results()$Accuracy,3),
                    "; Positive F1 = ",round(PF1_score,3),
                    "; Negative F1 = ",round(NF1_score,3))})
      
      output$XGBCL_used_hp = DT::renderDataTable(server=T,{data = datatable(Optimal_CLHP,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                    target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                    pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                    initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      refresh_trigger(FALSE)
      
    } else if (is.null(XGBCL_results())) {
      
      output$XGBCL_shapes = NULL
      output$XGBCL_fits = NULL
      output$XGBCL_scatplot = NULL
      output$XGBCL_confuse = NULL
      output$XGBCL_confuse_text = NULL
      output$XGBCL_used_hp = NULL
      refresh_trigger(FALSE)
    }
  })
  
  # Create PDP for a feature in the XGBCL model
  observeEvent(input$XGBCL_shapes_rows_selected, ignoreInit = T, {
    
    selected_index = input$XGBCL_shapes_rows_selected[1]
    selected_feature = XGBCL_coeffs()[selected_index,1]
    
    used_data = na.omit(XGBCL_final_data())
    
    train_data = as.data.frame(used_data[,-1])
    response = as.numeric(used_data[,1])
    
    predcl_fun = function(model, newdata) {
      predictions = predict(model, newdata = as.matrix(newdata))
      return(as.numeric(predictions))
    }
    
    predictor.xgbcl = Predictor$new(
      model = XGBCL_model,
      data = train_data,
      y = response,
      predict.fun = predcl_fun
    )
    
    pdp_info =  FeatureEffect$new(
      predictor.xgbcl,
      selected_feature,
      method = "pdp",
      grid.size = 25
    )
    
    output$XGBCL_pdp_plot = renderPlot({plot(pdp_info$plot())})
    
    showModal(modalDialog(
      title = paste("Partial Dependence Plot: ", selected_feature),
      size = "l",
      plotOutput("XGBCL_pdp_plot"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # XGB feature Selection
  observeEvent(input$run_XGB_select, ignoreInit = T, {
    
    if(running())
      return(NULL)
    running(TRUE)
    
    req(iv$is_valid())
    
    if (input$use_pca_data) {
      data = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
      fs_pcax_used(feats_to_use)
    } else {
      data = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
      fs_feats_used(feats_to_use)
    }
    
    eta = eta_set()
    gamma = gamma_set()
    max_depth = max_depth_set()
    min_child_weight = min_child_weight_set()
    nrounds = nrounds_set()
    early_stop = early_stop_set()
    subsamp = subsamp_set()
    colsamp = colsamp_set()
    
    xgb_tree_method = xgb_tree_method_set()
    xgb_booster = xgb_booster_set()
    dart_normalize_type = dart_normalize_type_set()
    dart_sample_type = dart_sample_type_set()
    rate_drop = rate_drop_set()
    skip_drop = skip_drop_set()
    
    standardize = input$XGB_standard
    lc_val = input$lc_val
    rc_val = input$rc_val
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
      
      xgb_selection(data,seed,rv,feats_to_use,ignored_rows,lc_val,rc_val,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,
                    standardize,xgb_tree_method,xgb_booster,dart_normalize_type,dart_sample_type,rate_drop,skip_drop,eta,gamma,max_depth,
                    min_child_weight,subsamp,colsamp,nrounds,early_stop,temp_db,MC_subbin,create_data)
      
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
    
    XGB_selection_results(final_xgb_select_result)
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Feat Select')
    
    #Return something other than the future so we don't block the UI
    NULL
  })
  
  observeEvent(c(XGB_selection_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(XGB_selection_results())) {
      
      output$XGB_select = DT::renderDataTable(server=T,{
        data = datatable(XGB_selection_results(),rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
              target = "row",mode="single"),editable=F,extensions="Buttons", options = list(autoWidth=F,dom='tB',paging = F,pageLength = num_rows_per_page,scrollX = F,
              scrollY = TRUE,buttons = c('copy', 'csv', 'excel'),columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
              initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}"))) %>%
          formatRound(columns=c(1,3:6), digits=c(0,4,4,4,4))
        
        data$x$data[[1]] = as.numeric(data$x$data[[1]])
        data$x$data[[3]] = as.numeric(data$x$data[[3]])
        data$x$data[[4]] = as.numeric(data$x$data[[4]])
        data$x$data[[5]] = as.numeric(data$x$data[[5]])
        data$x$data[[6]] = as.numeric(data$x$data[[6]])
        data
        
      })
      
      refresh_trigger(FALSE)
      
    } else if (is.null(XGB_selection_results())) {
      
      output$XGB_select = NULL
      refresh_trigger(FALSE)
    }
  })
  
  observeEvent(input$test_weight, ignoreInit = T, {
    
    tables = dbListTables(temp_db)
    
    if ('xgb_selection_results' %in% tables) {
      
      xgb_selection_results = dbReadTable(temp_db, "xgb_selection_results")
      xgb_selection_results = xgb_selection_results[,-3]
      xgb_selection_results = xgb_selection_results[,-2]
      
      weighted_mean = round(input$test_weight*as.numeric(xgb_selection_results[,5])+(1-input$test_weight)*as.numeric(xgb_selection_results[,4]),4)
      final_xgb_select_result = cbind(xgb_selection_results,weighted_mean)
      colnames(final_xgb_select_result) = c(colnames(xgb_selection_results),"Weighted Mean RMSE")
      
      XGB_selection_results(final_xgb_select_result)
    }
  })
  
  observeEvent(input$XGB_select_cancel, ignoreInit = T, {
    print("Stopping calculation...")
    stopMulticoreFuture(xgb_select_calculation)
  })
  
  observeEvent(input$XGB_select_rows_selected, ignoreInit = T, {
    
    if (input$use_pca_data) {
      all_feats = fs_pcax_used()
    } else {
      all_feats = fs_feats_used()
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
      updateCheckboxGroupButtons(session,"pcax_to_use",choices=pca_axes(),selected=remaining,size="xs",status = "custom")
    } else {
      updateCheckboxGroupButtons(session,"feats_to_use",choices=feat_names(),selected=remaining,size="xs",status = "custom")
    }
  })
  
  # XGB HP optimization
  observeEvent(c(input$XGB_optimize_HP,refresh_XGB_Optim_HP()), ignoreInit = T, {
    
    output$XGB_optim_hp = DT::renderDataTable(server=T,{data = datatable(t(Optimal_HP),rownames=T,colnames=NULL,extensions='Buttons',selection=list(selected =
              list(rows = NULL, cols = NULL),target = "row",mode="single"),editable=F,options = list(autoWidth=F,dom='t',paging = F,scrollX = F,
              scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
              initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
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
        column(4,numericInput("ss_exp", "Sum of Squares Weight", min=0.25, max=3, value=1, step = 0.25))),
      fluidRow(fluidRow(DT::dataTableOutput('XGB_optim_hp')))),
      footer = div(actionButton("run_XGB_optimize_HP", "Run"),modalButton('Close'))))#,actionButton("stop_xgb_HP_and_errors", "Cancel the Calculation"#))
    
    refresh_XGB_Optim_HP(FALSE)
  })
  
  observeEvent(input$run_XGB_optimize_HP, ignoreInit = T, {
    
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
    
    data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,input$XGB_standard)
    data = data1[,-1]
    
    xgb_optim_HP_results = xgb_pso(data,rv,feats_to_use,input$lc_val,input$lc_lowval,input$lc_upval,input$rc_val,input$rc_lowval,input$rc_upval,
                                   input$MC_runs,input$num_folds,input$loggy,input$XGB_hyper_metric,input$pso_max_iter,input$pso_swarm_size,input$member_exp,input$ss_exp,MC_subbin)
    
    xgb_optim_HP_results1 = data.frame(xgb_optim_HP_results)
    
    Optimal_HP$max_depth <<- round(xgb_optim_HP_results1[1,1],0)
    Optimal_HP$eta <<- round(xgb_optim_HP_results1[2,1],3)
    Optimal_HP$subsample <<- round(xgb_optim_HP_results1[3,1],2)
    Optimal_HP$colsample_bytree <<- round(xgb_optim_HP_results1[4,1],2)
    Optimal_HP$min_child_weight <<- round(xgb_optim_HP_results1[5,1],0)
    Optimal_HP$gamma <<- round(xgb_optim_HP_results1[6,1],1)
    Optimal_HP$nrounds <<- round(xgb_optim_HP_results1[7,1],0)
    
    refresh_XGB_Optim_HP(TRUE)
  })
  
  # XGB HP settings
  observeEvent(input$XGB_params, ignoreInit = T, {
    
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
  
  observeEvent(input$XGB_booster, ignoreInit = T, {
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
  debounced_XGB_pred_stand = debounce(reactive(input$XGB_pred_stand), plot_delay)
  debounced_XGB_pred_dc = debounce(reactive(input$XGB_pred_dc), plot_delay)
  
  observeEvent(debounced_XGB_pred_stand(), ignoreInit = T, {
    
    if (nrow(XGB_pred_scat_dat()) != 0) {
      
      iv$add_rule("XGB_pred_stand", sv_between(min(XGB_pred_scat_dat()[,2]),max(XGB_pred_scat_dat()[,2])))
      
      if (is.numeric(debounced_XGB_pred_stand())) {
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(debounced_XGB_pred_dc(), ignoreInit = T, {
    
    if (nrow(XGB_pred_scat_dat()) != 0) {
      
      iv$add_rule("XGB_pred_dc", sv_between(min(XGB_pred_scat_dat()[,3]),max(XGB_pred_scat_dat()[,3])))
      
      if (is.numeric(debounced_XGB_pred_dc())) {
        
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(input$run_XGB_predict, ignoreInit = T, {
    
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
    
    xgb_pred_results = xgb_call_predict(data,rv,id_var,input$model_seed,ignored_rows,feats_to_use,input$lc_val,input$rc_val,input$lc_lowval,
                                        input$lc_upval,input$rc_lowval,input$rc_upval,input$train_pct/100,input$MC_runs,input$num_folds,input$loggy,input$randomize,
                                        input$XGB_standard,Optimal_HP$eta,Optimal_HP$gamma,Optimal_HP$max_depth,
                                        Optimal_HP$min_child_weight,Optimal_HP$subsamp,Optimal_HP$colsamp,Optimal_HP$nrounds,MC_subbin,create_data)
    
    XGB_pred_results(xgb_pred_results[[1]])
    
    XGB_pred_scat_dat(XGB_pred_results()[,1:3])
    
    xgb_pred_coeffs=data.frame(xgb_pred_results[[2]])
    colnames(xgb_pred_coeffs) = c("Feature","SHAP Value")
    XGB_pred_coeffs(xgb_pred_coeffs)
    
    XGB_confuse_results(confuse(XGB_pred_scat_dat()[,2:3],input$XGB_pred_stand,input$XGB_pred_dc))
    
    removeModal()
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Predict')
    
    XGB_pred_standardize(input$XGB_standard)
    
    updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
    
  })
  
  observeEvent(c(XGB_pred_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(XGB_pred_results())) {
      
      xgb_pred_stepr = round((max(XGB_pred_results()[,2])-min(XGB_pred_results()[,2]))/100,2)
      
      updateNumericInput(session, "XGB_pred_stand",
                         # value = round(mean(XGB_pred_results()[,2]),2),
                         max = round(max(XGB_pred_results()[,2]),2),
                         min = round(min(XGB_pred_results()[,2]),2),
                         step = xgb_pred_stepr
      )
      
      xgb_pred_stepdc = round((max(XGB_pred_results()[,3])-min(XGB_pred_results()[,3]))/100,2)
      
      updateNumericInput(session, "XGB_pred_dc",
                         # value = round(mean(XGB_pred_results()[,3]),2),
                         max = round(max(XGB_pred_results()[,3]),2),
                         min = round(min(XGB_pred_results()[,3]),2),
                         step = xgb_pred_stepdc
      )
      
      results = XGB_pred_results()
      results[,2] = round(results[,2],3)
      results[,3] = round(results[,3],3)
      XGB_pred_results(results)
      
      output$XGB_predictions = DT::renderDataTable(server = T, {data = datatable(XGB_pred_results(),rownames = F,selection =
              list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions='Buttons',options = list(autoWidth = F,
              paging = TRUE,pageLength = num_rows_per_page,dom="ltBp",buttons = c('copy', 'csv', 'excel'),scrollX = TRUE,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',
              orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744',
              'color': '#fff'});","}")))#{if (date_format_string != "Non-Date") formatDate(data,1,date_format_string) else .}
      })
      
      output$XGB_pred_shapes = DT::renderDataTable(server = T, {data = datatable(XGB_pred_coeffs(),rownames = F,selection =
              list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
              list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
              className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGB_pred_scatplot = renderPlotly(scatter_confuse(XGB_pred_scat_dat(),debounced_XGB_pred_stand(),debounced_XGB_pred_dc()))
      
      XGB_pred_confuse_results(confuse(XGB_pred_scat_dat()[,2:3],debounced_XGB_pred_stand(),debounced_XGB_pred_dc()))
      
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = XGB_pred_confuse_results()$TP
      confuse_table[1,2] = XGB_pred_confuse_results()$TN
      confuse_table[1,3] = XGB_pred_confuse_results()$FP
      confuse_table[1,4] = XGB_pred_confuse_results()$FN
      
      precision = XGB_pred_confuse_results()$TP/(XGB_pred_confuse_results()$TP+XGB_pred_confuse_results()$FP)
      sensitivity = XGB_pred_confuse_results()$TP/(XGB_pred_confuse_results()$TP+XGB_pred_confuse_results()$FN)
      NPV = XGB_pred_confuse_results()$TN/(XGB_pred_confuse_results()$TN+XGB_pred_confuse_results()$FN)
      specificity = XGB_pred_confuse_results()$TN/(XGB_pred_confuse_results()$TN+XGB_pred_confuse_results()$FP)
      PF1_score = 2*(precision * sensitivity)/(precision + sensitivity)
      NF1_score = 2*(NPV * specificity)/(NPV + specificity)
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGB_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGB_pred_confuse_text = renderText({paste0("Sensitivity = ",round(XGB_pred_confuse_results()$Sensitivity,3),"; Specificity = ",
                      round(XGB_pred_confuse_results()$Specificity,3),"; Accuracy = ",round(XGB_pred_confuse_results()$Accuracy,3),
                      "; Positive F1 = ",round(PF1_score,3),
                      "; Negative F1 = ",round(NF1_score,3))})
      
      resid_data = XGB_pred_scat_dat()[,c(1,3)] %>% mutate(Residuals = round(XGB_pred_scat_dat()[,2]-XGB_pred_scat_dat()[,3],3))
      output$XGB_pred_resid_scatplot = renderPlotly(resid_scatter(resid_data))
      
      output$XGB_pred_lineplot = renderPlotly(plot_ly(XGB_pred_scat_dat(), x = ~XGB_pred_scat_dat()[,1], y = ~XGB_pred_scat_dat()[,2], name="Observations",
                            type="scatter", mode = "lines",text = ~paste("<b>ID: </b>",XGB_pred_scat_dat()[,1],"<br><b>Observed Value:</b> ",
                            XGB_pred_scat_dat()[,2],sep=""),hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5)) %>%
                      add_trace(y = ~XGB_pred_scat_dat()[,3], name="Predictions", mode = 'lines',
                            text = ~paste("<b>ID: </b>",XGB_pred_scat_dat()[,1],"<br><b>Predicted Value:</b> ",round(XGB_pred_scat_dat()[,3],3),sep=""),
                            hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                      layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Predictions",font=list(size=20)),
                            range=c(min(0.99*min(XGB_pred_scat_dat()[,2],XGB_pred_scat_dat()[,3]),1.01*min(XGB_pred_scat_dat()[,2],XGB_pred_scat_dat()[,3])),
                            max(0.99*max(XGB_pred_scat_dat()[,2],XGB_pred_scat_dat()[,3]),1.01*max(XGB_pred_scat_dat()[,2],XGB_pred_scat_dat()[,3]))))))
      
      output$XGB_used_hp_pred = DT::renderDataTable(server=T,{data = datatable(Optimal_HP,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                      target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                      pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                      initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      refresh_trigger(FALSE)
      
    } else if (is.null(XGB_pred_results())) {
      
      output$XGB_predictions = NULL
      output$XGB_pred_shapes = NULL
      output$XGB_pred_scatplot = NULL
      output$XGB_pred_confuse = NULL
      output$XGB_pred_confuse_text = NULL
      output$XGB_pred_resid_scatplot = NULL
      output$XGB_pred_lineplot = NULL
      output$XGB_used_hp_pred = NULL
      
      refresh_trigger(FALSE)
    }
  })
  
  # XGB fitting
  debounced_XGB_stand = debounce(reactive(input$XGB_stand), plot_delay)
  debounced_XGB_dec_crit = debounce(reactive(input$XGB_dec_crit), plot_delay)
  
  observeEvent(debounced_XGB_stand(), ignoreInit = T, {
    
    if (nrow(XGB_scat_dat()) != 0) {
      
      iv$add_rule("XGB_stand", sv_between(min(XGB_scat_dat()[,2]),max(XGB_scat_dat()[,2])))
      
      if (is.numeric(debounced_XGB_stand())) {
        
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(debounced_XGB_dec_crit(), ignoreInit = T, {
    
    if (nrow(XGB_scat_dat()) != 0) {
      
      iv$add_rule("XGB_dec_crit", sv_between(min(XGB_scat_dat()[,3]),max(XGB_scat_dat()[,3])))
      
      if (is.numeric(debounced_XGB_dec_crit())) {
        
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(input$XGB_final_fitting, ignoreInit = T, {
    
    req(iv$is_valid())
    
    updateNumericInput(session, "num_preds",value = 2)
    changed_model(TRUE)
    XGB_pred_data(NULL)
    
    if (input$use_pca_data) {
      data0 = PCA_dataset()
      rv=2
      XGB_final_features(PCA_coefficients()[,1])
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
      XGB_model_PCA(TRUE)
    } else {
      data0 = current_data()
      rv=response_var()
      XGB_final_features(input$feats_to_use)
      feats_to_use = input$feats_to_use
      XGB_model_PCA(FALSE)
    }
    
    data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,input$XGB_standard)
    data = data1[,-1]
    
    modeling_data = MC_final_subbin(data,input$loggy,input$lc_val,input$rc_val,0.5,1.5)
    
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
    
    xgb_model = xgboost(data = as.matrix(modeling_data[,-1]),label=modeling_data[,1], params=params, early_stopping_rounds=early_stop_set(), nrounds=1000, verbose=0)
    
    XGB_model <<- xgb_model
    
    fitted_values = predict(XGB_model, as.matrix(modeling_data[,-1]))
    lm_model = lm(fitted_values ~ modeling_data[,1])
    
    XGB_bias_slope(coef(lm_model)[2])
    XGB_bias_intercept(coef(lm_model)[1])
    
    fits = cbind(modeling_data[,1],round(fitted_values,3))
    
    xgb_results = data.frame(cbind(data1[,1],fits,round(data[,2:ncol(data)],4)))
    colnames(xgb_results)[1:3] = c(colnames(data0)[[1]],colnames(data0)[[rv]],"Fitted_Values")
    xgb_results = xgb_results[order(xgb_results[,1]),]
    
    XGB_results(xgb_results)
    
    shap_values = shap.values(xgb_model = xgb_model, X_train = as.matrix(modeling_data[,-1]))
    mean_shaps = shap_values$mean_shap_score
    shap_names = names(mean_shaps)
    shap_temp = data.frame(cbind(shap_names,mean_shaps))
    
    XGB_shapes = data.frame(shap_temp,row.names = NULL)
    XGB_shapes$mean_shaps = as.numeric(XGB_shapes$mean_shaps)
    XGB_shapes$mean_shaps = format(XGB_shapes$mean_shaps,scientific=FALSE,digits=3)
    XGB_shapes[order(XGB_shapes$mean_shaps,decreasing=TRUE),]
    colnames(XGB_shapes) = c("Feature","Mean SHAP")
    
    XGB_coeffs(XGB_shapes)
    
    XGB_scat_dat(XGB_results()[,1:3])
    
    XGB_confuse_results(confuse(XGB_scat_dat()[,2:3],input$XGB_stand,input$XGB_dec_crit))
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Fitting')
    
    XGB_standardize(input$XGB_standard)
    
    updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
    
  })
  
  observeEvent(c(XGB_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(XGB_results())) {
      
      xgb_stepr = round((max(XGB_results()[,2])-min(XGB_results()[,2]))/100,2)
      
      updateNumericInput(session, "XGB_stand",
                         # value = round(mean(XGB_results()[,2]),2),
                         max = round(max(XGB_results()[,2]),2),
                         min = round(min(XGB_results()[,2]),2),
                         step = xgb_stepr
      )
      
      xgb_stepdc = round((max(XGB_results()[,3])-min(XGB_results()[,3]))/100,2)
      
      updateNumericInput(session, "XGB_dec_crit",
                         # value = round(mean(XGB_results()[,3]),2),
                         max = round(max(XGB_results()[,3]),2),
                         min = round(min(XGB_results()[,3]),2),
                         step = xgb_stepdc
      )
      
      output$XGB_shapes = DT::renderDataTable(server = T, {data = datatable(XGB_coeffs(),rownames = F,selection =
                    list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                    list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                    className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      results = XGB_results()
      results[,2] = round(results[,2],3)
      XGB_results(results)
      
      output$XGB_fits = DT::renderDataTable(server = T, {data = datatable(XGB_results(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                    target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp", buttons = c('copy', 'csv', 'excel'),
                    paging = T,pageLength = num_rows_per_page,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                    JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGB_scatplot = renderPlotly(scatter_confuse(XGB_scat_dat(),debounced_XGB_stand(),debounced_XGB_dec_crit()))
      
      xgb_resid_data = XGB_scat_dat()[,c(1,3)] %>% mutate(Residuals = round(XGB_scat_dat()[,2]-XGB_scat_dat()[,3],3))
      output$XGB_resid_scatplot = renderPlotly(resid_scatter(xgb_resid_data))
      
      output$XGB_lineplot = renderPlotly(plot_ly(XGB_scat_dat(), x = ~XGB_scat_dat()[,1], y = ~XGB_scat_dat()[,2], name="Observations", type="scatter", mode = "lines",
                        text = ~paste("<b>ID: </b>",XGB_scat_dat()[,1],"<br><b>Observed Value:</b> ",XGB_scat_dat()[,2],sep=""),hoveron = 'points',hoverinfo='text',
                        line = list(color = "#2c3e50", width = 1.5)) %>%
                  add_trace(y = ~XGB_scat_dat()[,3], name="Fitted_Values", mode = 'lines',text = ~paste("<b>ID: </b>",XGB_scat_dat()[,1],"<br><b>Fitted Value:</b> ",
                        round(XGB_scat_dat()[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                  layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Fitted Values",font=list(size=20)),
                        range=c(min(0.99*min(XGB_scat_dat()[,2],XGB_scat_dat()[,3]),1.01*min(XGB_scat_dat()[,2],XGB_scat_dat()[,3])),max(0.99*max(XGB_scat_dat()[,2],
                        XGB_scat_dat()[,3]),1.01*max(XGB_scat_dat()[,2],XGB_scat_dat()[,3]))))))
      
      XGB_confuse_results(confuse(XGB_scat_dat()[,2:3],debounced_XGB_stand(),debounced_XGB_dec_crit()))
      
      xgb_confuse_table = matrix(0,nrow=1,ncol=4)
      
      xgb_confuse_table[1,1] = XGB_confuse_results()$TP
      xgb_confuse_table[1,2] = XGB_confuse_results()$TN
      xgb_confuse_table[1,3] = XGB_confuse_results()$FP
      xgb_confuse_table[1,4] = XGB_confuse_results()$FN
      
      precision = XGB_confuse_results()$TP/(XGB_confuse_results()$TP+XGB_confuse_results()$FP)
      sensitivity = XGB_confuse_results()$TP/(XGB_confuse_results()$TP+XGB_confuse_results()$FN)
      NPV = XGB_confuse_results()$TN/(XGB_confuse_results()$TN+XGB_confuse_results()$FN)
      specificity = XGB_confuse_results()$TN/(XGB_confuse_results()$TN+XGB_confuse_results()$FP)
      PF1_score = 2*(precision * sensitivity)/(precision + sensitivity)
      NF1_score = 2*(NPV * specificity)/(NPV + specificity)
      
      colnames(xgb_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGB_confuse = DT::renderDataTable(server = T, {data = datatable(xgb_confuse_table,rownames = F,selection = 
                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                    options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                    columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                    {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGB_confuse_text = renderText({paste0("Sensitivity = ",round(XGB_confuse_results()$Sensitivity,3),"; Specificity = ",
                    round(XGB_confuse_results()$Specificity,3),"; Accuracy = ",round(XGB_confuse_results()$Accuracy,3),
                    "; Positive F1 = ",round(PF1_score,3),
                    "; Negative F1 = ",round(NF1_score,3))})
      
      output$XGB_used_hp = DT::renderDataTable(server=T,{data = datatable(Optimal_HP,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                    target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                    pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                    initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      refresh_trigger(FALSE)
      
    } else if (is.null(XGB_results())) {
      
      output$XGB_shapes = NULL
      output$XGB_fits = NULL
      output$XGB_scatplot = NULL
      output$XGB_confuse = NULL
      output$XGB_confuse_text = NULL
      output$XGB_resid_scatplot = NULL
      output$XGB_lineplot = NULL
      output$XGB_used_hp = NULL
      refresh_trigger(FALSE)
    }
  })
  
  # Create PDP for a feature in the XGB model
  observeEvent(input$XGB_shapes_rows_selected, ignoreInit = T, {
    
    selected_index = input$XGB_shapes_rows_selected[1]
    selected_feature = XGB_coeffs()[selected_index,1]
    
    used_data = na.omit(XGB_final_data())
    
    train_data = as.data.frame(used_data[,-1])
    response = as.numeric(used_data[,1])
    
    predcl_fun = function(model, newdata) {
      predictions = predict(model, newdata = as.matrix(newdata))
      return(as.numeric(predictions))
    }
    
    predictor.xgb = Predictor$new(
      model = XGB_model,
      data = train_data,
      y = response,
      predict.fun = predcl_fun
    )
    
    pdp_info =  FeatureEffect$new(
      predictor.xgb,
      selected_feature,
      method = "pdp",
      grid.size = 25
    )
    
    # print(pdp_info$plot())
    
    output$XGB_pdp_plot = renderPlot({plot(pdp_info$plot())})
    
    showModal(modalDialog(
      title = paste("PDP for", selected_feature),
      size = "l",
      plotOutput("XGB_pdp_plot"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Elastic Net predictions
  debounced_EN_pred_stand = debounce(reactive(input$EN_pred_stand), plot_delay)
  debounced_EN_pred_dc = debounce(reactive(input$EN_pred_dc), plot_delay)
  
  observeEvent(debounced_EN_pred_stand(), ignoreInit = TRUE, {
    
    if (nrow(EN_pred_scat_dat()) != 0) {
      
      iv$add_rule("EN_pred_stand", sv_between(min(EN_pred_scat_dat()[,2]),max(EN_pred_scat_dat()[,2])))
      
      if (is.numeric(debounced_EN_pred_stand())) {
        
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(debounced_EN_pred_dc(), ignoreInit = TRUE,{
    
    if (nrow(EN_pred_scat_dat()) != 0) {
      
      iv$add_rule("EN_pred_dc", sv_between(min(EN_pred_scat_dat()[,3]),max(EN_pred_scat_dat()[,3])))
      
      if (is.numeric(debounced_EN_pred_dc())) {
        
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(input$EN_pred, ignoreInit = TRUE, {
    
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
    
    data = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,FALSE)
    
    MC_runs = input$MC_runs
    
    #Create n folds
    tot_folds = input$num_folds
    folds = cut(seq(1, nrow(data)), breaks = tot_folds, labels = FALSE)
    
    fold_predictions = matrix(0, nrow = 0, ncol = 2)
    fold_predictions = as.data.frame(fold_predictions)
    
    coeff_folds = matrix(0, nrow = ncol(data)-1, ncol = tot_folds+1)
    coeff_folds = as.data.frame(coeff_folds)
    coeff_folds[,1] = c("(Intercept)",feats_to_use)
    
    #Perform cross validation
    for (f in 1:tot_folds) {
      
      testIndices = which(folds == f, arr.ind = TRUE)
      testData0 = data[testIndices, ]
      trainData0 = data[-testIndices, ]
      
      testData = testData0[,-1]
      trainData = trainData0[,-1]
      
      # Create imputed training/testing data
      train_X = trainData[,-1]
      test_X = testData[,-1]
      
      imp_train_X=missForest(train_X)$ximp
      
      train_test_X = rbind(test_X, imp_train_X)
      imp_test_X = missForest(train_test_X)$ximp[1:nrow(test_X), ]
      
      train_data = cbind(trainData[,1], imp_train_X)
      test_data = cbind(testData[,1], imp_test_X)
      
      colnames(train_data) = colnames(trainData)
      colnames(test_data) = colnames(testData)
      
      temp_preds = matrix(0, nrow = nrow(test_data), ncol = 2*MC_runs)
      temp_preds = data.frame(temp_preds)
      
      temp_coeffs = matrix(0, nrow = ncol(trainData), ncol = MC_runs+1)
      temp_coeffs = data.frame(temp_coeffs)
      temp_coeffs[,1] = c("(Intercept)",feats_to_use)
      
      withProgress(
        message = 'EN Prediction Progress',
        detail = paste("MC runs:", x = 1,"/",MC_runs,"; Fold:",y = 1,"/", tot_folds),
        value = (f/tot_folds - 1/tot_folds),
        {
          
          for (i in 1:MC_runs) {
            
            trainingData = MC_subbin(train_data,input$loggy,input$lc_val,input$lc_lowval,input$lc_upval,input$rc_val,input$rc_lowval,input$rc_upval)
            testingData = MC_subbin(test_data,input$loggy,input$lc_val,input$lc_lowval,input$lc_upval,input$rc_val,input$rc_lowval,input$rc_upval)
            
            temp_preds[,2*i-1] = testingData[,1]
            
            # determine best alpha and lambda
            fit_mod = cva.glmnet(x=as.matrix(trainingData[,-1]),y=trainingData[,1],nfolds=input$num_folds,na.action="na.omit",
                                 standardize=input$EN_standard,intercept=TRUE)
            
            alpha = get_model_params(fit_mod)$alpha
            lambda = get_model_params(fit_mod)$lambdaMin
            
            # fit final model
            model = glmnet(x=as.matrix(trainingData[,-1]),trainingData[,1],lambda=lambda, alpha=alpha, na.action="na.omit",
                           standardize=input$EN_standard,intercept=TRUE)
            
            coeffs = as.matrix(coef(model, s=lambda))
            coeffs = as.data.frame(coeffs)
            temp_coeffs[,i+1] = coeffs
            
            preds = predict(model, newx = as.matrix(testingData[,-1]))
            
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
    
    prediction_results = data.frame(cbind(data[,1],round(fold_predictions[,1],3),round(fold_predictions[,2],3),round(data[,3:ncol(data)],4)))
    colnames(prediction_results) = c(colnames(data0)[1],colnames(data0)[rv],"Predictions",colnames(trainData[,-1]))
    
    EN_pred_results(prediction_results[order(prediction_results[,1]),])
    
    final_coeffs = data.frame(cbind(coeff_folds[,1],format(round(rowMeans(coeff_folds[,-1]),4),scientific=FALSE)))
    colnames(final_coeffs) = c("Feature","Coefficient")
    
    EN_pred_coeffs(final_coeffs)
    
    EN_pred_scat_dat(EN_pred_results()[,1:3])
    
    EN_pred_confuse_results(confuse(EN_pred_scat_dat()[,2:3],input$EN_pred_stand,input$EN_pred_dc))
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'EN: Predict')
    
    EN_pred_standardize(input$EN_standard)
    
    updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
    
  })
  
  observeEvent(c(EN_pred_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(EN_pred_results())) {
      
      en_stepr = round((max(EN_pred_results()[,2])-min(EN_pred_results()[,2]))/100,2)
      
      updateNumericInput(session, "EN_pred_stand",
                         # value = round(mean(EN_pred_results()[,2]),2),
                         max = round(max(EN_pred_results()[,2]),2),
                         min = round(min(EN_pred_results()[,2]),2),
                         step = en_stepr
      )
      
      en_stepdc = round((max(EN_pred_results()[,3])-min(EN_pred_results()[,3]))/100,2)
      
      updateNumericInput(session, "EN_pred_dc",
                         # value = round(mean(EN_pred_results()[,3]),2),
                         max = round(max(EN_pred_results()[,3]),2),
                         min = round(min(EN_pred_results()[,3]),2),
                         step = en_stepdc
      )
      
      output$EN_preds = DT::renderDataTable(server = T, {data = datatable(EN_pred_results(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp",buttons = c('copy', 'csv', 'excel'),paging = T,
                  pageLength = num_rows_per_page,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_coeffs = DT::renderDataTable(server = T, {data = datatable(EN_pred_coeffs(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),
                  paging = F,scrollX = F,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                  initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_scatplot = renderPlotly(scatter_confuse(EN_pred_scat_dat(),debounced_EN_pred_stand(),debounced_EN_pred_dc()))
      
      resid_data = EN_pred_results()[,c(1,3)]
      resid_data = resid_data %>% mutate(Residuals = round(EN_pred_results()[,2]-EN_pred_results()[,3],3))
      output$EN_pred_resid_scatter = renderPlotly(resid_scatter(resid_data))
      
      output$EN_pred_lineplot = renderPlotly(plot_ly(EN_pred_scat_dat(), x = ~EN_pred_scat_dat()[,1], y = ~EN_pred_scat_dat()[,2], name="Observations", type="scatter",
                        mode = "lines",text = ~paste("<b>ID: </b>",EN_pred_scat_dat()[,1],"<br><b>Observed Value:</b> ",EN_pred_scat_dat()[,2],sep=""),
                        hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5)) %>%
                  add_trace(y = ~EN_pred_scat_dat()[,3], name="Predictions", mode = 'lines',text = ~paste("<b>ID: </b>",EN_pred_scat_dat()[,1],"<br><b>Prediction:</b> ",
                        round(EN_pred_scat_dat()[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                  layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Predictions",
                        font=list(size=20)),range=c(min(0.99*min(EN_pred_scat_dat()[,2],EN_pred_scat_dat()[,3]),1.01*min(EN_pred_scat_dat()[,2],EN_pred_scat_dat()[,3])),
                        max(0.99*max(EN_pred_scat_dat()[,2],EN_pred_scat_dat()[,3]),1.01*max(EN_pred_scat_dat()[,2],EN_pred_scat_dat()[,3]))))))
      
      EN_pred_confuse_results(confuse(EN_pred_scat_dat()[,2:3],debounced_EN_pred_stand(),debounced_EN_pred_dc()))
      
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = EN_pred_confuse_results()$TP
      confuse_table[1,2] = EN_pred_confuse_results()$TN
      confuse_table[1,3] = EN_pred_confuse_results()$FP
      confuse_table[1,4] = EN_pred_confuse_results()$FN
      
      precision = EN_pred_confuse_results()$TP/(EN_pred_confuse_results()$TP+EN_pred_confuse_results()$FP)
      sensitivity = EN_pred_confuse_results()$TP/(EN_pred_confuse_results()$TP+EN_pred_confuse_results()$FN)
      NPV = EN_pred_confuse_results()$TN/(EN_pred_confuse_results()$TN+EN_pred_confuse_results()$FN)
      specificity = EN_pred_confuse_results()$TN/(EN_pred_confuse_results()$TN+EN_pred_confuse_results()$FP)
      PF1_score = 2*(precision * sensitivity)/(precision + sensitivity)
      NF1_score = 2*(NPV * specificity)/(NPV + specificity)
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_pred_confuse_text = renderText({paste0("Sensitivity = ",round(EN_pred_confuse_results()$Sensitivity,3),"; Specificity = ",
                      round(EN_pred_confuse_results()$Specificity,3),"; Accuracy = ",round(EN_pred_confuse_results()$Accuracy,3),
                      "; Positive F1 = ",round(PF1_score,3),
                      "; Negative F1 = ",round(NF1_score,3))})
      
      refresh_trigger(FALSE) 
      
    } else if (is.null(EN_pred_results())) {
      
      output$EN_preds = NULL
      output$EN_pred_coeffs = NULL
      output$EN_pred_scatplot = NULL
      output$EN_pred_confuse = NULL
      output$EN_pred_confuse_text = NULL
      output$EN_pred_resid_scatter = NULL
      output$EN_pred_lineplot = NULL
      
      refresh_trigger(FALSE) 
    }
  })
  
  # Elastic Net fitting
  debounced_EN_stand = debounce(reactive(input$EN_stand), plot_delay)
  debounced_EN_dec_crit = debounce(reactive(input$EN_dec_crit), plot_delay)
  
  observeEvent(debounced_EN_stand(), ignoreInit = TRUE, {
    
    if (nrow(EN_scat_dat()) != 0) {
      
      iv$add_rule("EN_stand", sv_between(min(EN_scat_dat()[,2]),max(EN_scat_dat()[,2])))
      
      if (is.numeric(debounced_EN_stand())) {
        
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(debounced_EN_dec_crit(), ignoreInit = TRUE, {
    
    if (nrow(EN_scat_dat()) != 0) {
      
      iv$add_rule("EN_dec_crit", sv_between(min(EN_scat_dat()[,3]),max(EN_scat_dat()[,3])))
      
      if (is.numeric(debounced_EN_dec_crit())) {
        
        refresh_trigger(TRUE)
      }
    }
  })
  
  observeEvent(input$EN_fit, ignoreInit = TRUE, {
    
    req(iv$is_valid())
    
    EN_pred_data(NULL)
    
    updateNumericInput(session, "num_preds",value = 2)
    changed_model(TRUE)
    
    if (input$use_pca_data) {
      data0 = PCA_dataset()
      rv=2
      EN_final_features(PCA_coefficients()[,1])
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
      EN_model_PCA(TRUE)
    } else {
      data0 = current_data()
      rv=response_var()
      EN_final_features(input$feats_to_use)
      feats_to_use = input$feats_to_use
      EN_model_PCA(FALSE)
    }
    
    set.seed(input$model_seed)
    
    data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,FALSE)
    data = data1[,-1]
    
    imp_X=missForest(data[,-1])$ximp
    trainData = cbind(data[,1],imp_X)
    colnames(trainData) = colnames(data)
    
    temp_coeffs = matrix(0, nrow = length(feats_to_use)+1, ncol = 2)
    temp_coeffs = as.data.frame(temp_coeffs)
    temp_coeffs[,1] = c("(Intercept)",feats_to_use)
    
    modeling_data = MC_final_subbin(trainData,input$loggy,input$lc_val,input$rc_val,0.5,1.5)
    
    # determine best alpha and lambda
    fit_mod = cva.glmnet(x=as.matrix(modeling_data[,-1]),y=modeling_data[,1],nfolds=input$num_folds,na.action="na.omit",
                         standardize=input$EN_standard,intercept=TRUE)
    
    alpha = get_model_params(fit_mod)$alpha
    lambda = get_model_params(fit_mod)$lambdaMin
    
    en_model = glmnet(x=as.matrix(modeling_data[,-1]),modeling_data[,1],lambda=lambda, alpha=alpha, na.action="na.omit",
                      standardize=input$EN_standard,intercept=TRUE)
    
    EN_model <<- en_model
    
    coeffs = as.matrix(coef(en_model,s=lambda))
    coeffs = data.frame(coeffs, row.names=NULL)
    temp_coeffs[,2] = round(coeffs[,1],4)
    colnames(temp_coeffs) = c("Feature","Coefficient")
    EN_coeffs(temp_coeffs)
    
    fits = predict(en_model, newx = as.matrix(modeling_data[,-1]))
    
    lm_model = lm(fits ~ modeling_data[,1])
    
    EN_bias_slope(coef(lm_model)[2])
    EN_bias_intercept(coef(lm_model)[1])
    
    en_results = data.frame(cbind(data1[,1],round(modeling_data[,1],3),round(fits,3),round(data[,-1],4)))
    colnames(en_results) = c(colnames(data0)[1],colnames(data0)[rv],"Fitted_Value",colnames(data[,-1]))
    
    en_results = en_results[order(en_results[,1]),]
    
    EN_results(en_results)
    EN_scat_dat(EN_results()[,1:3])
    
    EN_confuse_results(confuse(EN_scat_dat()[,2:3],input$EN_stand,input$EN_dec_crit))
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'EN: Fitting')
    
    EN_standardize(input$EN_standard)
    
    updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
  })
  
  observeEvent(c(EN_results(), refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(EN_results())) {
      
      en_stepr = round((max(EN_results()[,2])-min(EN_results()[,2]))/100,2)
      
      updateNumericInput(session, "EN_stand",
                         # value = round(mean(EN_results()[,2]),2),
                         max = round(max(EN_results()[,2]),2),
                         min = round(min(EN_results()[,2]),2),
                         step = en_stepr)
      
      en_stepdc = round((max(EN_results()[,3])-min(EN_results()[,3]))/100,2)
      
      updateNumericInput(session, "EN_dec_crit",
                         # value = round(mean(EN_results()[,3]),2),
                         max = round(max(EN_results()[,3]),2),
                         min = round(min(EN_results()[,3]),2),
                         step = en_stepdc)
      
      output$EN_fits = DT::renderDataTable(server = T, {data = datatable(EN_results(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F,dom="ltBp", buttons = c('copy', 'csv', 'excel'),
                  paging = T,pageLength = num_rows_per_page,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_coeffs = DT::renderDataTable(server = T, {data = datatable(EN_coeffs(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons",options = list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),
                  paging = F,scrollX = F,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_scatplot = renderPlotly(scatter_confuse(EN_scat_dat(),debounced_EN_stand(),debounced_EN_dec_crit()))
      
      resid_data = EN_results()[,c(1,3)]
      resid_data = resid_data %>% mutate(Residuals = round(EN_results()[,2]-EN_results()[,3],3))
      output$EN_resid_scatplot = renderPlotly(resid_scatter(resid_data))
      
      output$EN_lineplot = renderPlotly(plot_ly(EN_scat_dat(), x = ~EN_scat_dat()[,1], y = ~EN_scat_dat()[,2], name="Observations", type="scatter",
                        mode = "lines",text = ~paste("<b>ID: </b>",EN_scat_dat()[,1],"<br><b>Observed Value:</b> ",EN_scat_dat()[,2],sep=""),
                        hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5)) %>%
                  add_trace(y = ~EN_scat_dat()[,3], name="Fitted_Value", mode = 'lines',text = ~paste("<b>ID: </b>",EN_scat_dat()[,1],"<br><b>Fitted_Value:</b> ",
                        round(EN_scat_dat()[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                  layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Fitted_Values",
                        font=list(size=20)),range=c(min(0.99*min(EN_scat_dat()[,2],EN_scat_dat()[,3]),1.01*min(EN_scat_dat()[,2],EN_scat_dat()[,3])),
                        max(0.99*max(EN_scat_dat()[,2],EN_scat_dat()[,3]),1.01*max(EN_scat_dat()[,2],EN_scat_dat()[,3]))))))
      
      EN_confuse_results(confuse(EN_scat_dat()[,2:3],debounced_EN_stand(),debounced_EN_dec_crit()))
      
      EN_confuse_table = matrix(0,nrow=1,ncol=4)
      
      EN_confuse_table[1,1] = EN_confuse_results()$TP
      EN_confuse_table[1,2] = EN_confuse_results()$TN
      EN_confuse_table[1,3] = EN_confuse_results()$FP
      EN_confuse_table[1,4] = EN_confuse_results()$FN
      
      precision = EN_confuse_results()$TP/(EN_confuse_results()$TP+EN_confuse_results()$FP)
      sensitivity = EN_confuse_results()$TP/(EN_confuse_results()$TP+EN_confuse_results()$FN)
      NPV = EN_confuse_results()$TN/(EN_confuse_results()$TN+EN_confuse_results()$FN)
      specificity = EN_confuse_results()$TN/(EN_confuse_results()$TN+EN_confuse_results()$FP)
      PF1_score = 2*(precision * sensitivity)/(precision + sensitivity)
      NF1_score = 2*(NPV * specificity)/(NPV + specificity)
      
      colnames(EN_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$EN_confuse = DT::renderDataTable(server = T, {data = datatable(EN_confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$EN_confuse_text = renderText({paste0("Sensitivity = ",round(EN_confuse_results()$Sensitivity,3),"; Specificity = ",
                  round(EN_confuse_results()$Specificity,3),"; Accuracy = ",round(EN_confuse_results()$Accuracy,3),
                  "; Positive F1 = ",round(PF1_score,3),
                  "; Negative F1 = ",round(NF1_score,3))})
      
      refresh_trigger(FALSE)
      
    } else if (is.null(EN_results())) {
      
      output$EN_fits = NULL
      output$EN_coeffs = NULL
      output$EN_scatplot = NULL
      output$EN_confuse = NULL
      output$EN_confuse_text = NULL
      output$EN_resid_scatplot = NULL
      output$EN_lineplot = NULL
      
      refresh_trigger(FALSE)
    }
  })
  
  # Perform book-keeping functions when the "Prediction" tab is selected
  observeEvent(input$shinyVB, ignoreInit = T, {
    
    if (input$shinyVB == "Prediction") {
      
      if (is.null(current_data())) {
        return()
        
      } else {
        
        names = c("Logistic_Regression","XGB_Classifier","XGBoost","Elastic_Net")
        created = list(LG_results(),XGBCL_results(),XGB_results(),EN_results())
        available = c()
        
        for (i in 1:4) {
          if (!is.null(created[[i]])) {
            available = c(available,names[i])
          } else {
            available = available
          }
        }
        
        models_created(available)
        updateRadioButtons(session,"model_choice",selected=input$model_choice,choices=c("None",models_created()))
      }
      
      if (final_model_PCA()) {
        output$pca_model_text = renderText({HTML("NOTE: PCA axes being used as features.")})
      } else {
        output$pca_model_text = NULL
      }
    }
  })
  
  # Save Prediction file from Prediction Tab
  output$save_prediction = downloadHandler(filename = function() {paste("Prediction_File.RData")}, content = function(file) {
    
    save_list = list(
      type = "Prediction",
      Version = version,
      temp_db = temp_db,
      bo = bo(),
      current_data = current_data(),
      response_var = response_var(),
      col_names = col_names(),
      feat_names = feat_names(),
      feats_being_used = feats_being_used(),
      fs_feats_used = fs_feats_used(),
      init_data = init_data,
      ignored_rows = ignored_rows,
      init_ID_format = init_ID_format,
      date_format_string = date_format_string,
      saved_lc_val = input$lc_val,
      saved_rc_val = input$rc_val,
      saved_num_axes = input$num_axes,
      init_column_props = init_column_props,
      column_props = column_props,
      PCA_scaling_mean = PCA_scaling_mean(),
      PCA_scaling_sd   = PCA_scaling_sd(),
      PCA_dataset = PCA_dataset(),
      PCA_summary_df = PCA_summary_df(),
      PCA_coefficients = PCA_coefficients(),
      pca_axes_max = pca_axes_max(),
      pca_axes = pca_axes(),
      pcax_being_used = pcax_being_used(),
      fs_pcax_used = fs_pcax_used(),
      final_model_PCA = final_model_PCA(),
      rv_ao_map = shiny::reactiveValuesToList(rv_ao_map, all.names = TRUE),
      rv_pred = { p <- shiny::reactiveValuesToList(rv_pred, all.names = TRUE); p$pending <- NULL; p },
      poly_coeffs = as.list(POLY_COEFFS),
      
      LG_pred_results = LG_pred_results(),
      LG_pred_coeffs = LG_pred_coeffs(),
      LG_pred_confuse_results = LG_pred_confuse_results(),
      LG_pred_scat_dat = LG_pred_scat_dat(),
      LG_pred_standardize = LG_pred_standardize(),
      LG_pred_thresh = LG_pred_thresh(),
      LG_results = LG_results(),
      LG_coeffs = LG_coeffs(),
      LG_confuse_results = LG_confuse_results(),
      LG_scat_dat = LG_scat_dat(),
      LG_model = LG_model,
      LG_thresh = LG_thresh(),
      LG_crit_prob = LG_crit_prob(),
      LG_standardize = LG_pred_standardize(),
      LG_model_PCA = LG_model_PCA(),
      LG_final_features = LG_final_features(),
      LG_pred_data = LG_pred_data(),
      
      XGBCL_pred_results = XGBCL_pred_results(),
      XGBCL_pred_coeffs = XGBCL_pred_coeffs(),
      XGBCL_pred_confuse_results = XGBCL_pred_confuse_results(),
      XGBCL_pred_scat_dat = XGBCL_pred_scat_dat(),
      XGBCL_pred_standardize = XGBCL_pred_standardize(),
      XGBCL_pred_thresh = XGBCL_pred_thresh(),
      XGBCL_selection_results = XGBCL_selection_results(),
      XGBCL_results = XGBCL_results(),
      XGBCL_coeffs = XGBCL_coeffs(),
      XGBCL_confuse_results = XGBCL_confuse_results(),
      XGBCL_scat_dat = XGBCL_scat_dat(),
      XGBCL_model = XGBCL_model,
      XGBCL_thresh = XGBCL_thresh(),
      XGBCL_crit_prob = XGBCL_crit_prob(),
      XGBCL_standardize = XGBCL_standardize(),
      XGBCL_model_PCA = XGBCL_model_PCA(),
      XGBCL_final_features = XGBCL_final_features(),
      XGBCL_pred_data = XGBCL_pred_data(),
      XGBCL_final_data = XGBCL_final_data(),
      Optimal_CLHP = Optimal_CLHP,
      
      XGB_pred_results = XGB_pred_results(),
      XGB_pred_coeffs = XGB_pred_coeffs(),
      XGB_pred_confuse_results = XGB_pred_confuse_results(),
      XGB_pred_scat_dat = XGB_pred_scat_dat(),
      XGB_pred_standardize = XGB_pred_standardize(),
      XGB_selection_results = XGB_selection_results(),
      XGB_results = XGB_results(),
      XGB_coeffs = XGB_coeffs(),
      XGB_confuse_results = XGB_confuse_results(),
      XGB_stand = input$XGB_stand,
      XGB_dec_crit = input$XGB_dec_crit,
      XGB_scat_dat = XGB_scat_dat(),
      XGB_model = XGB_model,
      XGB_standardize = XGB_standardize(),
      XGB_model_PCA = XGB_model_PCA(),
      XGB_final_features = XGB_final_features(),
      XGB_pred_data = XGB_pred_data(),
      XGB_final_data = XGB_final_data(),
      Optimal_HP = Optimal_HP,
      
      EN_pred_results = EN_pred_results(),
      EN_pred_coeffs = EN_pred_coeffs(),
      EN_pred_confuse_results = EN_pred_confuse_results(),
      EN_pred_scat_dat = EN_pred_scat_dat(),
      EN_pred_standardize = EN_pred_standardize(),
      EN_results = EN_results(),
      EN_coeffs = EN_coeffs(),
      EN_confuse_results = EN_confuse_results(),
      EN_stand = input$EN_stand,
      EN_dec_crit = input$EN_dec_crit,
      EN_scat_dat = EN_scat_dat(),
      EN_model = EN_model,
      EN_standardize = EN_standardize(),
      EN_model_PCA = EN_model_PCA(),
      EN_final_features = EN_final_features(),
      EN_pred_data = EN_pred_data()
    )
    
    save(save_list, file = file)
  })
  
  # Keep track of the current page in the prediction data table
  observeEvent(input$pd_data_state, {
    x = input$pd_data_state$start
    y = input$pd_data_state$length
    current_page = 1 + x/y
    
    if (current_page != current_pred_page()) {
      current_pred_page(current_page)
      renderpreddata(pred_data(),column_props,current_pred_page(),init_ID_format,output)
    }
  })
  
  # Upload data file into prediction data table
  observeEvent(input$pred_file, ignoreInit = TRUE, {
    
    if (input$model_choice == "None") {
      showModal(modalDialog("No model has been chosen.", easyClose = FALSE,
                            footer = div(modalButton('Close'))))
      return()
    }
    
    ext <- tools::file_ext(input$pred_file$name)
    if (ext == "xlsx") {
      pred_file_data <<- read.xlsx(input$pred_file$datapath)
    } else {
      pred_file_data <<- read.csv(input$pred_file$datapath, header = TRUE, sep = input$sep)
    }
    
    # Basic validations
    if (any(sapply(data.frame(pred_file_data[, -c(1:2), drop = FALSE]),
                   function(col) !is.numeric(col)))) {
      showModal(modalDialog(
        "Features values in this dataset are either non-numeric or missing. Please remedy prior to data importation.",
        easyClose = FALSE, footer = div(modalButton('Close'))
      ))
      return()
    }
    if (nrow(pred_file_data) < 2) {
      showModal(modalDialog("File must contain more than 1 data row.",
                            easyClose = FALSE, footer = div(modalButton('Close'))))
      return()
    }
    
    # Determine model features
    if (input$model_choice == "Logistic_Regression") {
      columns_to_grab <- LG_final_features()
    } else if (input$model_choice == "XGB_Classifier") {
      columns_to_grab <- XGBCL_final_features()
    } else if (input$model_choice == "XGBoost") {
      columns_to_grab <- XGB_final_features()
    } else if (input$model_choice == "Elastic_Net") {
      columns_to_grab <- EN_final_features()
    }
    
    # Ensure feature names are unique characters
    columns_to_grab <- unique(as.character(columns_to_grab))
    
    # Identify interaction terms up front
    inter_terms <- columns_to_grab[grepl(INTER_PATTERN, columns_to_grab, perl = TRUE)]
    
    # Helpers for interactions
    get_inter_components <- function(term) {
      # Strip "Int.." prefix, then split by "__" to get component names
      s <- sub(INTER_PATTERN, "", term, perl = TRUE)
      strsplit(s, INTER_SEP, fixed = TRUE)[[1]]
    }
    get_base <- function(x) {
      if (grepl(TRANS_PATTERN, x, perl = TRUE)) {
        if (exists("base_name", mode = "function")) base_name(x) else sub(TRANS_PATTERN, "", x, perl = TRUE)
      } else x
    }
    
    # Detect A/O needs, including transformed A/O and interactions involving A/O
    ao_all <- c("WindA","WindO","CurrentA","CurrentO","WaveA","WaveO")
    need_wind    <- any(c("WindA","WindO") %in% columns_to_grab) ||
      any(grepl(paste0(TRANS_PATTERN, "(WindA|WindO)$"), columns_to_grab, perl = TRUE))
    need_current <- any(c("CurrentA","CurrentO") %in% columns_to_grab) ||
      any(grepl(paste0(TRANS_PATTERN, "(CurrentA|CurrentO)$"), columns_to_grab, perl = TRUE))
    need_wave    <- any(c("WaveA","WaveO") %in% columns_to_grab) ||
      any(grepl(paste0(TRANS_PATTERN, "(WaveA|WaveO)$"), columns_to_grab, perl = TRUE))
    
    # Include interaction terms in A/O need detection
    inter_terms <- columns_to_grab[grepl(INTER_PATTERN, columns_to_grab, perl = TRUE)]
    if (length(inter_terms)) {
      comps <- lapply(inter_terms, get_inter_components)
      bases <- unlist(lapply(comps, function(pair) vapply(pair, get_base, FUN.VALUE = character(1))), use.names = FALSE)
      need_wind    <- need_wind    || any(bases %in% c("WindA","WindO"))
      need_current <- need_current || any(bases %in% c("CurrentA","CurrentO"))
      need_wave    <- need_wave    || any(bases %in% c("WaveA","WaveO"))
    }
    
    rv_pred$pending <- list(
      df     = pred_file_data,
      cols   = columns_to_grab,
      trans  = columns_to_grab[grepl(TRANS_PATTERN, columns_to_grab, perl = TRUE)],
      inter  = inter_terms,
      needs  = list(wind = need_wind, current = need_current, wave = need_wave),
      model  = input$model_choice
    )
    
    if (need_wind || need_current || need_wave) {
      # Ask user to map raw columns for the required group(s)
      showModal(modalDialog(
        title = "Assign columns for A/O components",
        easyClose = FALSE,
        size = "m",
        tagList(
          tags$style(HTML("
          .ao-map .form-group { width: 75%; }
          .ao-map .selectize-control { width: 75% !important; max-width: none !important; }
          .ao-map .selectize-control .selectize-input { width: 75% !important; max-width: none !important; }
          .ao-map .selectize-dropdown { width: 75% !important; max-width: none !important; }
          .ao-map select.form-control { width: 75% !important; max-width: none !important; }
        ")),
          div(class = "ao-map",
              if (need_wind) tagList(
                selectInput("map_wind_speed", "Wind Speed",
                            choices = colnames(pred_file_data), width = "75%"),
                selectInput("map_wind_dir", "Wind Direction",
                            choices = colnames(pred_file_data), width = "75%")
              ),
              if (need_current) tagList(
                selectInput("map_current_speed", "Current Speed",
                            choices = colnames(pred_file_data), width = "75%"),
                selectInput("map_current_dir", "Current Direction",
                            choices = colnames(pred_file_data), width = "75%")
              ),
              if (need_wave) tagList(
                selectInput("map_wave_height", "Wave Height",
                            choices = colnames(pred_file_data), width = "75%"),
                selectInput("map_wave_dir", "Wave Direction",
                            choices = colnames(pred_file_data), width = "75%")
              )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("pred_map_confirm", "Continue")
        )
      ))
      
      # Stash state to continue after user confirms
      rv_pred$pending <- list(
        df    = pred_file_data,
        cols  = columns_to_grab,
        needs = list(wind = need_wind, current = need_current, wave = need_wave),
        model = input$model_choice
      )
      return()
    }
    
    # No A/O mapping needed: require only raw bases for transformed AND interaction features
    # Exclude transformed and interaction feature names themselves from ui_required
    trans_in_model <- columns_to_grab[grepl(TRANS_PATTERN, columns_to_grab, perl = TRUE)]
    ui_required    <- columns_to_grab[
      !grepl(TRANS_PATTERN, columns_to_grab, perl = TRUE) &
        !grepl(INTER_PATTERN, columns_to_grab, perl = TRUE)
    ]
    
    # Add bases for transformed features
    if (length(trans_in_model)) {
      bases <- if (exists("base_name", mode = "function")) {
        vapply(trans_in_model, base_name, FUN.VALUE = character(1))
      } else {
        sub(TRANS_PATTERN, "", trans_in_model, perl = TRUE)
      }
      ui_required <- unique(c(ui_required, bases))
    }
    
    # Add bases for interaction terms (exclude AO bases since no A/O mapping in this branch)
    if (length(inter_terms)) {
      inter_bases <- unlist(lapply(inter_terms, function(t) {
        comps <- get_inter_components(t)
        vapply(comps, get_base, FUN.VALUE = character(1))
      }), use.names = FALSE)
      inter_bases <- inter_bases[!(inter_bases %in% ao_all)]
      ui_required <- unique(c(ui_required, inter_bases))
    }
    
    # Validate raw columns exist in imported file
    missing_columns <- setdiff(ui_required, colnames(pred_file_data))
    if (length(missing_columns) > 0) {
      showModal(modalDialog(
        title = "Error",
        paste("The following required columns are missing in the imported prediction data:",
              paste(missing_columns, collapse = ", ")),
        easyClose = TRUE
      ))
      return()
    }
    
    # Build prediction-entry table with only raw inputs (no transformed/interaction columns)
    rv_name <- colnames(current_data())[response_var()]
    temp_data <- data.frame(matrix(NA, nrow = nrow(pred_file_data), ncol = length(ui_required) + 6))
    temp_data[, 1:2] <- pred_file_data[, 1:2, drop = FALSE]
    temp_data[, 3:(2 + length(ui_required))] <- pred_file_data[, ui_required, drop = FALSE]
    temp_data[, (ncol(temp_data) - 3):ncol(temp_data)] <- -999
    colnames(temp_data) <- c("Sample_ID", rv_name, ui_required, "Prediction","Lower_Bound","Upper_Bound","Outcome")
    
    updateNumericInput(session, "num_preds", value = nrow(temp_data))
    current_pred_page(1)
    
    if (input$model_choice == "Logistic_Regression") {
      LG_pred_data(temp_data)
    } else if (input$model_choice == "XGB_Classifier") {
      XGBCL_pred_data(temp_data)
    } else if (input$model_choice == "XGBoost") {
      XGB_pred_data(temp_data)
    } else if (input$model_choice == "Elastic_Net") {
      EN_pred_data(temp_data)
    }
    
    # Date formatting (unchanged)
    if (init_ID_format == "YMD") {
      temp_data[, 1] <- ymd(temp_data[, 1]);        date_format_string <<- "toLocaleDateString"
    } else if (init_ID_format == "MDY") {
      temp_data[, 1] <- mdy(temp_data[, 1]);        date_format_string <<- "toLocaleDateString"
    } else if (init_ID_format == "MDYHM") {
      temp_data[, 1] <- parse_date_time(temp_data[, 1], c('%m/%d/%y %H:%M'), exact = TRUE)
      date_format_string <<- "toLocaleString"
    } else if (init_ID_format == "Character") {
      date_format_string <<- "Character"
    } else if (init_ID_format == "Numeric") {
      date_format_string <<- "Numeric"
    }
    
    renderpreddata(temp_data, column_props, current_pred_page(), init_ID_format, output)
  })
  
  # Compute A/O components from mapped Magnitude/Direction columns in imported prediction file
  observeEvent(input$pred_map_confirm, {
    req(rv_pred$pending)
    df    <- rv_pred$pending$df
    needs <- rv_pred$pending$needs
    model <- rv_pred$pending$model
    cols  <- unique(as.character(rv_pred$pending$cols))
    
    `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
    
    # Helpers for interactions and transformed bases
    get_inter_components <- function(term) {
      s <- sub(INTER_PATTERN, "", term, perl = TRUE)
      strsplit(s, INTER_SEP, fixed = TRUE)[[1]]
    }
    get_base <- function(x) {
      if (grepl(TRANS_PATTERN, x, perl = TRUE)) {
        if (exists("base_name", mode = "function")) base_name(x) else sub(TRANS_PATTERN, "", x, perl = TRUE)
      } else x
    }
    
    # Ensure canonical names from training are known (rv_ao_map should have canonical names)
    canon_missing <- c(
      if (needs$wind    && (is.null(rv_ao_map$wind_speed)    || is.null(rv_ao_map$wind_dir)))   "Wind",
      if (needs$current && (is.null(rv_ao_map$current_speed) || is.null(rv_ao_map$current_dir))) "Currents",
      if (needs$wave    && (is.null(rv_ao_map$wave_height)   || is.null(rv_ao_map$wave_dir)))   "Waves"
    )
    if (length(canon_missing)) {
      showModal(modalDialog(
        title = "Missing component column names",
        HTML(sprintf(
          "The following groups do not have A/O component raw column names from the training data: %s.<br>
         Please create A/O components from the training data first (or define component names) before importing predictions.",
          paste(unique(canon_missing), collapse = ", ")
        )),
        easyClose = TRUE
      ))
      return()
    }
    
    # Validate user mapping selections
    if (needs$wind && (is.null(input$map_wind_speed) || is.null(input$map_wind_dir))) {
      showNotification("Please select both Wind Speed and Wind Direction.", type = "error"); return()
    }
    if (needs$current && (is.null(input$map_current_speed) || is.null(input$map_current_dir))) {
      showNotification("Please select both Current Speed and Current Direction.", type = "error"); return()
    }
    if (needs$wave && (is.null(input$map_wave_height) || is.null(input$map_wave_dir))) {
      showNotification("Please select both Wave Height and Wave Direction.", type = "error"); return()
    }
    
    # Copy/rename imported columns into canonical (training) names and compute A/O
    if (needs$wind) {
      sp_name <- rv_ao_map$wind_speed
      dr_name <- rv_ao_map$wind_dir
      df[[sp_name]] <- as.numeric(df[[input$map_wind_speed]])
      df[[dr_name]] <- as.numeric(df[[input$map_wind_dir]])
      df[["WindA"]] <- -df[[sp_name]] * cos((df[[dr_name]] - bo()) * pi/180)
      df[["WindO"]] <-  df[[sp_name]] * sin((df[[dr_name]] - bo()) * pi/180)
    }
    if (needs$current) {
      sp_name <- rv_ao_map$current_speed
      dr_name <- rv_ao_map$current_dir
      df[[sp_name]] <- as.numeric(df[[input$map_current_speed]])
      df[[dr_name]] <- as.numeric(df[[input$map_current_dir]])
      df[["CurrentA"]] <- -df[[sp_name]] * cos((df[[dr_name]] - bo()) * pi/180)
      df[["CurrentO"]] <-  df[[sp_name]] * sin((df[[dr_name]] - bo()) * pi/180)
    }
    if (needs$wave) {
      ht_name <- rv_ao_map$wave_height
      dr_name <- rv_ao_map$wave_dir
      df[[ht_name]] <- as.numeric(df[[input$map_wave_height]])
      df[[dr_name]] <- as.numeric(df[[input$map_wave_dir]])
      df[["WaveA"]] <- -df[[ht_name]] * cos((df[[dr_name]] - bo()) * pi/180)
      df[["WaveO"]] <-  df[[ht_name]] * sin((df[[dr_name]] - bo()) * pi/180)
    }
    
    # Build list of RAW columns required for the prediction UI (not transformed/interaction names)
    ao_groups <- list(
      wind    = list(ao = c("WindA","WindO"),
                     raw = c(rv_ao_map$wind_speed %||% "Wind Speed",
                             rv_ao_map$wind_dir   %||% "Wind Direction")),
      current = list(ao = c("CurrentA","CurrentO"),
                     raw = c(rv_ao_map$current_speed %||% "Current Speed",
                             rv_ao_map$current_dir   %||% "Current Direction")),
      wave    = list(ao = c("WaveA","WaveO"),
                     raw = c(rv_ao_map$wave_height %||% "Wave Height",
                             rv_ao_map$wave_dir    %||% "Wave Direction"))
    )
    ao_all <- unlist(lapply(ao_groups, `[[`, "ao"), use.names = FALSE)
    
    # Identify transformed and interaction terms in the model
    trans_in_model <- cols[grepl(TRANS_PATTERN, cols, perl = TRUE)]
    inter_terms    <- cols[grepl(INTER_PATTERN, cols, perl = TRUE)]
    
    # Start with non-transformed, non-interaction, non-A/O model features
    ui_required <- cols[
      !grepl(TRANS_PATTERN, cols, perl = TRUE) &
        !grepl(INTER_PATTERN, cols, perl = TRUE) &
        !(cols %in% ao_all)
    ]
    
    # For any A/O groups needed (in model), add their canonical raw inputs (mapped)
    if (any(cols %in% ao_groups$wind$ao))    ui_required <- c(ui_required, ao_groups$wind$raw)
    if (any(cols %in% ao_groups$current$ao)) ui_required <- c(ui_required, ao_groups$current$raw)
    if (any(cols %in% ao_groups$wave$ao))    ui_required <- c(ui_required, ao_groups$wave$raw)
    
    # Add bases for transformed features (skip A/O bases; raw inputs for A/O already added)
    if (length(trans_in_model)) {
      bases <- if (exists("base_name", mode = "function")) {
        vapply(trans_in_model, base_name, FUN.VALUE = character(1))
      } else {
        sub(TRANS_PATTERN, "", trans_in_model, perl = TRUE)
      }
      for (b in bases) {
        if (b %in% ao_all) next
        ui_required <- c(ui_required, b)
      }
    }
    
    # Add bases for interaction terms
    if (length(inter_terms)) {
      for (t in inter_terms) {
        parts <- get_inter_components(t)
        for (p in parts) {
          b <- get_base(p)
          if (b %in% c("WindA","WindO")) {
            ui_required <- c(ui_required, ao_groups$wind$raw)
          } else if (b %in% c("CurrentA","CurrentO")) {
            ui_required <- c(ui_required, ao_groups$current$raw)
          } else if (b %in% c("WaveA","WaveO")) {
            ui_required <- c(ui_required, ao_groups$wave$raw)
          } else {
            ui_required <- c(ui_required, b)
          }
        }
      }
    }
    
    # Validate RAW columns exist in imported file (do NOT require transformed/interaction names)
    ui_required <- unique(ui_required)
    ui_required <- ui_required[!is.null(ui_required) & nzchar(ui_required)]
    missing_columns <- setdiff(ui_required, colnames(df))
    if (length(missing_columns) > 0) {
      showModal(modalDialog(
        title = "Error",
        paste("The following required RAW columns are missing in the imported prediction data:",
              paste(missing_columns, collapse = ", ")),
        easyClose = TRUE
      ))
      return()
    }
    
    # Build and populate the displayed prediction table from df using RAW ui_required
    iv_name <- "Sample_ID"
    rv_name <- colnames(current_data())[response_var()]
    
    temp_data <- data.frame(matrix(NA, nrow = nrow(df), ncol = length(ui_required) + 6))
    temp_data[, 1:2] <- df[, 1:2, drop = FALSE]  # assumes first two cols are ID/RV in imported data
    temp_data[, 3:(2 + length(ui_required))] <- df[, ui_required, drop = FALSE]
    temp_data[, (ncol(temp_data) - 3):ncol(temp_data)] <- -999
    
    colnames(temp_data) <- c(
      iv_name,
      rv_name,
      ui_required,
      "Prediction", "Lower_Bound", "Upper_Bound", "Outcome"
    )
    
    updateNumericInput(session, "num_preds", value = nrow(temp_data))
    current_pred_page(1)
    
    # Store the displayed prediction table (RAW UI columns), not model columns
    if (model == "Logistic_Regression") {
      LG_pred_data(temp_data)
    } else if (model == "XGB_Classifier") {
      XGBCL_pred_data(temp_data)
    } else if (model == "XGBoost") {
      XGB_pred_data(temp_data)
    } else if (model == "Elastic_Net") {
      EN_pred_data(temp_data)
    }
    
    # Date formatting (unchanged)
    if (init_ID_format == "YMD") {
      temp_data[, 1] <- ymd(temp_data[, 1]);        date_format_string <<- "toLocaleDateString"
    } else if (init_ID_format == "MDY") {
      temp_data[, 1] <- mdy(temp_data[, 1]);        date_format_string <<- "toLocaleDateString"
    } else if (init_ID_format == "MDYHM") {
      temp_data[, 1] <- parse_date_time(temp_data[, 1], c('%m/%d/%y %H:%M'), exact = TRUE)
      date_format_string <<- "toLocaleString"
    } else if (init_ID_format == "Character") {
      date_format_string <<- "Character"
    } else if (init_ID_format == "Numeric") {
      date_format_string <<- "Numeric"
    }
    
    renderpreddata(temp_data, column_props, current_pred_page(), init_ID_format, output)
    
    removeModal()
    rv_pred$pending <- NULL
  })
  
  # Load a model for prediction
  observeEvent(input$model_choice, ignoreInit = T, {
    
    if (input$model_choice == "None") {
      output$pd_data = NULL
      output$pd_feat_ranges = NULL
      output$resid_text = NULL
      output$model_text = NULL
      output$pca_model_text = NULL
      
    } else {
      
      feature_mismatch(FALSE)
      standard_mismatch(FALSE)
      thresh_mismatch(FALSE)
      no_resids(FALSE)
      
      if (input$model_choice == "Logistic_Regression") {
        
        output$model_text = renderUI({
          threshold = LG_thresh()
          ifelse(is.null(LG_crit_prob()), LG_crit_prob(0.5), LG_crit_prob())
          probability = LG_crit_prob()
          
          feats = if (isTRUE(LG_model_PCA())) {
            nm = names(PCA_scaling_mean())
            if (is.null(nm) || !length(nm)) LG_final_features() else nm
          } else {
            LG_final_features()
          }
          feats = unique(feats)
          feats_str = if (length(feats)) paste(feats, collapse = ", ") else "(none)"
          
          HTML(paste0(
            "<div style='font-size: 20px; font-weight: bold;'>Model: Logistic Regression</div>",
            "<div style='font-size: 16px;'><span style='font-weight: bold;'>Model features:</span> ",feats_str,"</div>",
            "<div style='font-style: italic;'>Predictions: Probabilities of Exceedance</div>",
            "<div>Binarize Threshold: ", threshold, "</div>",
            "<div>Critical Probability: ", probability, "</div>"
          ))
        })
        
        model_to_use(LG_model)
        model_features = LG_final_features()
        pred_data(LG_pred_data())
        
        if (LG_model_PCA()) {
          
          final_model_PCA(TRUE)
          model_PCA_axes(colnames(LG_results())[4:ncol(LG_results())])
          
          if (is.null(LG_pred_results())) {
            
            no_resids(TRUE)
            resids = NULL
            
          } else if (LG_pred_standardize() != LG_standardize()) {
            
            standard_mismatch(TRUE)
            resids = NULL
            
          } else if (!identical(model_PCA_axes(),colnames(LG_pred_results())[4:ncol(LG_pred_results())])) {
            
            feature_mismatch(TRUE)
            resids = NULL
            
          } else if (LG_pred_thresh() != LG_thresh()) {
            
            thresh_mismatch(TRUE)
            resids = NULL
            
          } else {
            
            feature_mismatch(FALSE)
            standard_mismatch(FALSE)
            thresh_mismatch(FALSE)
            no_resids(FALSE)
          }
          
        } else {
          
          final_model_PCA(FALSE)
          model_PCA_axes(NULL)
          
          if (is.null(LG_pred_results())) {
            
            no_resids(TRUE)
            resids = NULL
            
          } else if (LG_pred_standardize() != LG_standardize()) {
            
            standard_mismatch(TRUE)
            resids = NULL
            
          } else if (!identical(model_features,colnames(LG_pred_results())[4:ncol(LG_pred_results())])) {
            
            feature_mismatch(TRUE)
            resids = NULL
            
          } else if (LG_pred_thresh() != LG_thresh()) {
            
            thresh_mismatch(TRUE)
            resids = NULL
            
          } else {
            
            feature_mismatch(FALSE)
            standard_mismatch(FALSE)
            thresh_mismatch(FALSE)
            no_resids(FALSE)
          }
        }
        
        if (!is.null(LG_pred_results()) && !feature_mismatch() && !standard_mismatch() && !thresh_mismatch() && !no_resids()) {
          
          resids = ((LG_pred_results()[,2] - LG_pred_results()[,3])^2)^0.5
          
        } else {
          
          resids = NULL
        }
        
      } else if (input$model_choice == "XGB_Classifier") {
        
        output$model_text = renderUI({
          threshold = XGBCL_thresh()
          ifelse(is.null(XGBCL_crit_prob()), XGBCL_crit_prob(0.5), XGBCL_crit_prob())
          probability = XGBCL_crit_prob()
          
          feats = if (isTRUE(XGBCL_model_PCA())) {
            nm = names(PCA_scaling_mean())
            if (is.null(nm) || !length(nm)) XGBCL_final_features() else nm
          } else {
            XGBCL_final_features()
          }
          feats = unique(feats)
          feats_str = if (length(feats)) paste(feats, collapse = ", ") else "(none)"
          
          HTML(paste0(
            "<div style='font-size: 20px; font-weight: bold;'>Model: XGBoost Classifier</div>",
            "<div style='font-size: 16px;'><span style='font-weight: bold;'>Model features:</span> ",feats_str,"</div>",
            "<div style='font-style: italic;'>Predictions: Probabilities of Exceedance</div>",
            "<div>Binarize Threshold: ", threshold, "</div>",
            "<div>Critical Probability: ", probability, "</div>"
          ))
        })
        
        model_to_use(XGBCL_model)
        model_features = XGBCL_final_features()
        pred_data(XGBCL_pred_data())
        
        if (XGBCL_model_PCA()) {
          
          final_model_PCA(TRUE)
          model_PCA_axes(colnames(XGBCL_results())[4:ncol(XGBCL_results())])
          
          if (is.null(XGBCL_pred_results())) {
            
            no_resids(TRUE)
            resids = NULL
            
          } else if (XGBCL_pred_standardize() != XGBCL_standardize()) {
            
            standard_mismatch(TRUE)
            resids = NULL
            
          } else if (!identical(model_PCA_axes(),colnames(XGBCL_pred_results())[4:ncol(XGBCL_pred_results())])) {
            
            feature_mismatch(TRUE)
            resids = NULL
            
          } else if (XGBCL_pred_thresh() != XGBCL_thresh()) {
            
            thresh_mismatch(TRUE)
            resids = NULL
            
          } else {
            
            feature_mismatch(FALSE)
            standard_mismatch(FALSE)
            thresh_mismatch(FALSE)
            no_resids(FALSE)
          }
          
        } else {
          
          final_model_PCA(FALSE)
          model_PCA_axes(NULL)
          
          if (is.null(XGBCL_pred_results())) {
            
            no_resids(TRUE)
            resids = NULL
            
          } else if (XGBCL_pred_standardize() != XGBCL_standardize()) {
            
            standard_mismatch(TRUE)
            resids = NULL
            
          } else if (!identical(model_features,colnames(XGBCL_pred_results())[4:ncol(XGBCL_pred_results())])) {
            
            feature_mismatch(TRUE)
            resids = NULL
            
          } else if (XGBCL_pred_thresh() != XGBCL_thresh()) {
            
            thresh_mismatch(TRUE)
            resids = NULL
            
          } else {
            
            feature_mismatch(FALSE)
            standard_mismatch(FALSE)
            thresh_mismatch(FALSE)
            no_resids(FALSE)
          }
        }
        
        if (!is.null(XGBCL_pred_results()) && !feature_mismatch() && !standard_mismatch() && !thresh_mismatch() && !no_resids()) {
          
          resids = ((XGBCL_pred_results()[,2] - XGBCL_pred_results()[,3])^2)^0.5
          
        } else {
          
          resids = NULL
        }
        
      } else if (input$model_choice == "XGBoost") {
        
        output$model_text = renderUI({
          
          feats = if (isTRUE(XGB_model_PCA())) {
            nm = names(PCA_scaling_mean())
            if (is.null(nm) || !length(nm)) XGB_final_features() else nm
          } else {
            XGB_final_features()
          }
          feats = unique(feats)
          feats_str = if (length(feats)) paste(feats, collapse = ", ") else "(none)"
          
          HTML(paste0(
            "<div style='font-size: 20px; font-weight: bold;'>Model: XGBoost</div>",
            "<div style='font-size: 16px;'><span style='font-weight: bold;'>Model features:</span> ",feats_str,"</div>",
            "<div style='font-style: italic;'>Predictions: Response Variable Units</div>",
            "<div>Regulatory Standard: ", input$XGB_stand, "</div>",
            "<div>Decision Criterion: ", input$XGB_dec_crit, "</div>"
          ))
        })
        
        model_to_use(XGB_model)
        model_features = XGB_final_features()
        pred_data(XGB_pred_data())
        
        if (XGB_model_PCA()) {
          
          final_model_PCA(TRUE)
          model_PCA_axes(colnames(XGB_results())[4:ncol(XGB_results())])
          
          if (is.null(XGB_pred_results())) {
            
            no_resids(TRUE)
            resids = NULL
            
          } else if (XGB_pred_standardize() != XGB_standardize()) {
            
            standard_mismatch(TRUE)
            resids = NULL
            
          } else if (!identical(model_PCA_axes(),colnames(XGB_pred_results())[4:ncol(XGB_pred_results())])) {
            
            feature_mismatch(TRUE)
            resids = NULL
            
          } else {
            
            feature_mismatch(FALSE)
            standard_mismatch(FALSE)
            no_resids(FALSE)
          }
          
        } else {
          
          final_model_PCA(FALSE)
          model_PCA_axes(NULL)
          
          if (is.null(XGB_pred_results())) {
            
            no_resids(TRUE)
            resids = NULL
            
          } else if (XGB_pred_standardize() != XGB_standardize()) {
            
            standard_mismatch(TRUE)
            resids = NULL
            
          } else if (!identical(model_features,colnames(XGB_pred_results())[4:ncol(XGB_pred_results())])) {
            
            feature_mismatch(TRUE)
            resids = NULL
            
          } else {
            
            feature_mismatch(FALSE)
            standard_mismatch(FALSE)
            no_resids(FALSE)
          }
        }
        
        if (!is.null(XGB_pred_results()) && !feature_mismatch() && !standard_mismatch() && !no_resids()) {
          
          resids = ((XGB_pred_results()[,2] - XGB_pred_results()[,3])^2)^0.5
          
        } else {
          
          resids = NULL
        }
        
      } else if (input$model_choice == "Elastic_Net") {
        
        output$model_text = renderUI({
          
          feats = if (isTRUE(EN_model_PCA())) {
            nm = names(PCA_scaling_mean())
            if (is.null(nm) || !length(nm)) EN_final_features() else nm
          } else {
            EN_final_features()
          }
          feats = unique(feats)
          feats_str = if (length(feats)) paste(feats, collapse = ", ") else "(none)"
          
          HTML(paste0(
            "<div style='font-size: 20px; font-weight: bold;'>Model: Elastic Net</div>",
            "<div style='font-size: 16px;'><span style='font-weight: bold;'>Model features:</span> ",feats_str,"</div>",
            "<div style='font-style: italic;'>Predictions: Response Variable Units</div>",
            "<div>Regulatory Standard: ", input$EN_stand, "</div>",
            "<div>Decision Criterion: ", input$EN_dec_crit, "</div>"
          ))
        })
        
        model_to_use(EN_model)
        model_features = EN_final_features()
        pred_data(EN_pred_data())
        
        if (EN_model_PCA()) {
          
          final_model_PCA(TRUE)
          model_PCA_axes(colnames(EN_results())[4:ncol(EN_results())])
          
          if (is.null(EN_pred_results())) {
            
            no_resids(TRUE)
            resids = NULL
            
          } else if (EN_pred_standardize() != EN_standardize()) {
            
            standard_mismatch(TRUE)
            resids = NULL
            
          } else if (!identical(model_PCA_axes(),colnames(EN_pred_results())[4:ncol(EN_pred_results())])) {
            
            feature_mismatch(TRUE)
            resids = NULL
            
          } else {
            
            feature_mismatch(FALSE)
            standard_mismatch(FALSE)
            no_resids(FALSE)
          }
          
        } else {
          
          final_model_PCA(FALSE)
          model_PCA_axes(NULL)
          
          if (is.null(EN_pred_results())) {
            
            no_resids(TRUE)
            resids = NULL
            
          } else if (EN_pred_standardize() != EN_standardize()) {
            
            standard_mismatch(TRUE)
            resids = NULL
            
          } else if (!identical(model_features,colnames(EN_pred_results())[4:ncol(EN_pred_results())])) {
            
            feature_mismatch(TRUE)
            resids = NULL
            
          } else {
            
            feature_mismatch(FALSE)
            standard_mismatch(FALSE)
            no_resids(FALSE)
          }
        }
        
        if (!is.null(EN_pred_results()) && !feature_mismatch() && !standard_mismatch() && !no_resids()) {
          
          resids = ((EN_pred_results()[,2] - EN_pred_results()[,3])^2)^0.5
          
        } else {
          
          resids = NULL
        }
      }
      
      if (final_model_PCA()) {
        
        output$pca_model_text = renderText({HTML("PCA axes used as features+")})
        
      } else {
        
        output$pca_model_text = NULL
      }

      iv_name <- "Sample_ID"
      rv_name <- colnames(current_data())[response_var()]
      
      `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
      
      # A/O groups and mapping to raw inputs
      ao_groups <- list(
        wind    = list(ao = c("WindA","WindO"),
                       raw = c(rv_ao_map$wind_speed   %||% "Wind Speed",
                               rv_ao_map$wind_dir     %||% "Wind Direction")),
        current = list(ao = c("CurrentA","CurrentO"),
                       raw = c(rv_ao_map$current_speed %||% "Current Speed",
                               rv_ao_map$current_dir   %||% "Current Direction")),
        wave    = list(ao = c("WaveA","WaveO"),
                       raw = c(rv_ao_map$wave_height %||% "Wave Height",
                               rv_ao_map$wave_dir    %||% "Wave Direction"))
      )
      ao_all <- unlist(lapply(ao_groups, `[[`, "ao"), use.names = FALSE)
      
      get_inter_components <- function(term) {
        s <- sub(INTER_PATTERN, "", term, perl = TRUE)
        strsplit(s, INTER_SEP, fixed = TRUE)[[1]]
      }
      
      get_base <- function(x) {
        if (grepl(TRANS_PATTERN, x, perl = TRUE)) {
          if (exists("base_name", mode = "function")) base_name(x) else sub(TRANS_PATTERN, "", x, perl = TRUE)
        } else x
      }
      
      # Start with model features excluding A/O components
      ui_features <- model_features[!model_features %in% ao_all]
      
      # Add A/O raw inputs once if the model uses that A/O group
      for (g in names(ao_groups)) {
        if (any(model_features %in% ao_groups[[g]]$ao)) {
          for (nm in ao_groups[[g]]$raw) {
            if (!nm %in% ui_features) ui_features <- c(ui_features, nm)
          }
        }
      }
      
      inter_terms <- model_features[grepl(INTER_PATTERN, model_features, perl = TRUE)]
      if (length(inter_terms)) {
        comps  <- lapply(inter_terms, get_inter_components)
        bases  <- unlist(lapply(comps, function(parts) vapply(parts, get_base, FUN.VALUE = character(1))), use.names = FALSE)
        
        need_wind    <- any(bases %in% c("WindA","WindO"))
        need_current <- any(bases %in% c("CurrentA","CurrentO"))
        need_wave    <- any(bases %in% c("WaveA","WaveO"))
        
        if (need_wind) {
          for (nm in ao_groups$wind$raw) if (!nm %in% ui_features) ui_features <- c(ui_features, nm)
        }
        if (need_current) {
          for (nm in ao_groups$current$raw) if (!nm %in% ui_features) ui_features <- c(ui_features, nm)
        }
        if (need_wave) {
          for (nm in ao_groups$wave$raw) if (!nm %in% ui_features) ui_features <- c(ui_features, nm)
        }
      }
      
      # Remove any transformed features AND interaction terms from the UI list
      ui_features <- ui_features[!grepl(TRANS_PATTERN, ui_features, perl = TRUE)]
      ui_features <- ui_features[!grepl(INTER_PATTERN, ui_features, perl = TRUE)]
      
      # Ensure raw base features are shown for each transformed feature in the model (unchanged)
      trans_in_model <- model_features[grepl(TRANS_PATTERN, model_features, perl = TRUE)]
      if (length(trans_in_model)) {
        bases <- sub(TRANS_PATTERN, "", trans_in_model, perl = TRUE)
        for (b in bases) {
          if (b %in% ao_all) next
          if (!b %in% ui_features) ui_features <- c(ui_features, b)
        }
      }
      
      # NEW: ensure raw base features are shown for each interaction term in the model
      if (length(inter_terms)) {
        for (t in inter_terms) {
          parts <- get_inter_components(t)
          for (p in parts) {
            b <- get_base(p)
            # Skip A/O bases—raw speed/dir or height/dir were already added above
            if (b %in% ao_all) next
            if (!b %in% ui_features) ui_features <- c(ui_features, b)
          }
        }
      }
      
      # Deduplicate while preserving order
      ui_features <- unique(ui_features)
      
      # Build the prediction-entry table using UI features
      temp_data <- data.frame(matrix(-999, nrow = input$num_preds, ncol = length(ui_features) + 6))
      colnames(temp_data) <- c(iv_name, rv_name, ui_features, "Prediction","Lower_Bound","Upper_Bound","Outcome")
      
      if (is.null(pred_data()) || changed_model()) {
        pred_data(temp_data)
        changed_model(FALSE)
      }
      
      pred_residuals(resids)
      pred_model_features(model_features)
      
      # if (input$num_preds == 1) {
      #   
      #   temp_data[1,1:2] = 0
      #   temp_data[1,(ncol(temp_data)-2):ncol(temp_data)] = 0
      #   
      # } else {
      #   
      #   temp_data[,1:2] = 0
      #   temp_data[,(ncol(temp_data)-2):ncol(temp_data)] = 0
      # }
      
      if (no_resids()) {
        output$resid_text = renderUI({HTML("<div style='font-size: 16px; font-weight: bold;'>NO PREDICTION RESIDUALS: Run a prediction model for confidence intervals</div>")})
      } else if (standard_mismatch()) {
        output$resid_text = renderUI({HTML("<div style='font-size: 16px; font-weight: bold;'>NO PREDICTION RESIDUALS: Prediction/Fitted models use different feature standardizations</div>")})
      } else if (feature_mismatch()) {
        output$resid_text = renderUI({HTML("<div style='font-size: 16px; font-weight: bold;'>NO PREDICTION RESIDUALS: Prediction/Fitted models use different features</div>")})
      } else if (thresh_mismatch()) {
        output$resid_text = renderUI({HTML("<div style='font-size: 16px; font-weight: bold;'>NO PREDICTION RESIDUALS: Prediction/Fitted models use different binarize thresholds</div>")})
      } else if (is.null(resids)) {
        output$resid_text = renderUI({HTML("<div style='font-size: 16px; font-weight: bold;'>NO PREDICTION RESIDUALS: Run a prediction model for confidence intervals</div>")})
      } else {
        output$resid_text = renderUI({HTML("<div style='font-size: 16px; font-weight: bold;'>PREDICTION RESIDUALS AVAILABLE for confidence interval calculations</div>")})
      }
      
      current_pred_page(1)
      
      # Build the columns to display in the Feature Ranges table; use model_features for non-A/O features; for any A/O groups present in the model, replace with the canonical raw columns
      ao_groups = list(
        wind    = list(ao = c("WindA","WindO"),
                       raw = c(rv_ao_map$wind_speed,    rv_ao_map$wind_dir)),
        current = list(ao = c("CurrentA","CurrentO"),
                       raw = c(rv_ao_map$current_speed, rv_ao_map$current_dir)),
        wave    = list(ao = c("WaveA","WaveO"),
                       raw = c(rv_ao_map$wave_height,   rv_ao_map$wave_dir))
      )
      
      ao_all = unlist(lapply(ao_groups, `[[`, "ao"), use.names = FALSE)
      
      # Start with model features excluding A/O features
      ui_features_ranges = model_features[!model_features %in% ao_all]
      
      # For each group with A/O in the model, append its canonical raw columns (once)
      for (g in names(ao_groups)) {
        if (any(model_features %in% ao_groups[[g]]$ao)) {
          raws = ao_groups[[g]]$raw
          # keep only non-null names that actually exist in current_data()
          raws = raws[!vapply(raws, is.null, logical(1))]
          raws = raws[raws %in% colnames(current_data())]
          if (length(raws) == 0) {
            # Fallback: if raw mapping not available, keep the A/O names so table isn't empty
            for (ao in ao_groups[[g]]$ao) {
              if (!(ao %in% ui_features_ranges)) ui_features_ranges = c(ui_features_ranges, ao)
            }
          } else {
            for (nm in raws) {
              if (!(nm %in% ui_features_ranges)) ui_features_ranges = c(ui_features_ranges, nm)
            }
          }
        }
      }
      
      # Deduplicate while preserving order and keep only columns present in current_data
      present_cols = unique(ui_features_ranges)
      present_cols = present_cols[present_cols %in% colnames(current_data())]
      
      # Compute ranges on these columns
      if (length(present_cols) > 0) {
        selected_data = current_data()[, present_cols, drop = FALSE]
        num_cols = length(present_cols)
        
        min_feats = apply(selected_data, 2, min, na.rm = TRUE)
        max_feats = apply(selected_data, 2, max, na.rm = TRUE)
        
        feature_ranges = data.frame(matrix(NA, nrow = 2, ncol = num_cols + 1))
        feature_ranges[1, 1] <- "Minimum"
        feature_ranges[2, 1] <- "Maximum"
        feature_ranges[1, 2:(1 + num_cols)] = magnitude_round(min_feats)
        feature_ranges[2, 2:(1 + num_cols)] = magnitude_round(max_feats)
        
        colnames(feature_ranges) = c("Feature_Characteristic", present_cols)
      } else {
        # Graceful fallback: only the label column
        feature_ranges = data.frame(Feature_Characteristic = c("Minimum", "Maximum"))
      }
      
      model_feature_ranges(feature_ranges)
      
      renderpreddata(pred_data(), column_props, current_pred_page(), init_ID_format, output)
      
      if (input$model_choice == "Logistic_Regression") {
        LG_pred_data(pred_data())
      } else if (input$model_choice == "XGB_Classifier") {
        XGBCL_pred_data(pred_data())
      } else if (input$model_choice == "XGBoost") {
        XGB_pred_data(pred_data())
      } else if (input$model_choice == "Elastic_Net") {
        EN_pred_data(pred_data())
      }
      
      output$pd_feat_ranges = DT::renderDataTable(server = TRUE, {
        datatable(
          data.frame(feature_ranges),
          rownames = FALSE,
          selection = "none",
          colnames = c("Feature Characteristic", colnames(feature_ranges)[2:ncol(feature_ranges)]),
          editable = FALSE,
          options = list(
            autoWidth = FALSE,
            dom = 't',
            paging = FALSE,
            pageLength = 5,
            scrollX = TRUE,
            scrollY = FALSE,
            columnDefs = list(
              list(targets = 0, width = "170px"),
              list(targets = '_all', className = 'dt-center')
            ),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "}"
            )
          )
        )
      })
    }
  })
  
  # Change the number of desired predictions
  observeEvent(input$num_preds, ignoreInit = TRUE, {
    req(iv$is_valid())
    
    temp_data <- switch(input$model_choice,
                        "Logistic_Regression" = LG_pred_data(),
                        "XGB_Classifier"      = XGBCL_pred_data(),
                        "XGBoost"             = XGB_pred_data(),
                        "Elastic_Net"         = EN_pred_data(),
                        NULL
    )
    req(!is.null(temp_data))
    
    cols      <- colnames(temp_data)
    n_current <- nrow(temp_data)
    n_target  <- input$num_preds
    n_new     <- n_target - n_current
    
    if (n_new > 0) {
      # Append rows with the same columns as temp_data
      new_rows <- as.data.frame(matrix(-999, nrow = n_new, ncol = length(cols)))
      names(new_rows) <- cols
      temp_data1 <- rbind(temp_data, new_rows)
      
      # Fill ID/Response for the appended rows with "-999"
      temp_data1[(n_current + 1):nrow(temp_data1), 1] <- "-999"
      temp_data1[(n_current + 1):nrow(temp_data1), 2] <- "-999"
      # Coerce those two columns to character so "-999" shows and avoids NA
      temp_data1[, 1] <- as.character(temp_data1[, 1])
      temp_data1[, 2] <- as.character(temp_data1[, 2])
      
    } else if (n_target == 1) {
      # Rebuild as a single-row table with same columns
      temp_data1 <- as.data.frame(matrix(-999, nrow = 1, ncol = length(cols)))
      names(temp_data1) <- cols
      
      # Fill ID/Response for the single row with "-999"
      temp_data1[1, 1] <- "-999"
      temp_data1[1, 2] <- "-999"
      temp_data1[, 1]  <- as.character(temp_data1[, 1])
      temp_data1[, 2]  <- as.character(temp_data1[, 2])
      
    } else {
      # Truncate to requested number of rows
      temp_data1 <- temp_data[seq_len(n_target), , drop = FALSE]
    }
    
    # Save back to the appropriate store
    switch(input$model_choice,
           "Logistic_Regression" = LG_pred_data(temp_data1),
           "XGB_Classifier"      = XGBCL_pred_data(temp_data1),
           "XGBoost"             = XGB_pred_data(temp_data1),
           "Elastic_Net"         = EN_pred_data(temp_data1)
    )
    
    renderpreddata(temp_data1, column_props, current_pred_page(), init_ID_format, output)
  })
  
  # Prediction dataset cell editing
  observeEvent(input$pd_data_cell_edit, ignoreInit = T, {
    
    info = input$pd_data_cell_edit
    
    if (input$model_choice == "Logistic_Regression") {
      
      data = LG_pred_data()
      
    } else if (input$model_choice == "XGB_Classifier") {
      
      data = XGBCL_pred_data()
      
    } else if (input$model_choice == "XGBoost") {
      
      data = XGB_pred_data()
      
    } else if (input$model_choice == "Elastic_Net") {
      
      data = EN_pred_data()
    }
    
    if (is.null(info) || info$value == "" || is.na(info$value)) {
      
      showModal(modalDialog(title = "Input Error",paste("Missing feature values are not allowed."),easyClose = FALSE,footer = div(modalButton('Close'))))
      renderpreddata(data,column_props,current_pred_page(),init_ID_format,output)
      
    } else {
      
      i = info$row
      j = info$col + 1
      
      data_new = editData(data,input$pd_data_cell_edit,"pd_data",rownames = FALSE)
      pred_data(data_new)
      
      if (input$model_choice == "Logistic_Regression") {
        
        LG_pred_data(data_new)
        
      } else if (input$model_choice == "XGB_Classifier") {
        
        XGBCL_pred_data(data_new)
        
      } else if (input$model_choice == "XGBoost") {
        
        XGB_pred_data(data_new)
        
      } else if (input$model_choice == "Elastic_Net") {
        
        EN_pred_data(data_new)
      }
      
      renderpreddata(data_new,column_props,current_pred_page(),init_ID_format,output)
    }
  })
  
  # Make predictions using selected model
  observeEvent(input$make_preds, ignoreInit = TRUE, {
    req(iv$is_valid())
    
    # 1) Get the displayed prediction table for the chosen model
    prediction_data = switch(input$model_choice,
                              "Logistic_Regression" = LG_pred_data(),
                              "XGB_Classifier"      = XGBCL_pred_data(),
                              "XGBoost"             = XGB_pred_data(),
                              "Elastic_Net"         = EN_pred_data(),
                              NULL
    )
    
    if (is.null(prediction_data)) {
      showModal(modalDialog(
        "Missing feature values are not allowed. Provide all feature values to make predictions.",
        easyClose = FALSE, footer = div(modalButton('Close'))
      ))
      return()
    }
    
    # 2) Validate no missing values in the UI feature region
    ui_start <- 3
    ui_stop  <- ncol(prediction_data) - 4  # last 4 are Prediction/Bounds/Outcome
    ui_df    <- if (ui_stop >= ui_start) prediction_data[, ui_start:ui_stop, drop = FALSE] else NULL
    
    if (is.null(ui_df) ||
        any(is.na(ui_df)) ||
        any(as.matrix(ui_df) == -999, na.rm = TRUE)) {
      showModal(modalDialog(
        "Missing feature values detected. Provide all feature values to make predictions.",
        easyClose = FALSE, footer = div(modalButton('Close'))
      ))
      return()
    }
    
    # 3) Determine the model + features the model expects
    model = model_to_use()
    model_features = switch(input$model_choice,
                             "Logistic_Regression" = LG_final_features(),
                             "XGB_Classifier"      = XGBCL_final_features(),
                             "XGBoost"             = XGB_final_features(),
                             "Elastic_Net"         = EN_final_features()
    )
    
    # 4) Build a working feature frame from the UI table and compute A/O if needed
    # The UI feature columns (3:ui_stop) contain raw Magnitude/Direction columns (canonical names).
    df_ui = as.data.frame(prediction_data[, ui_start:ui_stop, drop = FALSE])
    
    # Compute A/O components if the model expects them.
    need_wind    = any(model_features %in% c("WindA",    "WindO"))
    need_current = any(model_features %in% c("CurrentA", "CurrentO"))
    need_wave    = any(model_features %in% c("WaveA",    "WaveO"))
    
    `%||%` = function(a, b) if (!is.null(a) && nzchar(a)) a else b
    
    # Wind
    if (need_wind) {
      sp = rv_ao_map$wind_speed   %||% "Wind Speed"
      dr = rv_ao_map$wind_dir     %||% "Wind Direction"
      if (!all(c(sp, dr) %in% names(df_ui))) {
        showModal(modalDialog(
          "Wind raw inputs are missing from the prediction table.", easyClose = TRUE
        ))
        return()
      }
      ws = as.numeric(df_ui[[sp]])
      wd = as.numeric(df_ui[[dr]])
      df_ui[["WindA"]] = -ws * cos((wd - bo()) * pi/180)
      df_ui[["WindO"]] =  ws * sin((wd - bo()) * pi/180)
    }
    
    # Currents
    if (need_current) {
      sp = rv_ao_map$current_speed %||% "Current Speed"
      dr = rv_ao_map$current_dir   %||% "Current Direction"
      if (!all(c(sp, dr) %in% names(df_ui))) {
        showModal(modalDialog(
          "Current raw inputs are missing from the prediction table.", easyClose = TRUE
        ))
        return()
      }
      cs = as.numeric(df_ui[[sp]])
      cd = as.numeric(df_ui[[dr]])
      df_ui[["CurrentA"]] = -cs * cos((cd - bo()) * pi/180)
      df_ui[["CurrentO"]] =  cs * sin((cd - bo()) * pi/180)
    }
    
    # Waves
    if (need_wave) {
      ht = rv_ao_map$wave_height %||% "Wave Height"
      dr = rv_ao_map$wave_dir    %||% "Wave Direction"
      if (!all(c(ht, dr) %in% names(df_ui))) {
        showModal(modalDialog(
          "Wave raw inputs are missing from the prediction table.", easyClose = TRUE
        ))
        return()
      }
      wh = as.numeric(df_ui[[ht]])
      wd = as.numeric(df_ui[[dr]])
      df_ui[["WaveA"]] = -wh * cos((wd - bo()) * pi/180)
      df_ui[["WaveO"]] =  wh * sin((wd - bo()) * pi/180)
    }
    
    # Map prefix -> transform kind
    prefix_kind <- list(
      "Log.."     = "Log",
      "Inverse.." = "Inverse",
      "Square.."  = "Square",
      "Sqrt.."    = "Sqrt",
      "Qdrt.."    = "Qdrt",
      "Poly.."    = "Poly"
    )
    
    # For every transformed model feature, compute it from its base raw column in df_ui
    trans_feats <- model_features[grepl(TRANS_PATTERN, model_features)]
    if (length(trans_feats)) {
      for (tf in trans_feats) {
        base <- sub(TRANS_PATTERN, "", tf)
        if (!base %in% names(df_ui)) {
          showModal(modalDialog(
            title = "Missing raw input",
            paste0("Prediction table is missing raw feature '", base, "' required to compute '", tf, "'."),
            easyClose = TRUE
          ))
          return()
        }
        # Identify which prefix/kind applies
        prefix <- sub(paste0("^(Log..|Inverse..|Square..|Sqrt..|Qdrt..|Poly..).*$"), "\\1", tf)
        kind   <- prefix_kind[[prefix]]
        df_ui[[tf]] <- compute_transform(as.numeric(df_ui[[base]]), kind)
      }
    }
    
    int_feats <- model_features[is_interaction(model_features)]
    if (length(int_feats)) {
      for (nm in int_feats) {
        bases <- parse_inter_bases(nm)  # c(a, b)
        a <- bases[1]; b <- bases[2]
        if (!all(c(a, b) %in% names(df_ui))) {
          showModal(modalDialog(
            title = "Missing inputs for interactions",
            paste0("Missing raw input(s) '", paste(setdiff(c(a, b), names(df_ui)), collapse = ", "),
                   "' required to compute interaction '", nm, "'."),
            easyClose = TRUE
          ))
          return()
        }
        df_ui[[nm]] <- as.numeric(df_ui[[a]]) * as.numeric(df_ui[[b]])
      }
    }
    
    # 5) Build the model input matrix in the exact feature order required by the model
    # For PCA models, we must project from the pre-PCA feature space.
    final_pca = isTRUE(final_model_PCA())
    
    if (final_pca) {
      # Names of the original (pre-PCA) features used to fit PCA
      base_vars = names(PCA_scaling_mean())
      if (is.null(base_vars)) {
        showModal(modalDialog(
          "PCA scaling information is missing. Cannot compute PCA features.",
          easyClose = TRUE
        ))
        return()
      }
      
      missing_base = setdiff(base_vars, names(df_ui))
      if (length(missing_base) > 0) {
        showModal(modalDialog(
          title = "Missing pre-PCA features",
          HTML(paste(
            "The following pre-PCA features are required but not found in the prediction table:",
            paste(missing_base, collapse = ", ")
          )),
          easyClose = TRUE
        ))
        return()
      }
      
      pred_base = as.data.frame(df_ui[, base_vars, drop = TRUE])
      # Ensure it is 2D even for single-row, single-col cases
      if (is.null(dim(pred_base))) pred_base = as.data.frame(matrix(pred_base, nrow = nrow(df_ui), dimnames = list(NULL, base_vars)))
      
      matched_mean = PCA_scaling_mean()[base_vars]
      matched_sd   = PCA_scaling_sd()[base_vars]
      scaled_pred  = scale(pred_base, center = matched_mean, scale = matched_sd)
      
      # Project onto the axes used by the model
      axes = model_PCA_axes()
      if (is.null(axes) || length(axes) == 0) {
        showModal(modalDialog(
          "Model PCA axes are not available.", easyClose = TRUE
        ))
        return()
      }
      coeff = PCA_coefficients()
      # Expect coeff rows = base_vars, columns include axes
      # Ensure ordering matches
      coeff_sub = as.matrix(coeff[base_vars, axes, drop = FALSE])
      pred_pca_data = as.data.frame(as.matrix(scaled_pred) %*% coeff_sub)
      
      # Sanity: columns of pred_pca_data should be axes (model features)
      # We'll feed pred_pca_data to predict()
    } else {
      # Non-PCA: ensure all model_features exist (from df_ui that now includes any needed A/O)
      missing_feats = setdiff(model_features, names(df_ui))
      if (length(missing_feats) > 0) {
        showModal(modalDialog(
          title = "Missing required features",
          HTML(paste(
            "The following required model features are missing:",
            paste(missing_feats, collapse = ", ")
          )),
          easyClose = TRUE
        ))
        return()
      }
      pred_feat_data = as.data.frame(df_ui[, model_features, drop = FALSE])
      # Ensure 2D for single-row edge cases
      if (is.null(dim(pred_feat_data))) {
        pred_feat_data = as.data.frame(matrix(pred_feat_data, nrow = nrow(df_ui), dimnames = list(NULL, model_features)))
      }
    }
    
    # 6) Predict with the chosen model
    resids = pred_residuals()
    rv_val = prediction_data[, 2, drop = TRUE]  # response column for outcome labelling
    outcomes = character(nrow(prediction_data))
    
    if (input$model_choice == "Logistic_Regression") {
      if (final_pca) {
        predictions = predict(model, newx = as.matrix(pred_pca_data), type = "response", scale = LG_standardize())
      } else {
        predictions = predict(model, newx = as.matrix(pred_feat_data), type = "response", scale = LG_standardize())
      }
      
      for (i in seq_len(nrow(prediction_data))) {
        if (is.numeric(rv_val[i])) {
          outcomes[i] = ifelse(predictions[i] >= LG_crit_prob() && rv_val[i] >= LG_thresh(), "TP",
                                ifelse(predictions[i] <= LG_crit_prob() && rv_val[i] <= LG_thresh(), "TN",
                                       ifelse(predictions[i] >= LG_crit_prob() && rv_val[i] <= LG_thresh(), "FP", "FN")))
        } else {
          outcomes[i] = NA
        }
      }
      
    } else if (input$model_choice == "XGB_Classifier") {
      if (final_pca) {
        predictions = predict(model, newdata = as.matrix(pred_pca_data), type = "response", scale = XGBCL_standardize())
      } else {
        predictions = predict(model, newdata = as.matrix(pred_feat_data), type = "response", scale = XGBCL_standardize())
      }
      
      for (i in seq_len(nrow(prediction_data))) {
        if (is.numeric(rv_val[i])) {
          outcomes[i] = ifelse(predictions[i] >= XGBCL_crit_prob() && rv_val[i] >= XGBCL_thresh(), "TP",
                                ifelse(predictions[i] <= XGBCL_crit_prob() && rv_val[i] <= XGBCL_thresh(), "TN",
                                       ifelse(predictions[i] >= XGBCL_crit_prob() && rv_val[i] <= XGBCL_thresh(), "FP", "FN")))
        } else {
          outcomes[i] = NA
        }
      }
      
    } else if (input$model_choice == "XGBoost") {
      if (final_pca) {
        predictions = predict(model, newdata = as.matrix(pred_pca_data), scale = XGB_standardize())
      } else {
        predictions = predict(model, newdata = as.matrix(pred_feat_data), scale = XGB_standardize())
      }
      
      for (i in seq_len(nrow(prediction_data))) {
        if (is.numeric(rv_val[i])) {
          outcomes[i] = ifelse(predictions[i] >= input$XGB_dec_crit && rv_val[i] >= input$XGB_stand, "TP",
                                ifelse(predictions[i] <= input$XGB_dec_crit && rv_val[i] <= input$XGB_stand, "TN",
                                       ifelse(predictions[i] >= input$XGB_dec_crit && rv_val[i] <= input$XGB_stand, "FP", "FN")))
        } else {
          outcomes[i] = NA
        }
      }
      
    } else if (input$model_choice == "Elastic_Net") {
      if (final_pca) {
        predictions = predict(model, newx = as.matrix(pred_pca_data), scale = EN_standardize())
      } else {
        predictions = predict(model, newx = as.matrix(pred_feat_data), scale = EN_standardize())
      }
      
      for (i in seq_len(nrow(prediction_data))) {
        if (is.numeric(rv_val[i])) {
          outcomes[i] = ifelse(predictions[i] >= input$EN_dec_crit && rv_val[i] >= input$EN_stand, "TP",
                                ifelse(predictions[i] <= input$EN_dec_crit && rv_val[i] <= input$EN_stand, "TN",
                                       ifelse(predictions[i] >= input$EN_dec_crit && rv_val[i] <= input$EN_stand, "FP", "FN")))
        } else {
          outcomes[i] = NA
        }
      }
    }
    
    # 7) Confidence bounds from residuals
    if (!is.null(resids)) {
      sorted_resids = sort(resids, decreasing = TRUE)
      percentile = 100 * (1 - input$conf_bound)
      index = ceiling((percentile / 100) * length(sorted_resids))
      crit_value = sorted_resids[index]
      upper_bound = round(predictions + crit_value, 3)
      lower_bound = round(predictions - crit_value, 3)
      if (input$model_choice %in% c("Logistic_Regression", "XGB_Classifier")) {
        upper_bound[upper_bound > 1] = 1
        lower_bound[lower_bound < 0] = 0
      }
    } else {
      upper_bound = rep(NA, length(predictions))
      lower_bound = rep(NA, length(predictions))
    }
    
    # 8) Update only the last four columns of the displayed table
    displayed = prediction_data
    n = nrow(displayed)
    displayed[, ncol(displayed) - 3] = round(predictions, 3)   # Prediction
    displayed[, ncol(displayed) - 2] = lower_bound             # Lower_Bound
    displayed[, ncol(displayed) - 1] = upper_bound             # Upper_Bound
    displayed[, ncol(displayed)]     = outcomes                # Outcome
    
    # 9) Save back and render
    pred_data(displayed)  # main store used by output
    # Optionally also update model-specific stores to keep them in sync:
    switch(input$model_choice,
           "Logistic_Regression" = LG_pred_data(displayed),
           "XGB_Classifier"      = XGBCL_pred_data(displayed),
           "XGBoost"             = XGB_pred_data(displayed),
           "Elastic_Net"         = EN_pred_data(displayed)
    )
    
    output$pd_data = renderpreddata(pred_data(), column_props, current_pred_page(), init_ID_format, output)
  })
  
  # Disconnect the opened SQLite database
  session$onSessionEnded(function() {
    dbDisconnect(temp_db)
  })
  
}

shinyApp(ui, server)