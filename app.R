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
library(Matrix)
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
library(shinyjs)
library(shinythemes)
library(shinyvalidate)
library(shinyWidgets)
library(stringr)
library(tidyverse)
library(tools)
library(xgboost)
library(DT)

# library(NCmisc)
# library(here)

plan(multicore)

source("ui.R")
source("app_variables.R")
source("createAO.R")
source("confusion.R")
source("global_functions.R")
source("input_validation.R")
source("lineplot.R")
source("map_click.R")
source("rain.R")
source("renderdata.R")
source("renderPCAdata.R")
source("renderpreddata.R")
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
      zoom_threshold = 13
      bounds = input$map_bounds
      
      if (zoom < zoom_threshold) {
        
        updateSwitchInput(session,"show_shorelines",value = FALSE)
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
  
  output$zoom_level = renderText({
    input$map_zoom
  })
  
  output$beach_orient = renderText({bo()})
  
  # Save Project file from Data tab
  output$save_project = downloadHandler(filename = function() {paste("Project_File.RData")}, content = function(file) {
    
    save_list = list(
      type = "Project",
      Version = Version,
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
      date_format_string = date_format_string,
      saved_lc_val = input$lc_val,
      saved_rc_val = input$rc_val,
      saved_num_axes = input$num_axes,
      init_column_props = init_column_props,
      column_props = column_props,
      PCA_scaling_mean = reactiveVal(),
      PCA_scaling_sd = reactiveVal(),
      PCA_dataset = PCA_dataset(),
      PCA_summary_df = PCA_summary_df(),
      PCA_coefficients = PCA_coefficients(),
      pca_axes_max = pca_axes_max(),
      pca_axes = pca_axes(),
      pcax_being_used = pcax_being_used(),
      fs_pcax_used = fs_pcax_used(),
      final_model_PCA = final_model_PCA(),
      
      LG_pred_results = LG_pred_results(),
      LG_pred_coeffs = LG_pred_coeffs(),
      LG_pred_confuse_results = LG_pred_confuse_results(),
      LG_pred_scat_dat = LG_pred_scat_dat(),
      LG_pred_standardize = LG_pred_standardize(),
      LG_results = LG_results(),
      LG_coeffs = LG_coeffs(),
      LG_confuse_results = LG_confuse_results(),
      LG_scat_dat = LG_scat_dat(),
      LG_model = LG_model,
      LG_standardize = LG_pred_standardize(),
      LG_model_PCA = LG_model_PCA(),
      
      XGBCL_pred_results = XGBCL_pred_results(),
      XGBCL_pred_coeffs = XGBCL_pred_coeffs(),
      XGBCL_pred_confuse_results = XGBCL_pred_confuse_results(),
      XGBCL_pred_scat_dat = XGBCL_pred_scat_dat(),
      XGBCL_pred_standardize = XGBCL_pred_standardize(),
      XGBCL_selection_results = XGBCL_selection_results(),
      XGBCL_results = XGBCL_results(),
      XGBCL_coeffs = XGBCL_coeffs(),
      XGBCL_confuse_results = XGBCL_confuse_results(),
      XGBCL_scat_dat = XGBCL_scat_dat(),
      XGBCL_model = XGBCL_model,
      XGBCL_standardize = XGBCL_standardize(),
      XGBCL_model_PCA = XGBCL_model_PCA(),
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
      XGB_scat_dat = XGB_scat_dat(),
      XGB_model = XGB_model,
      XGB_standardize = XGB_standardize(),
      XGB_model_PCA = XGB_model_PCA(),
      Optimal_HP = Optimal_HP,
      
      EN_pred_results = EN_pred_results(),
      EN_pred_coeffs = EN_pred_coeffs(),
      EN_pred_confuse_results = EN_pred_confuse_results(),
      EN_pred_scat_dat = EN_pred_scat_dat(),
      EN_pred_standardize = EN_pred_standardize(),
      EN_results = EN_results(),
      EN_coeffs = EN_coeffs(),
      EN_confuse_results = EN_confuse_results(),
      EN_scat_dat = EN_scat_dat(),
      EN_model = EN_model,
      EN_standardize = EN_standardize(),
      EN_model_PCA = EN_model_PCA()
    )
    
    save(save_list, file = file)
    })
  
  # Save Project file from Modeling tab
  output$save_project2 = downloadHandler(filename = function() {paste("Project_File.RData")}, content = function(file) {
    
    save_list = list(
      type = "Project",
      Version = Version,
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
      date_format_string = date_format_string,
      saved_lc_val = input$lc_val,
      saved_rc_val = input$rc_val,
      saved_num_axes = input$num_axes,
      init_column_props = init_column_props,
      column_props = column_props,
      PCA_scaling_mean = reactiveVal(),
      PCA_scaling_sd = reactiveVal(),
      PCA_dataset = PCA_dataset(),
      PCA_summary_df = PCA_summary_df(),
      PCA_coefficients = PCA_coefficients(),
      pca_axes_max = pca_axes_max(),
      pca_axes = pca_axes(),
      pcax_being_used = pcax_being_used(),
      fs_pcax_used = fs_pcax_used(),
      final_model_PCA = final_model_PCA(),
      
      LG_pred_results = LG_pred_results(),
      LG_pred_coeffs = LG_pred_coeffs(),
      LG_pred_confuse_results = LG_pred_confuse_results(),
      LG_pred_scat_dat = LG_pred_scat_dat(),
      LG_pred_standardize = LG_pred_standardize(),
      LG_results = LG_results(),
      LG_coeffs = LG_coeffs(),
      LG_confuse_results = LG_confuse_results(),
      LG_scat_dat = LG_scat_dat(),
      LG_model = LG_model,
      LG_standardize = LG_pred_standardize(),
      LG_model_PCA = LG_model_PCA(),
      
      XGBCL_pred_results = XGBCL_pred_results(),
      XGBCL_pred_coeffs = XGBCL_pred_coeffs(),
      XGBCL_pred_confuse_results = XGBCL_pred_confuse_results(),
      XGBCL_pred_scat_dat = XGBCL_pred_scat_dat(),
      XGBCL_pred_standardize = XGBCL_pred_standardize(),
      XGBCL_selection_results = XGBCL_selection_results(),
      XGBCL_results = XGBCL_results(),
      XGBCL_coeffs = XGBCL_coeffs(),
      XGBCL_confuse_results = XGBCL_confuse_results(),
      XGBCL_scat_dat = XGBCL_scat_dat(),
      XGBCL_model = XGBCL_model,
      XGBCL_standardize = XGBCL_standardize(),
      XGBCL_model_PCA = XGBCL_model_PCA(),
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
      XGB_scat_dat = XGB_scat_dat(),
      XGB_model = XGB_model,
      XGB_standardize = XGB_standardize(),
      XGB_model_PCA = XGB_model_PCA(),
      Optimal_HP = Optimal_HP,
      
      EN_pred_results = EN_pred_results(),
      EN_pred_coeffs = EN_pred_coeffs(),
      EN_pred_confuse_results = EN_pred_confuse_results(),
      EN_pred_scat_dat = EN_pred_scat_dat(),
      EN_pred_standardize = EN_pred_standardize(),
      EN_results = EN_results(),
      EN_coeffs = EN_coeffs(),
      EN_confuse_results = EN_confuse_results(),
      EN_scat_dat = EN_scat_dat(),
      EN_model = EN_model,
      EN_standardize = EN_standardize(),
      EN_model_PCA = EN_model_PCA()
    )
    
    save(save_list, file = file)
  })

  # Load saved Workspace
  observeEvent(c(input$load_project,input$load_project2,input$load_prediction), ignoreInit = T, {

    req(!is.null(input$load_project) | !is.null(input$load_project2) | !is.null(input$load_prediction))
    
    if (!is.null(input$load_project)) {
       load_element = input$load_project
    } else if (!is.null(input$load_project2)) {
      load_element = input$load_project2
    } else {
      load_element = input$load_prediction
    }
    
    temp_env = new.env()
    load(load_element$datapath, envir = temp_env)
    
    if (temp_env$save_list$Version == "1.0.0" && exists("save_list", envir = temp_env)) {

      temp_db <<- temp_env$save_list$temp_db
      bo(temp_env$save_list$bo)
      current_data(temp_env$save_list$current_data)
      response_var(temp_env$save_list$response_var)
      col_names(temp_env$save_list$col_names)
      feat_names(temp_env$save_list$feat_names)
      feats_being_used(temp_env$save_list$feats_being_used)
      fs_feats_used(temp_env$save_list$fs_feats_used)
      init_data <<- temp_env$save_list$init_data
      ignored_rows <<- temp_env$save_list$ignored_rows
      date_format_string <<- temp_env$save_list$date_format_string
      saved_lc_val = temp_env$save_list$saved_lc_val
      saved_rc_val = temp_env$save_list$saved_rc_val
      saved_num_axes = temp_env$save_list$saved_num_axes
      init_column_props <<- temp_env$save_list$init_column_props
      column_props <<- temp_env$save_list$column_props
      PCA_scaling_mean(temp_env$save_list$PCA_scaling_mean)
      PCA_scaling_sd(temp_env$save_list$PCA_scaling_sd)
      PCA_dataset(temp_env$save_list$PCA_dataset)
      PCA_summary_df(temp_env$save_list$PCA_summary_df)
      PCA_coefficients(temp_env$save_list$PCA_coefficients)
      pca_axes_max(temp_env$save_list$pca_axes_max)
      pca_axes(temp_env$save_list$pca_axes)
      pcax_being_used(temp_env$save_list$pcax_being_used)
      fs_pcax_used(temp_env$save_list$fs_pcax_used)
      final_model_PCA(temp_env$save_list$final_model_PCA)
      
      LG_pred_results(temp_env$save_list$LG_pred_results)
      LG_pred_coeffs(temp_env$save_list$LG_pred_coeffs)
      LG_pred_confuse_results(temp_env$save_list$LG_pred_confuse_results)
      LG_pred_scat_dat(temp_env$save_list$LG_pred_scat_dat)
      LG_pred_standardize(temp_env$save_list$LG_pred_standardize)
      LG_results(temp_env$save_list$LG_results)
      LG_coeffs(temp_env$save_list$LG_coeffs)
      LG_confuse_results(temp_env$save_list$LG_confuse_results)
      LG_scat_dat(temp_env$save_list$LG_scat_dat)
      LG_model <<- temp_env$save_list$LG_model
      LG_standardize(temp_env$save_list$LG_standardize)
      LG_model_PCA(temp_env$save_list$LG_model_PCA)
      
      XGBCL_pred_results(temp_env$save_list$XGBCL_pred_results)
      XGBCL_pred_coeffs(temp_env$save_list$XGBCL_pred_coeffs)
      XGBCL_pred_confuse_results(temp_env$save_list$XGBCL_pred_confuse_results)
      XGBCL_pred_scat_dat(temp_env$save_list$XGBCL_pred_scat_dat)
      XGBCL_pred_standardize(temp_env$save_list$XGBCL_pred_standardize)
      XGBCL_selection_results(temp_env$save_list$XGBCL_selection_results)
      XGBCL_results(temp_env$save_list$XGBCL_results)
      XGBCL_coeffs(temp_env$save_list$XGBCL_coeffs)
      XGBCL_confuse_results(temp_env$save_list$XGBCL_confuse_results)
      XGBCL_scat_dat(temp_env$save_list$XGBCL_scat_dat)
      XGBCL_model <<- temp_env$save_list$XGBCL_model
      XGBCL_standardize(temp_env$save_list$XGBCL_standardize)
      XGBCL_model_PCA(temp_env$save_list$XGBCL_model_PCA)
      Optimal_CLHP <<- temp_env$save_list$Optimal_CLHP
      
      XGB_pred_results(temp_env$save_list$XGB_pred_results)
      XGB_pred_coeffs(temp_env$save_list$XGB_pred_coeffs)
      XGB_pred_confuse_results(temp_env$save_list$XGB_pred_confuse_results)
      XGB_pred_scat_dat(temp_env$save_list$XGB_pred_scat_dat)
      XGB_pred_standardize(temp_env$save_list$XGB_pred_standardize)
      XGB_selection_results(temp_env$save_list$XGB_selection_results)
      XGB_results(temp_env$save_list$XGB_results)
      XGB_coeffs(temp_env$save_list$XGB_coeffs)
      XGB_confuse_results(temp_env$save_list$XGB_confuse_results)
      XGB_scat_dat(temp_env$save_list$XGB_scat_dat)
      XGB_model <<- temp_env$save_list$XGB_model
      XGB_standardize(temp_env$save_list$XGB_standardize)
      XGB_model_PCA(temp_env$save_list$XGB_model_PCA)
      Optimal_HP <<- temp_env$save_list$Optimal_HP
      
      EN_pred_results(temp_env$save_list$EN_pred_results)
      EN_pred_coeffs(temp_env$save_list$EN_pred_coeffs)
      EN_pred_confuse_results(temp_env$save_list$EN_pred_confuse_results)
      EN_pred_scat_dat(temp_env$save_list$EN_pred_scat_dat)
      EN_pred_standardize(temp_env$save_list$EN_pred_standardize)
      EN_results(temp_env$save_list$EN_results)
      EN_coeffs(temp_env$save_list$EN_coeffs)
      EN_confuse_results(temp_env$save_list$EN_confuse_results)
      EN_scat_dat(temp_env$save_list$EN_scat_dat)
      EN_model <<- temp_env$save_list$EN_model
      EN_model_PCA(temp_env$save_list$EN_model_PCA)
      EN_standardize(temp_env$save_list$EN_standardize)
      
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
          output$pca_model_text = renderText({HTML("NOTE: PCA features being used.")})
      } else {
          output$pca_model_text = NULL
      }
      
      refresh_trigger(TRUE)
      
      if (temp_env$save_list$type == "Project") {
        
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
          enable("pca_check")
          enable("run_iso_forest")
          
          updateNumericInput(session, "num_axes",
                             value = pca_axes_max(),
                             max = pca_axes_max()
          )
          
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
        
      } else if (temp_env$save_list$type == "Prediction") {
        
        session$sendCustomMessage(type = 'disableTabs', message = list(action = 'disable'))
        updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Prediction')
      }
      
    } else if (temp_env$save_list$Version != "1.0.0") {
      
      showModal(modalDialog(paste("This project file is not compatible with this version of ShinyVB."),
                            easyClose = F,footer = div(modalButton('Close'))))
      
    } else if (!exists("save_list", envir = temp_env)) {
      
      showModal(modalDialog(paste("No saved data in this project file."),
                            easyClose = F,footer = div(modalButton('Close'))))
    }
  })
  
  # Create a feature correlation matrix
  observeEvent(input$corr_check, ignoreInit = T, {
    
    showModal(modalDialog(title="Choose Features", card(
    
      checkboxGroupButtons(
        inputId = "feats_to_corr",
        label = "Features to Use: ",
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
  
  # Create PCA dataset for later analysis
  observeEvent(input$pca_check, ignoreInit = T, {
    
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
  observeEvent(input$data_cell_edit, ignoreInit = T, ignoreNULL = T, {
    
    info = input$data_cell_edit
    temp_data=current_data()

    i = info$row
    j = info$col + 1
    
    temp_data = editData(temp_data, input$data_cell_edit, "data", rownames = FALSE)
    
    current_data(temp_data)
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
  })
  
  # Change the response variable
  observeEvent(input$data_columns_selected, ignoreInit = T, {
    
    if (input$select_choice == "Change_Response") {
      
      if ((input$data_columns_selected+1) != response_var() && input$data_columns_selected != 0) {
        
        response_var(input$data_columns_selected + 1)
        
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
        
        updateNumericInput(session, "lc_replace",value = round(min(real_responses),3))
        updateNumericInput(session, "rc_replace",value = round(max(real_responses),3))
        
        renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
        
        pca_axes_max(ncol(init_data)-2)
        pca_axes(NULL)
        pcax_being_used(NULL)
        PCA_dataset(NULL)
        
        clear_modeling(TRUE)
        changed_model(TRUE)
        
        updateNumericInput(session, "num_axes",
                           value = pca_axes_max(),
                           max = pca_axes_max())
      }
    } else {
      return()
    }
  })
  
  # Enabling/disabling rows
  observeEvent(input$ignore_rows, ignoreInit = T, {
    
    if (input$data_tabs == "Data Table") {
      add_in = input$data_rows_selected[-1]
    }
    if (input$data_tabs == "IsoForest Outliers") {
      add_in = input$iso_outliers_rows_selected
    }
    
    if(identical(unique(append(ignored_rows,add_in)),integer(0))) {
      ignored_rows <<- NULL
    } else {
      ignored_rows <<- unique(append(ignored_rows,add_in))
    }
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
  })
  
  observeEvent(input$enable_rows, ignoreInit = T, {
    
    if (input$data_tabs == "Data Table") {
      add_back = input$data_rows_selected[-1]
    }
    if (input$data_tabs == "IsoForest Outliers") {
      add_back = input$iso_outliers_rows_selected
    }
    
    if(identical(ignored_rows[!ignored_rows %in% add_back],integer(0))) {
      ignored_rows <<- NULL
    } else {
      ignored_rows <<- ignored_rows[!ignored_rows %in% add_back]
    }
    
    renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
  })
  
  # Restore the original dataset
  observeEvent(input$restore, ignoreInit = T, {
    
    response_var(2)
    
    temp_data = init_data[,-1]
    temp_data = temp_data[,-1]
    
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
    
    updateNumericInput(session, "num_axes",
                       value = pca_axes_max(),
                       max = pca_axes_max()
    )

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
      
      # Logistic Regression Results
      LG_results(NULL)
      LG_coeffs(NULL)
      LG_confuse_results(NULL)
      LG_scat_dat(NULL)
      LG_model <<- NULL
      
      # XGBoost Classifier Prediction Results
      XGBCL_pred_results(NULL)
      XGBCL_pred_coeffs(NULL)
      XGBCL_pred_confuse_results(NULL)
      XGBCL_pred_scat_dat(NULL)
      
      # XGBoost Classifier Other Results
      XGBCL_selection_results(NULL)
      
      # XGBoost Classifier Results
      XGBCL_results(NULL)
      XGBCL_coeffs(NULL)
      XGBCL_confuse_results(NULL)
      XGBCL_scat_dat(NULL)
      XGBCL_model <<- NULL
      
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
        init_data[,1] = ymd(init_data[,1])
        date_format_string <<- "toLocaleDateString"
      } else if (input$IDasDate == "MDY") {
        init_data[,1] = mdy(init_data[,1])
        date_format_string <<- "toLocaleDateString"
      } else if (input$IDasDate == "MDYHM") {
        init_data[,1] = parse_date_time(init_data[,1],c('%m/%d/%y %H:%M'),exact=TRUE)
        date_format_string <<- "toLocaleString"
      } else if (input$IDasDate == "Character") {
        date_format_string <<- "Character"
      } else if (input$IDasDate == "Numeric") {
        date_format_string <<- "Numeric"
      }
      
      current_data(init_data)
      col_names(colnames(init_data[,-1]))
      
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
      
      feat_data0 = init_data[,-1]
      feat_data = feat_data0[,-1]
      
      feats_being_used(colnames(feat_data))
      feat_names(colnames(feat_data))
      
      enable("restore")
      enable("set_column_props")
      enable("corr_check")
      enable("pca_check")
      enable("run_iso_forest")
      
      pca_axes_max(ncol(init_data)-2)
      
      updateNumericInput(session, "num_axes",
                         value = pca_axes_max(),
                         max = pca_axes_max()
      )
      
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

  # Isolation Forest analysis for outlier detection
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
    
    output$iso_outliers = DT::renderDataTable(server = T, {data = datatable(iso_results,rownames = F,selection = list(selection = "multiple",
                selected = list(rows = NULL),target = "row"),editable = F,extensions="Buttons",options = list(paging = TRUE,dom="ltBp",
                buttons = c('copy', 'csv', 'excel'),pageLength = num_rows_per_page,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),
                initComplete =JS("function(settings, json) {","$(this.api().table().header()).css({'background-color':'#073744', 'color': '#fff'});","}")))})
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = 'IsoForest Outliers')
    
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
  
  # Create wind/wave/current A/O components
  observeEvent(input$create, ignoreInit = T, {
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Data')
    updateTabsetPanel(session, inputId = 'data_tabs', selected = 'Data Table')
    
    if (input$speed != "-" & input$direct != "-") {
      
      column_names = colnames(current_data())
      
      if (!(input$A_name %in% column_names) & !(input$O_name %in% column_names)) {
        
        Aname = input$A_name
        Oname = input$O_name
        
        speed_dat = current_data()[,input$speed]
        dir_dat = current_data()[,input$direct]
        
        A_comp = -speed_dat*cos((dir_dat - bo())*pi/180)
        O_comp = speed_dat*sin((dir_dat - bo())*pi/180)
        
        new_data = cbind(current_data(),A_comp,O_comp)
        colnames(new_data) = c(column_names,Aname,Oname)
        
        for (i in (ncol(new_data)-1):ncol(new_data)) {
          .set(column_props,keys=colnames(new_data)[i],values=2)
          #.set(column_props,keys=colnames(new_data)[i],values=c(prop1=2,prop2=NA,prop3=NA,prop4=NA))
        }
        
        current_data(new_data)
        
        rv = response_var()-1
        
        new_column_names = colnames(current_data())[-1]
        feat_names(new_column_names[-rv])
        feats_being_used(feat_names())
        
        # updateSelectInput(session,"id",choices=c(col_names()))
        updateSelectInput(session,"set_column_props",choices=c("-",new_column_names))
        updateSelectInput(session,"rainplot",choices=c("-",new_column_names))
        updateSelectInput(session,"lineplot",choices=c("-",new_column_names))
        updateSelectInput(session,"scatterx",selected=input$scatterx,choices=c("-",new_column_names))
        updateSelectInput(session,"scattery",selected=input$scattery,choices=c("-",new_column_names))

        pca_axes_max(ncol(current_data())-2)
        PCA_dataset(NULL)
        changed_model(TRUE)
        
        updateNumericInput(session, "num_axes",
                           value = pca_axes_max(),
                           max = pca_axes_max()
        )

        clear_modeling(TRUE)
        
        renderdata(current_data(),response_var(),id_var,input$select_choice,date_format_string,column_props,ignored_rows,current_data_page(),output)
        
      } else {
        showModal(modalDialog(div("ERROR: BOTH new component columns must have different names 
                                  than any currently existing column names.",style="font-size:160%"),easyClose = T))
      }
      
    } else {
      showModal(modalDialog(div("ERROR: A speed and direction data column must be specified.",style="font-size:160%"),easyClose = T))
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
  
  # LGR predictions
  observeEvent(input$LG_pred_dc, ignoreInit = T, {
    
    if (nrow(LG_pred_scat_dat()) != 0) {
      refresh_trigger(TRUE)
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
      
      LG_pred_confuse_results(confuse(LG_pred_scat_dat()[,2:3],0.5,input$LG_pred_dc))
      
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
                                             geom_vline(xintercept = input$LG_pred_dc, linetype = "dashed", color = "darkgreen") +
                                             labs(x = "Predicted Probability", y = "Density", fill="OBS") +
                                             theme_bw() +
                                             theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                             theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                             theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      LG_pred_confuse_results(confuse(LG_pred_scat_dat()[,2:3],0.5,input$LG_pred_dc))
      
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = LG_pred_confuse_results()$TP
      confuse_table[1,2] = LG_pred_confuse_results()$TN
      confuse_table[1,3] = LG_pred_confuse_results()$FP
      confuse_table[1,4] = LG_pred_confuse_results()$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$LG_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_pred_confuse_text = renderText({paste0("Sensitivity = ",round(LG_pred_confuse_results()$Sensitivity,3),"; Specificity = ",
                  round(LG_pred_confuse_results()$Specificity,3),"; Accuracy = ",round(LG_pred_confuse_results()$Accuracy,3))})
      
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
  
  # LGR fitting
  observeEvent(input$LG_fit_dc, ignoreInit = T, {
    
    if (nrow(LG_scat_dat()) != 0) {
      refresh_trigger(TRUE)
    }
  })
  
  observeEvent(input$run_fitted_LG, ignoreInit = T, {
    
    response_data = as.numeric(current_data()[,response_var()])
    response_data = response_data[!is.na(response_data)]
    
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
        ignored_rows = NULL
        LG_model_PCA(TRUE)
      } else {
        data0 = current_data()
        rv=response_var()
        feats_to_use = input$feats_to_use
        LG_model_PCA(FALSE)
      }
      
      set.seed(input$model_seed)
      crit_value = input$LG_binarize_crit_value
      LG_thresh(crit_value)
      MC_runs = input$MC_runs
      
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
      
      temp_fits = matrix(0, nrow = nrow(imp_data), ncol = 2*MC_runs)
      temp_fits = data.frame(temp_fits)
      
      temp_coeffs = matrix(0, nrow = ncol(imp_data), ncol = MC_runs+1)
      temp_coeffs = data.frame(temp_coeffs)
      temp_coeffs[,1] = c("(Intercept)",feats_to_use)
      
      withProgress(
        message = 'Logistic Fitting Progress',
        detail = paste("MC runs:", x=1,"/",MC_runs),
        value = 0,
        {
          
          for (i in 1:MC_runs) {
            
            MC_data = MC_subbin(imp_data, input$loggy, input$lc_val, input$lc_lowval, input$lc_upval, input$rc_val,input$rc_lowval, input$rc_upval)

            # Binarize Response
            
            if (input$LG_binarize) {
              for (j in 1:nrow(MC_data)) {
                MC_data[j, 1] = ifelse(test = MC_data[j, 1] >= crit_value, yes = 1, no = 0)
              }
            }
            
            temp_fits[,2*i-1] = MC_data[,1]
            
            # determine best alpha and lambda
            fit_mod = cva.glmnet(x=as.matrix(MC_data[,-1]),y=MC_data[,1],nfolds=input$num_folds,family="binomial",type.measure=input$LG_eval,na.action="na.omit",
                                 standardize=input$LG_standard,intercept=TRUE)
            
            alpha = get_model_params(fit_mod)$alpha
            lambda = get_model_params(fit_mod)$lambdaMin
            
            # fit training model
            model = glmnet(x=as.matrix(MC_data[,-1]),MC_data[,1],lambda=lambda, alpha=alpha, na.action="na.omit",family="binomial",type.measure=input$LG_eval,
                           standardize=input$LG_standard,intercept=TRUE)
            
            fits = predict(model, newx = as.matrix(MC_data[,-1]), type = "response")
            temp_fits[,2*i] = round(fits,3)

            tmp_coeffs = coef(model, s = lambda)
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

      fitted_coeffs[,2] = round(rowMeans(temp_coeffs[,-1]),4)
      fitted_coeffs[,3] = round(exp(fitted_coeffs[,2]),4)
      
      even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
      odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
      
      obs_mean_values = ifelse(test = rowMeans(odd_columns) >= 0.5, yes = 1, no = 0)
      fit_mean_values = round(rowMeans(even_columns),3)
      model_results = cbind(obs_mean_values,fit_mean_values)
      
      if (input$use_pca_data) {
        fitted_model_results = data.frame(cbind(data1[,1],model_results[,1],model_results[,2],round(data[,-1],4)))
      } else {
        fitted_model_results = data.frame(cbind(data1[,1],model_results[,1],model_results[,2],data[,-1]))
      }
      
      colnames(fitted_model_results) = c(colnames(data0)[1],colnames(data0)[rv],"Fitted_Prob",feats_to_use)
      
      LG_results(fitted_model_results[order(fitted_model_results[,1]),])
      
      colnames(fitted_coeffs) = c("Feature","Coefficient","Odds Ratio")
      
      LG_coeffs(fitted_coeffs)
      
      LG_scat_dat(LG_results()[,1:3])
      
      LG_confuse_results(confuse(LG_scat_dat()[,2:3],0.5,input$LG_fit_dc))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'LG: Fitting')
      
      # Fit final model
      final_LG_data = MC_final_subbin(imp_data,input$loggy,input$lc_val,input$rc_val,0.5,1.5)

      # Binarize Response
      if (input$LG_binarize) {
        for (j in 1:nrow(final_LG_data)) {
          final_LG_data[j, 1] = ifelse(test = final_LG_data[j, 1] >= crit_value, yes = 1, no = 0)
        }
      }
      
      optimized_LG_model = cva.glmnet(x=as.matrix(final_LG_data[,-1]),y=final_LG_data[,1],nfolds=input$num_folds,family="binomial",type.measure=input$LG_eval,
                                 na.action="na.omit",standardize=input$LG_standard,intercept=TRUE)
      
      final_alpha = get_model_params(optimized_LG_model)$alpha
      final_lambda = get_model_params(optimized_LG_model)$lambdaMin
      
      # Compute and save final LG model
      LG_model <<- glmnet(x=as.matrix(final_LG_data[,-1]),final_LG_data[,1],lambda=final_lambda, alpha=final_alpha, na.action="na.omit",family="binomial",type.measure=input$LG_eval,
                          standardize=input$LG_standard,intercept=TRUE)
    }
    
    LG_standardize(input$LG_standard)
    
    updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
    
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
                                        geom_vline(xintercept = input$LG_fit_dc, linetype = "dashed", color = "darkgreen") +
                                        labs(x = "Fitted Probability", y = "Density", fill="OBS") +
                                        theme_bw() +
                                        theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                        theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                        theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      

      LG_confuse_results(confuse(LG_scat_dat()[,2:3],0.5,input$LG_fit_dc))
      
      confuse_table = matrix(0,nrow=1,ncol=4)
      
      confuse_table[1,1] = LG_confuse_results()$TP
      confuse_table[1,2] = LG_confuse_results()$TN
      confuse_table[1,3] = LG_confuse_results()$FP
      confuse_table[1,4] = LG_confuse_results()$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$LG_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$LG_confuse_text = renderText({paste0("Sensitivity = ",round(LG_confuse_results()$Sensitivity,3),"; Specificity = ",
                  round(LG_confuse_results()$Specificity,3),"; Accuracy = ",round(LG_confuse_results()$Accuracy,3))})
      
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
  observeEvent(input$XGBCL_pred_dc, ignoreInit = T, {
    
    if (nrow(XGBCL_pred_scat_dat()) != 0) {
      refresh_trigger(TRUE)
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
      
      XGBCL_pred_confuse_results(confuse(XGBCL_pred_scat_dat()[,2:3],0.5,input$XGBCL_pred_dc))
      
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
                      geom_vline(xintercept = input$XGBCL_pred_dc, linetype = "dashed", color = "darkgreen") +
                      labs(x = "Predicted Probability", y = "Density", fill="OBS") +
                      theme_bw() +
                      theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                      theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                      theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
      XGBCL_pred_confuse_results(confuse(XGBCL_pred_scat_dat()[,2:3],0.5,input$XGBCL_pred_dc))
      
      confuse_table = data.frame(matrix(0,nrow=1,ncol=4))
      
      confuse_table[1,1] = XGBCL_pred_confuse_results()$TP
      confuse_table[1,2] = XGBCL_pred_confuse_results()$TN
      confuse_table[1,3] = XGBCL_pred_confuse_results()$FP
      confuse_table[1,4] = XGBCL_pred_confuse_results()$FN
      
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
      output$XGBCL_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
      output$XGBCL_pred_confuse_text = renderText({paste0("Sensitivity = ",round(XGBCL_pred_confuse_results()$Sensitivity,3),"; Specificity = ",
                      round(XGBCL_pred_confuse_results()$Specificity,3),"; Accuracy = ",round(XGBCL_pred_confuse_results()$Accuracy,3))})
      
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
  observeEvent(input$XGBCL_dec_crit, ignoreInit = T, {
    
    if (nrow(XGBCL_scat_dat()) != 0) {
      refresh_trigger(TRUE)
    }
  })
  
  observeEvent(input$run_fit_XGBCL, ignoreInit = T, {
    
    response_data = as.numeric(current_data()[,response_var()])
    response_data = response_data[!is.na(response_data)]
    
    if (length(unique(response_data)) > 2 && input$XGBCL_binarize == FALSE) {
      
      showModal(modalDialog(paste("Response Variable must be binarized for this analysis. Choose to 'Binarize' using the button above."),footer = modalButton("Close")))
      
    } else {
      
      req(iv$is_valid())
      
      updateNumericInput(session, "num_preds",value = 2)
      changed_model(TRUE)
      
      if (input$use_pca_data) {
        data0 = PCA_dataset()
        rv=2
        feats_to_use = input$pcax_to_use
        ignored_rows = NULL
        XGBCL_model_PCA(TRUE)
      } else {
        data0 = current_data()
        rv=response_var()
        feats_to_use = input$feats_to_use
        XGBCL_model_PCA(FALSE)
      }
      
      crit_value = input$XGBCL_binarize_crit_value
      XGBCL_thresh(crit_value)
      MC_runs = input$MC_runs
      
      data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,input$XGBCL_standard)
      data = data1[,-1]

      withProgress(
        message = 'XGBCL Fitting Progress',
        detail = paste("MC runs:", x = 1,"/",MC_runs),
        value = 0,
        {
          
          temp_fits = matrix(0, nrow = nrow(data), ncol = 2*MC_runs)
          temp_fits = data.frame(temp_fits)
          
          temp_shapes = matrix(0, nrow = length(feats_to_use), ncol = MC_runs+1)
          temp_shapes = data.frame(temp_shapes)
          temp_shapes[,1] = feats_to_use
          
          for (i in 1:MC_runs) {
            
            MC_data = MC_subbin(data, input$loggy, input$lc_val, input$lc_lowval, input$lc_upval, input$rc_val,input$rc_lowval, input$rc_upval)
            
            if (input$XGBCL_binarize) {
              for (j in 1:nrow(MC_data)) {
                MC_data[j, 1] = ifelse(test = MC_data[j, 1] >= crit_value, yes = 1, no = 0)
              }
            }
            
            temp_fits[,2*i-1] = MC_data[,1]
            
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
            
            xgbcl_model = xgboost(data = as.matrix(MC_data[,-1]),label=MC_data[,1], params=params, early_stopping_rounds=early_stop_set(), nrounds=1000, verbose=0)
            
            fits = predict(xgbcl_model, newdata=as.matrix(MC_data[,-1]))
            temp_fits[,2*i] = fits
            
            if (ncol(MC_data) > 2) {
              shap_values = shap.values(xgb_model = xgbcl_model, X_train = as.matrix(MC_data[,-1]))
              mean_shaps = round(shap_values$mean_shap_score,4)
              shap_names = names(mean_shaps)
              shap = data.frame(cbind(shap_names,mean_shaps))
            } else {
              shap = data.frame("Feature" = colnames(MC_data)[[2]], "mean_shaps" = 0)
            }
            
            for (c in 1:nrow(temp_shapes)) {
              current_feat = temp_shapes[c,1]
              temp_shapes[c,i+1] = as.numeric(shap[which(shap[,1] == current_feat),2])
            }
            
            incProgress(1/MC_runs, detail = paste("MC run: ",i,"/",MC_runs))
          }
        })

      even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
      odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
      
      obs_mean_values = ifelse(test = rowMeans(odd_columns) >= 0.5, yes = 1, no = 0)
      fit_mean_values = rowMeans(even_columns)
      fits = cbind(obs_mean_values,round(fit_mean_values,3))
      
      if (input$use_pca_data) {
        xgbcl_results = data.frame(cbind(data1[,1],fits[,1],fits[,2],round(data[,-1],4)))
      } else {
        xgbcl_results = data.frame(cbind(data1[,1],fits[,1],fits[,2],data[,-1]))
      }
      
      colnames(xgbcl_results) = c(colnames(data0)[[1]],colnames(data0)[[rv]],"Fitted_Prob",colnames(data[,-1]))
      
      xgbcl_results = xgbcl_results[order(xgbcl_results[,1]),]
      XGBCL_results(xgbcl_results)
      
      xgbcl_shapes1 = as.data.frame(temp_shapes[,-1])
      xgbcl_shapes2 = rowMeans(xgbcl_shapes1)
      xgbcl_shapes = data.frame(cbind(temp_shapes[,1],xgbcl_shapes2))
      xgbcl_shapes = xgbcl_shapes[order(xgbcl_shapes[,2],decreasing = T),]
      colnames(xgbcl_shapes) = c("Feature","Mean_SHAP")
      
      XGBCL_coeffs(xgbcl_shapes)
      
      XGBCL_scat_dat(XGBCL_results()[,1:3])
      
      XGBCL_confuse_results(confuse(XGBCL_scat_dat()[,2:3],0.5,input$XGBCL_dec_crit))
      
      updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
      updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGBCL: Fitting')
      
      # Fit final XGBCL_Model
      final_XGBCL_data = MC_final_subbin(data,input$loggy,input$lc_val,input$rc_val,0.5,1.5)

      if (input$XGBCL_binarize) {
        for (j in 1:nrow(final_XGBCL_data)) {
          final_XGBCL_data[j, 1] = ifelse(test = final_XGBCL_data[j, 1] >= crit_value, yes = 1, no = 0)
        }
      }
      
      XGBCL_final_data(final_XGBCL_data)
      
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
      
      XGBCL_model <<- xgboost(data = as.matrix(final_XGBCL_data[,-1]),label=final_XGBCL_data[,1], params=params, early_stopping_rounds=early_stop_set(),nrounds=2000, verbose=0)
      
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
                                           geom_vline(xintercept = input$XGBCL_dec_crit, linetype = "dashed", color = "darkgreen") +
                                           labs(x = "Fitted Probability", y = "Density", fill="OBS") +
                                           theme_bw() +
                                           theme(panel.grid.minor = element_line(linewidth = 0.1), panel.grid.major = element_line(linewidth = 0.1)) +
                                           theme(axis.text=element_text(size=16, face="bold"),axis.title=element_text(size=20,face="bold")) +
                                           theme(legend.position.inside = c(0.75, 0.9),legend.text = element_text(size=14),legend.title=element_text(size=16)))
      
        XGBCL_confuse_results(confuse(XGBCL_scat_dat()[,2:3],0.5,input$XGBCL_dec_crit))
        
        xgbcl_confuse_table = matrix(0,nrow=1,ncol=4)
      
        xgbcl_confuse_table[1,1] = XGBCL_confuse_results()$TP
        xgbcl_confuse_table[1,2] = XGBCL_confuse_results()$TN
        xgbcl_confuse_table[1,3] = XGBCL_confuse_results()$FP
        xgbcl_confuse_table[1,4] = XGBCL_confuse_results()$FN
      
        colnames(xgbcl_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
      
        output$XGBCL_confuse = DT::renderDataTable(server = T, {data = datatable(xgbcl_confuse_table,rownames = F,selection = 
                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                    options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                    columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                    {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
      
        output$XGBCL_confuse_text = renderText({paste0("Sensitivity = ",round(XGBCL_confuse_results()$Sensitivity,3),"; Specificity = ",
                    round(XGBCL_confuse_results()$Specificity,3),"; Accuracy = ",round(XGBCL_confuse_results()$Accuracy,3))})
      
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
  observeEvent(input$XGB_pred_stand, ignoreInit = T, {
    
    if (nrow(XGB_pred_scat_dat()) != 0) {
      
      iv$add_rule("XGB_pred_stand", sv_between(min(XGB_pred_scat_dat()[,2]),max(XGB_pred_scat_dat()[,2])))
      
      refresh_trigger(TRUE)
    }
  })
  
  observeEvent(input$XGB_pred_dc, ignoreInit = T, {
    
    if (nrow(XGB_pred_scat_dat()) != 0) {
      
      iv$add_rule("XGB_pred_dc", sv_between(min(XGB_pred_scat_dat()[,3]),max(XGB_pred_scat_dat()[,3])))
      refresh_trigger(TRUE)
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
    
      xgb_pred_stepr = round((max(XGB_pred_results()[,2])-min(XGB_pred_results()[,2]))/40,2)
    
      updateNumericInput(session, "XGB_pred_stand",
                       value = round(mean(XGB_pred_results()[,2]),2),
                       max = round(max(XGB_pred_results()[,2]),2),
                       min = round(min(XGB_pred_results()[,2]),2),
                       step = xgb_pred_stepr
      )
    
      xgb_pred_stepdc = round((max(XGB_pred_results()[,3])-min(XGB_pred_results()[,3]))/40,2)
    
      updateNumericInput(session, "XGB_pred_dc",
                       value = round(mean(XGB_pred_results()[,3]),2),
                       max = round(max(XGB_pred_results()[,3]),2),
                       min = round(min(XGB_pred_results()[,3]),2),
                       step = xgb_pred_stepdc
      )
    
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
    
      output$XGB_pred_scatplot = renderPlotly(scatter_confuse(XGB_pred_scat_dat(),input$XGB_pred_stand,input$XGB_pred_dc))
      
      XGB_confuse_results(confuse(XGB_pred_scat_dat()[,2:3],input$XGB_pred_stand,input$XGB_pred_dc))
    
      confuse_table = matrix(0,nrow=1,ncol=4)
    
      confuse_table[1,1] = XGB_confuse_results()$TP
      confuse_table[1,2] = XGB_confuse_results()$TN
      confuse_table[1,3] = XGB_confuse_results()$FP
      confuse_table[1,4] = XGB_confuse_results()$FN
    
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
      output$XGB_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
      output$XGB_pred_confuse_text = renderText({paste0("Sensitivity = ",round(XGB_confuse_results()$Sensitivity,3),"; Specificity = ",
                      round(XGB_confuse_results()$Specificity,3),"; Accuracy = ",round(XGB_confuse_results()$Accuracy,3))})
    

      resid_data = XGB_pred_scat_dat()[,c(1,3)] %>% mutate(Residuals = round(XGB_pred_scat_dat()[,2]-XGB_pred_scat_dat()[,3],3))
      output$XGB_pred_resid_scatplot = renderPlotly(scatter(resid_data))
    
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
  observeEvent(input$XGB_stand, ignoreInit = T, {
    
    if (nrow(XGB_scat_dat()) != 0) {
      
      iv$add_rule("XGB_stand", sv_between(min(XGB_scat_dat()[,2]),max(XGB_scat_dat()[,2])))
      
      refresh_trigger(TRUE)
    }
  })
  
  observeEvent(input$XGB_dec_crit, ignoreInit = T, {
    
    if (nrow(XGB_scat_dat()) != 0) {
      
      iv$add_rule("XGB_dec_crit", sv_between(min(XGB_scat_dat()[,3]),max(XGB_scat_dat()[,3])))
      
      refresh_trigger(TRUE)
    }
  })
  
  observeEvent(input$XGB_final_fitting, ignoreInit = T, {
    
    req(iv$is_valid())
    
    updateNumericInput(session, "num_preds",value = 2)
    changed_model(TRUE)
    
    if (input$use_pca_data) {
      data0 = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
      XGB_model_PCA(TRUE)
    } else {
      data0 = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
      XGB_model_PCA(FALSE)
    }
    
    data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,input$XGB_standard)
    data = data1[,-1]
    
    MC_runs = input$MC_runs
    
    temp_fits = matrix(0, nrow = nrow(data), ncol = 2*MC_runs)
    temp_fits = data.frame(temp_fits)
    
    temp_shapes = matrix(0, nrow = length(feats_to_use), ncol = MC_runs+1)
    temp_shapes = data.frame(temp_shapes)
    temp_shapes[,1] = feats_to_use
    
    withProgress(
      message = 'XGB Fitting Progress',
      detail = paste("MC runs:", x = 1,"/",MC_runs),
      value = 0,
      {
        
        for (i in 1:MC_runs) {
          
          MC_data = MC_subbin(data,input$loggy,input$lc_val,input$lc_lowval,input$lc_upval,input$rc_val,input$rc_lowval,input$rc_upval)
          
          temp_fits[,2*i-1] = MC_data[,1]
          
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
          
          xgb_model = xgboost(data = as.matrix(MC_data[,-1]),label=MC_data[,1], params=params, early_stopping_rounds=early_stop_set(), nrounds=1000, verbose=0)
          
          temp_fits[,2*i] = predict(xgb_model, newdata=as.matrix(MC_data[,-1]))
          
          if (ncol(MC_data) > 2) {
            
            shap_values = shap.values(xgb_model = xgb_model, X_train = as.matrix(MC_data[,-1]))
            mean_shaps = shap_values$mean_shap_score
            shap_names = names(mean_shaps)
            shap_temp = data.frame(cbind(shap_names,mean_shaps))
          } else {
            shap_temp = data.frame("Feature" = colnames(MC_data)[[2]], "Mean_SHAP" = 0)
          }
          
          for (c in 1:nrow(temp_shapes)) {
            current_feat = temp_shapes[c,1]
            temp_shapes[c,i+1] = as.numeric(shap_temp[shap_temp[,1] == current_feat,2])
          }
          
          incProgress(1/MC_runs, detail = paste("MC run: ",i,"/",MC_runs))
        }
        
      })
    
    even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
    odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
    
    obs_mean_values = rowMeans(odd_columns)
    fits_mean_values = rowMeans(even_columns)
    fits = cbind(obs_mean_values,round(fits_mean_values,3))
    
    temp_shapes1 = data.frame(temp_shapes[,-1])
    
    XGB_shapes0 = rowMeans(temp_shapes1)
    
    xgb_results = data.frame(cbind(data1[,1],fits,round(data[,2:ncol(data)],4)))
    colnames(xgb_results)[1:3] = c(colnames(data0)[[1]],colnames(data0)[[rv]],"Fitted_Values")
    xgb_results = xgb_results[order(xgb_results[,1]),]
    
    XGB_results(xgb_results)
  
    XGB_shapes = data.frame(cbind(temp_shapes[,1],format(round(XGB_shapes0,4),scientific=FALSE)))
    colnames(XGB_shapes) = c("Feature","Mean_SHAP")
    XGB_coeffs(XGB_shapes[order(XGB_shapes[,2],decreasing=TRUE),])
    
    XGB_scat_dat(XGB_results()[,1:3])
    
    XGB_confuse_results(confuse(XGB_scat_dat()[,2:3],input$XGB_stand,input$XGB_dec_crit))
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'XGB: Fitting')
    
    # Fit final XGB_Model
    final_XGB_data = MC_final_subbin(data,input$loggy,input$lc_val,input$rc_val,0.5,1.5)
    
    XGB_final_data(final_XGB_data)
    
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
    
    XGB_model <<- xgboost(data = as.matrix(final_XGB_data[,-1]),label=final_XGB_data[,1], params=params, early_stopping_rounds=early_stop_set(),
                            nrounds=2000, verbose=0)
    
    XGB_standardize(input$XGB_standard)
    
    updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
    
  })
  
  observeEvent(c(XGB_results(),refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(XGB_results())) {
    
      xgb_stepr = round((max(XGB_results()[,2])-min(XGB_results()[,2]))/40,2)
    
      updateNumericInput(session, "XGB_stand",
                       value = round(mean(XGB_results()[,2]),2),
                       max = round(max(XGB_results()[,2]),2),
                       min = round(min(XGB_results()[,2]),2),
                       step = xgb_stepr
      )
    
      xgb_stepdc = round((max(XGB_results()[,3])-min(XGB_results()[,3]))/40,2)
    
      updateNumericInput(session, "XGB_dec_crit",
                       value = round(mean(XGB_results()[,3]),2),
                       max = round(max(XGB_results()[,3]),2),
                       min = round(min(XGB_results()[,3]),2),
                       step = xgb_stepdc
      )
    
      output$XGB_shapes = DT::renderDataTable(server = T, {data = datatable(XGB_coeffs(),rownames = F,selection =
                  list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                  list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                  className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
      output$XGB_fits = DT::renderDataTable(server = T, {data = datatable(XGB_results(),rownames = F,selection = list(selected = list(rows = NULL, cols = NULL),
                  target = "row",mode = "single"),editable = F,extensions="Buttons", options = list(autoWidth = F,dom="ltBp", buttons = c('copy', 'csv', 'excel'),
                  paging = T,pageLength = num_rows_per_page,scrollX = T,scrollY = T,columnDefs = list(list(className = 'dt-center',orderable = T,targets = '_all')),initComplete =
                  JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})

      output$XGB_scatplot = renderPlotly(scatter_confuse(XGB_scat_dat(),input$XGB_stand,input$XGB_dec_crit))
    
      xgb_resid_data = XGB_scat_dat()[,c(1,3)] %>% mutate(Residuals = round(XGB_scat_dat()[,2]-XGB_scat_dat()[,3],3))
      output$XGB_resid_scatplot = renderPlotly(scatter(xgb_resid_data))
    
      output$XGB_lineplot = renderPlotly(plot_ly(XGB_scat_dat(), x = ~XGB_scat_dat()[,1], y = ~XGB_scat_dat()[,2], name="Observations", type="scatter", mode = "lines",
                        text = ~paste("<b>ID: </b>",XGB_scat_dat()[,1],"<br><b>Observed Value:</b> ",XGB_scat_dat()[,2],sep=""),hoveron = 'points',hoverinfo='text',
                        line = list(color = "#2c3e50", width = 1.5)) %>%
                  add_trace(y = ~XGB_scat_dat()[,3], name="Fitted_Values", mode = 'lines',text = ~paste("<b>ID: </b>",XGB_scat_dat()[,1],"<br><b>Fitted Value:</b> ",
                        round(XGB_scat_dat()[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                  layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Fitted Values",font=list(size=20)),
                        range=c(min(0.99*min(XGB_scat_dat()[,2],XGB_scat_dat()[,3]),1.01*min(XGB_scat_dat()[,2],XGB_scat_dat()[,3])),max(0.99*max(XGB_scat_dat()[,2],
                        XGB_scat_dat()[,3]),1.01*max(XGB_scat_dat()[,2],XGB_scat_dat()[,3]))))))
      
      XGB_confuse_results(confuse(XGB_scat_dat()[,2:3],input$XGB_stand,input$XGB_dec_crit))

      xgb_confuse_table = matrix(0,nrow=1,ncol=4)
    
      xgb_confuse_table[1,1] = XGB_confuse_results()$TP
      xgb_confuse_table[1,2] = XGB_confuse_results()$TN
      xgb_confuse_table[1,3] = XGB_confuse_results()$FP
      xgb_confuse_table[1,4] = XGB_confuse_results()$FN
    
      colnames(xgb_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
      output$XGB_confuse = DT::renderDataTable(server = T, {data = datatable(xgb_confuse_table,rownames = F,selection = 
                    list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                    options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                    columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                    {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
      output$XGB_confuse_text = renderText({paste0("Sensitivity = ",round(XGB_confuse_results()$Sensitivity,3),"; Specificity = ",
                    round(XGB_confuse_results()$Specificity,3),"; Accuracy = ",round(XGB_confuse_results()$Accuracy,3))})
    
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
    
    print(pdp_info$plot())
    
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
  observeEvent(input$EN_pred_stand, ignoreInit = TRUE, {
    
    if (nrow(EN_pred_scat_dat()) != 0) {
      
      iv$add_rule("EN_pred_stand", sv_between(min(EN_pred_scat_dat()[,2]),max(EN_pred_scat_dat()[,2])))
      
      refresh_trigger(TRUE)
    }
  })
  
  observeEvent(input$EN_pred_dc, ignoreInit = TRUE,{
    
    if (nrow(EN_pred_scat_dat()) != 0) {
      
      iv$add_rule("EN_pred_dc", sv_between(min(EN_pred_scat_dat()[,3]),max(EN_pred_scat_dat()[,3])))
      
      refresh_trigger(TRUE)
      
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
    
      en_stepr = round((max(EN_pred_results()[,2])-min(EN_pred_results()[,2]))/40,2)
    
      updateNumericInput(session, "EN_pred_stand",
                       value = round(mean(EN_pred_results()[,2]),2),
                       max = round(max(EN_pred_results()[,2]),2),
                       min = round(min(EN_pred_results()[,2]),2),
                       step = en_stepr
      )
    
      en_stepdc = round((max(EN_pred_results()[,3])-min(EN_pred_results()[,3]))/40,2)
    
      updateNumericInput(session, "EN_pred_dc",
                       value = round(mean(EN_pred_results()[,3]),2),
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
    

      output$EN_pred_scatplot = renderPlotly(scatter_confuse(EN_pred_scat_dat(),input$EN_pred_stand,input$EN_pred_dc))
    

      resid_data = EN_pred_results()[,c(1,3)]
      resid_data = resid_data %>% mutate(Residuals = round(EN_pred_results()[,2]-EN_pred_results()[,3],3))
      output$EN_pred_resid_scatter = renderPlotly(scatter(resid_data))
    
      output$EN_pred_lineplot = renderPlotly(plot_ly(EN_pred_scat_dat(), x = ~EN_pred_scat_dat()[,1], y = ~EN_pred_scat_dat()[,2], name="Observations", type="scatter",
                            mode = "lines",text = ~paste("<b>ID: </b>",EN_pred_scat_dat()[,1],"<br><b>Observed Value:</b> ",EN_pred_scat_dat()[,2],sep=""),
                            hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5)) %>%
                  add_trace(y = ~EN_pred_scat_dat()[,3], name="Predictions", mode = 'lines',text = ~paste("<b>ID: </b>",EN_pred_scat_dat()[,1],"<br><b>Prediction:</b> ",
                            round(EN_pred_scat_dat()[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                  layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Predictions",
                            font=list(size=20)),range=c(min(0.99*min(EN_pred_scat_dat()[,2],EN_pred_scat_dat()[,3]),1.01*min(EN_pred_scat_dat()[,2],EN_pred_scat_dat()[,3])),
                            max(0.99*max(EN_pred_scat_dat()[,2],EN_pred_scat_dat()[,3]),1.01*max(EN_pred_scat_dat()[,2],EN_pred_scat_dat()[,3]))))))
      
      EN_pred_confuse_results(confuse(EN_pred_scat_dat()[,2:3],input$EN_pred_stand,input$EN_pred_dc))

      confuse_table = matrix(0,nrow=1,ncol=4)
    
      confuse_table[1,1] = EN_pred_confuse_results()$TP
      confuse_table[1,2] = EN_pred_confuse_results()$TN
      confuse_table[1,3] = EN_pred_confuse_results()$FP
      confuse_table[1,4] = EN_pred_confuse_results()$FN
    
      colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
      output$EN_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                      list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions="Buttons",
                      options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                      columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
      output$EN_pred_confuse_text = renderText({paste0("Sensitivity = ",round(EN_pred_confuse_results()$Sensitivity,3),"; Specificity = ",
                      round(EN_pred_confuse_results()$Specificity,3),"; Accuracy = ",round(EN_pred_confuse_results()$Accuracy,3))})
    
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
  observeEvent(input$EN_stand, ignoreInit = TRUE, {
    
    if (nrow(EN_scat_dat()) != 0) {
      
      iv$add_rule("EN_stand", sv_between(min(EN_scat_dat()[,2]),max(EN_scat_dat()[,2])))
      
      refresh_trigger(TRUE)
      
    }
  })
  
  observeEvent(input$EN_dec_crit, ignoreInit = TRUE, {
    
    if (nrow(EN_scat_dat()) != 0) {
      
      iv$add_rule("EN_dec_crit", sv_between(min(EN_scat_dat()[,3]),max(EN_scat_dat()[,3])))
      
      refresh_trigger(TRUE)
    }
  })
  
  observeEvent(input$EN_fit, ignoreInit = TRUE, {
    
    req(iv$is_valid())
    
    updateNumericInput(session, "num_preds",value = 2)
    changed_model(TRUE)
    
    if (input$use_pca_data) {
      data0 = PCA_dataset()
      rv=2
      feats_to_use = input$pcax_to_use
      ignored_rows = NULL
      EN_model_PCA(TRUE)
    } else {
      data0 = current_data()
      rv=response_var()
      feats_to_use = input$feats_to_use
      EN_model_PCA(FALSE)
    }
    
    set.seed(input$model_seed)
    
    MC_runs=input$MC_runs
    
    data1 = create_data(data0,rv,feats_to_use,ignored_rows,input$randomize,FALSE)
    data = data1[,-1]
    
    imp_X=missForest(data[,-1])$ximp
    trainData = cbind(data[,1],imp_X)
    colnames(trainData) = colnames(data)
    
    temp_fits = matrix(0, nrow = nrow(trainData), ncol = 2*MC_runs)
    temp_fits = data.frame(temp_fits)
    
    temp_coeffs = matrix(0, nrow = ncol(trainData), ncol = MC_runs+1)
    temp_coeffs = data.frame(temp_coeffs)
    temp_coeffs[,1] = c("(Intercept)",feats_to_use)
    
    withProgress(
      message = 'EN Fitting Progress',
      detail = paste("MC runs: ", x = 1,"/",MC_runs),
      value = 0,
      {
        for (i in 1:MC_runs) {
          
          MC_data = MC_subbin(trainData,input$loggy,input$lc_val,input$lc_lowval,input$lc_upval,input$rc_val,input$rc_lowval,input$rc_upval) 
          
          temp_fits[,2*i-1] = MC_data[,1]
          
          # determine best alpha and lambda
          fit_mod = cva.glmnet(x=as.matrix(MC_data[,-1]),y=MC_data[,1],nfolds=input$num_folds,na.action="na.omit",
                               standardize=input$EN_standard,intercept=TRUE)
          
          alpha = get_model_params(fit_mod)$alpha
          lambda = get_model_params(fit_mod)$lambdaMin
          
          en_model = glmnet(x=as.matrix(MC_data[,-1]),MC_data[,1],lambda=lambda, alpha=alpha, na.action="na.omit",
                              standardize=input$EN_standard,intercept=TRUE)
          
          coeffs = as.matrix(coef(en_model,s=lambda))
          coeffs = as.data.frame(coeffs)
          temp_coeffs[,i+1] = coeffs
          
          fits = predict(en_model, newx = as.matrix(MC_data[,-1]))
          temp_fits[,2*i] = round(fits,3)
          
          incProgress(1/MC_runs, detail = paste("MC run:",i,"/",MC_runs))
        }
      })
    
    mean_coeffs = format(round(rowMeans(temp_coeffs[,-1]),4),scientific=FALSE)
    en_coeffs = data.frame(cbind(temp_coeffs[,1],mean_coeffs))
    colnames(en_coeffs) = c("Feature","Coefficient")
    
    EN_coeffs(en_coeffs)
    
    even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
    odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
    
    obs_mean_values = rowMeans(odd_columns)
    fit_mean_values = rowMeans(even_columns)
    en_results = data.frame(cbind(data1[,1],round(obs_mean_values,3),round(fit_mean_values,3),round(data[,-1],4)))
    colnames(en_results) = c(colnames(data0)[1],colnames(data0)[rv],"Fitted_Value",colnames(data[,-1]))
    
    en_results = en_results[order(en_results[,1]),]
    
    EN_results(en_results)
    EN_scat_dat(EN_results()[,1:3])
    
    EN_confuse_results(confuse(EN_scat_dat()[,2:3],input$EN_stand,input$EN_dec_crit))
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'EN: Fitting')
    
    # Fit final EN_Model
    final_EN_data = MC_final_subbin(trainData,input$loggy,input$lc_val,input$rc_val,0.5,1.5)
    
    # determine best alpha and lambda
    fit_EN_model = cva.glmnet(x=as.matrix(final_EN_data[,-1]),y=final_EN_data[,1],nfolds=input$num_folds,na.action="na.omit",
                         standardize=input$EN_standard,intercept=TRUE)
    
    final_alpha = get_model_params(fit_EN_model)$alpha
    final_lambda = get_model_params(fit_EN_model)$lambdaMin
    
    EN_model <<- glmnet(x=as.matrix(final_EN_data[,-1]),final_EN_data[,1],lambda=final_lambda, alpha=final_alpha, na.action="na.omit",
                        standardize=input$EN_standard,intercept=TRUE)
    
    EN_standardize(input$EN_standard)
    
    updateRadioButtons(session,"model_choice",selected="None",choices=c("None",models_created()))
  })
  
  observeEvent(c(EN_results(), refresh_trigger()), ignoreInit = TRUE, {
    
    if (!is.null(EN_results())) {
    
      en_stepr = round((max(EN_results()[,2])-min(EN_results()[,2]))/40,2)
    
      updateNumericInput(session, "EN_stand",
                       value = round(mean(EN_results()[,2]),2),
                       max = round(max(EN_results()[,2]),2),
                       min = round(min(EN_results()[,2]),2),
                       step = en_stepr)
    
      en_stepdc = round((max(EN_results()[,3])-min(EN_results()[,3]))/40,2)
    
      updateNumericInput(session, "EN_dec_crit",
                       value = round(mean(EN_results()[,3]),2),
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
    

      output$EN_scatplot = renderPlotly(scatter_confuse(EN_scat_dat(),input$EN_stand,input$EN_dec_crit))
    
      resid_data = EN_results()[,c(1,3)]
      resid_data = resid_data %>% mutate(Residuals = round(EN_results()[,2]-EN_results()[,3],3))
      output$EN_resid_scatplot = renderPlotly(scatter(resid_data))

      output$EN_lineplot = renderPlotly(plot_ly(EN_scat_dat(), x = ~EN_scat_dat()[,1], y = ~EN_scat_dat()[,2], name="Observations", type="scatter",
                        mode = "lines",text = ~paste("<b>ID: </b>",EN_scat_dat()[,1],"<br><b>Observed Value:</b> ",EN_scat_dat()[,2],sep=""),
                        hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5)) %>%
                add_trace(y = ~EN_scat_dat()[,3], name="Fitted_Value", mode = 'lines',text = ~paste("<b>ID: </b>",EN_scat_dat()[,1],"<br><b>Fitted_Value:</b> ",
                        round(EN_scat_dat()[,3],3),sep=""),hoveron = 'points',hoverinfo='text',line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Fitted_Values",
                        font=list(size=20)),range=c(min(0.99*min(EN_scat_dat()[,2],EN_scat_dat()[,3]),1.01*min(EN_scat_dat()[,2],EN_scat_dat()[,3])),
                        max(0.99*max(EN_scat_dat()[,2],EN_scat_dat()[,3]),1.01*max(EN_scat_dat()[,2],EN_scat_dat()[,3]))))))
      
      EN_confuse_results(confuse(EN_scat_dat()[,2:3],input$EN_stand,input$EN_dec_crit))
    
      EN_confuse_table = matrix(0,nrow=1,ncol=4)
    
      EN_confuse_table[1,1] = EN_confuse_results()$TP
      EN_confuse_table[1,2] = EN_confuse_results()$TN
      EN_confuse_table[1,3] = EN_confuse_results()$FP
      EN_confuse_table[1,4] = EN_confuse_results()$FN
    
      colnames(EN_confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
      output$EN_confuse = DT::renderDataTable(server = T, {data = datatable(EN_confuse_table,rownames = F,selection = 
                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions = 'Buttons',
                  options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                  columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                  {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
      output$EN_confuse_text = renderText({paste0("Sensitivity = ",round(EN_confuse_results()$Sensitivity,3),"; Specificity = ",
                  round(EN_confuse_results()$Specificity,3),"; Accuracy = ",round(EN_confuse_results()$Accuracy,3))})
    
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
        output$pca_model_text = renderText({HTML("NOTE: PCA features being used.")})
      } else {
        output$pca_model_text = NULL
      }
    }
  })
  
  # Save Prediction file from Prediction Tab
  output$save_prediction = downloadHandler(filename = function() {paste("Prediction_File.RData")}, content = function(file) {
    
    save_list = list(
      type = "Prediction",
      Version = Version,
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
      date_format_string = date_format_string,
      saved_lc_val = input$lc_val,
      saved_rc_val = input$rc_val,
      saved_num_axes = input$num_axes,
      init_column_props = init_column_props,
      column_props = column_props,
      PCA_scaling_mean = reactiveVal(),
      PCA_scaling_sd = reactiveVal(),
      PCA_dataset = PCA_dataset(),
      PCA_summary_df = PCA_summary_df(),
      PCA_coefficients = PCA_coefficients(),
      pca_axes_max = pca_axes_max(),
      pca_axes = pca_axes(),
      pcax_being_used = pcax_being_used(),
      fs_pcax_used = fs_pcax_used(),
      final_model_PCA = final_model_PCA(),
      
      LG_pred_results = LG_pred_results(),
      LG_pred_coeffs = LG_pred_coeffs(),
      LG_pred_confuse_results = LG_pred_confuse_results(),
      LG_pred_scat_dat = LG_pred_scat_dat(),
      LG_pred_standardize = LG_pred_standardize(),
      LG_results = LG_results(),
      LG_coeffs = LG_coeffs(),
      LG_confuse_results = LG_confuse_results(),
      LG_scat_dat = LG_scat_dat(),
      LG_model = LG_model,
      LG_standardize = LG_pred_standardize(),
      LG_model_PCA = LG_model_PCA(),
      
      XGBCL_pred_results = XGBCL_pred_results(),
      XGBCL_pred_coeffs = XGBCL_pred_coeffs(),
      XGBCL_pred_confuse_results = XGBCL_pred_confuse_results(),
      XGBCL_pred_scat_dat = XGBCL_pred_scat_dat(),
      XGBCL_pred_standardize = XGBCL_pred_standardize(),
      XGBCL_selection_results = XGBCL_selection_results(),
      XGBCL_results = XGBCL_results(),
      XGBCL_coeffs = XGBCL_coeffs(),
      XGBCL_confuse_results = XGBCL_confuse_results(),
      XGBCL_scat_dat = XGBCL_scat_dat(),
      XGBCL_model = XGBCL_model,
      XGBCL_standardize = XGBCL_standardize(),
      XGBCL_model_PCA = XGBCL_model_PCA(),
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
      XGB_scat_dat = XGB_scat_dat(),
      XGB_model = XGB_model,
      XGB_standardize = XGB_standardize(),
      XGB_model_PCA = XGB_model_PCA(),
      Optimal_HP = Optimal_HP,
      
      EN_pred_results = EN_pred_results(),
      EN_pred_coeffs = EN_pred_coeffs(),
      EN_pred_confuse_results = EN_pred_confuse_results(),
      EN_pred_scat_dat = EN_pred_scat_dat(),
      EN_pred_standardize = EN_pred_standardize(),
      EN_results = EN_results(),
      EN_coeffs = EN_coeffs(),
      EN_confuse_results = EN_confuse_results(),
      EN_scat_dat = EN_scat_dat(),
      EN_model = EN_model,
      EN_standardize = EN_standardize(),
      EN_model_PCA = EN_model_PCA()
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
      renderpreddata(pred_data(),date_format_string,column_props,current_pred_page(),output)
    }
  })
  
  # Upload excel/csv data file into prediction data table
  observeEvent(input$pred_file, ignoreInit = T, {
    
    ext = tools::file_ext(input$pred_file$name)
    
    if (ext == "xlsx") {
      pred_file_data <<- read.xlsx(input$pred_file$datapath)
    } else {
      pred_file_data <<- read.csv(input$pred_file$datapath,header = TRUE,sep = input$sep)
    }
    
    if (any(sapply(data.frame(pred_file_data[,-1]), function(col) !is.numeric(col)))) {
      
      showModal(modalDialog(paste("This dataset contains non-numeric data. Please remedy prior to data importation."),
                            easyClose = F,footer = div(modalButton('Close'))))
      return()
      
    } else if (ncol(pred_file_data) < length(pred_model_features())+2) {
      
      showModal(modalDialog(paste("Not enough columns in the imported file."),
                            easyClose = F,footer = div(modalButton('Close'))))
      
      return()
      
    }  else if (nrow(pred_file_data) < 2) {
      
      showModal(modalDialog(paste("File must contain more than 1 data row."), easyClose = F,footer = div(modalButton('Close'))))
      
      return()
      
    } else {
      
      num_feats = length(pred_model_features())
      
      temp_data = data.frame(matrix(NA, nrow = nrow(pred_file_data), ncol = num_feats+5))
      
      for (i in 1:nrow(pred_file_data)) {
        temp_data[i,] = cbind(pred_file_data[i,1:(num_feats+2)],-999,-999,-999)
      }
      
      for (i in 1:nrow(temp_data)) {
        for (j in 1:ncol(temp_data)) {
          if (is.na(temp_data[i,j])) {
            temp_data[i,j] = -999
          }
        }
      }
      
      colnames(temp_data) = c(colnames(current_data()[1]),colnames(current_data()[response_var()]),pred_model_features(),"Prediction","Lower_Bound","Upper_Bound")
      pred_data(temp_data)
      
      updateNumericInput(session, "num_preds",value = nrow(pred_data()))
      
      current_pred_page(1)
      
      renderpreddata(pred_data(),date_format_string,column_props,current_pred_page(),output)
    }
  })
  
  # Load a model for prediction
  observeEvent(input$model_choice, ignoreInit = T, {
    
    if (input$model_choice == "None") {
      output$pd_data = NULL
      output$resid_text = NULL
      output$model_text = NULL
    } else {
      
      feature_mismatch(FALSE)
      standard_mismatch(FALSE)
      thresh_mismatch(FALSE)
      no_resids(FALSE)
      
      if (input$model_choice == "Logistic_Regression") {
        
        output$model_text <- renderUI({
          threshold = LG_thresh()
          
          HTML(paste0(
            "<div style='font-size: 20px; font-weight: bold;'>Model: Logistic Regression</div>",
            "<div style='font-style: italic;'>Predictions: Probabilities of Exceedance</div>",
            "<div>Critical Threshold: ", threshold, "</div>"
          ))
        })
        
        model_to_use(LG_model)
        
        if (LG_model_PCA()) {
          final_model_PCA(TRUE)
          model_features = PCA_coefficients()[,1]
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
          model_features = colnames(LG_results())[4:ncol(LG_results())]
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
          
          HTML(paste0(
            "<div style='font-size: 20px; font-weight: bold;'>Model: XGBoost Classifier</div>",
            "<div style='font-style: italic;'>Predictions: Probabilities of Exceedance</div>",
            "<div>Critical Threshold: ", threshold, "</div>"
          ))
        })
        
        model_to_use(XGBCL_model)
        
        if (XGBCL_model_PCA()) {
          final_model_PCA(TRUE)
          model_features = PCA_coefficients()[,1]
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
          model_features = colnames(XGBCL_results())[4:ncol(XGBCL_results())]
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
        
        output$model_text = renderUI({HTML("<div style='font-size: 20px; font-weight: bold;'>Model: XGBoost</div>
            <div style='font-style: italic;'>Predictions: Response Variable Units</div>")})
        
        model_to_use(XGB_model)
        
        if (XGB_model_PCA()) {
          final_model_PCA(TRUE)
          model_features = PCA_coefficients()[,1]
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
          model_features = colnames(XGB_results())[4:ncol(XGB_results())]
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
        
        output$model_text = renderUI({HTML("<div style='font-size: 20px; font-weight: bold;'>Model: Elastic Net</div>
            <div style='font-style: italic;'>Predictions: Response Variable Units</div>")})
        
        model_to_use(EN_model)
        
        if (EN_model_PCA()) {
          final_model_PCA(TRUE)
          model_features = PCA_coefficients()[,1]
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
          model_features = colnames(EN_results())[4:ncol(EN_results())]
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
        output$pca_model_text = renderText({HTML("NOTE: PCA features being used.")})
      } else {
        output$pca_model_text = NULL
      }
      
      iv_name = colnames(current_data())[1]
      rv_name = colnames(current_data())[response_var()]
      
      temp_data = data.frame(matrix(-999, nrow = input$num_preds, ncol = length(model_features)+5))
      
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
      
      colnames(temp_data) = c(iv_name,rv_name,model_features,"Prediction","Lower_Bound","Upper_Bound")
      
      if (is.null(pred_data()) || changed_model()) {
        pred_data(temp_data)
        changed_model(FALSE)
      }
      
      pred_residuals(resids)
      pred_model_features(model_features)
      
      if (no_resids()) {
        output$resid_text = renderText({HTML("NOTE: There are no available prediction residuals. Run a prediction model to generate them if you want confidence interval calculations.")})
      } else if (standard_mismatch()) {
        output$resid_text = renderText({HTML("NOTE: Prediction and Fitted models have different feature standardizations. The prediction residuals cannot be used.")})
      } else if (feature_mismatch()) {
        output$resid_text = renderText({HTML("NOTE: Prediction and Fitted models have different features. The prediction residuals cannot be used.")})
      } else if (thresh_mismatch()) {
        output$resid_text = renderText({HTML("NOTE: Prediction and Fitted models have different critical value thresholds. The prediction residuals cannot be used.")})
      } else if (is.null(resids)) {
        output$resid_text = renderText({HTML("NOTE: There are no available prediction residuals. Run a prediction model to generate them if you want confidence interval calculations.")})
      } else {
        output$resid_text = renderText({HTML("SUCCESS: Prediction residuals are available and will be used for confidence interval calculations.")})
      }
      
      current_pred_page(1)
      
      renderpreddata(pred_data(),date_format_string,column_props,current_pred_page(),output)
    }
  })
  
  # Change the number of desired predictions
  observeEvent(input$num_preds, ignoreInit = T, {
    
    req(iv$is_valid())
    
    temp_data = pred_data()
    model_features = pred_model_features()
    iv_name = colnames(temp_data)[1]
    rv_name = colnames(temp_data)[2]
    
    if (nrow(temp_data) < input$num_preds) {
      
      new_rows = matrix(-999, nrow = (input$num_preds-nrow(temp_data)), ncol = length(model_features)+5)
      colnames(new_rows) = c(iv_name,rv_name,model_features,"Prediction","Lower_Bound","Upper_Bound")

      temp_data1 = rbind(temp_data,new_rows)
      colnames(temp_data1) = colnames(new_rows)
      
    } else if (input$num_preds==1) {
      
      temp_data1 = matrix(-999, nrow = 1, ncol = length(model_features)+5)
      colnames(temp_data1) = c(iv_name,rv_name,model_features,"Prediction","Lower_Bound","Upper_Bound")

    } else {
      temp_data1 = temp_data[1:input$num_preds,]
    }
    
    pred_data(temp_data1)
    
    renderpreddata(pred_data(),date_format_string,column_props,current_pred_page(),output)
    
  })
  
  # Provide prediction dataset cell value editing
  observeEvent(input$pd_data_cell_edit, ignoreInit = T, ignoreNULL = T, {
    
    info = input$pd_data_cell_edit
    
    data=pred_data()

    i = info$row
    j = info$col + 1
    
    data = editData(data,input$pd_data_cell_edit,"pd_data",rownames = FALSE)
    
    pred_data(data)
    renderpreddata(pred_data(),date_format_string,column_props,current_pred_page(),output)
  })
  
  # Make predictions using selected model
  observeEvent (input$make_preds, ignoreInit = T, {
    
    req(iv$is_valid())
    
    prediction_data = data.frame(pred_data())
    
    if (any(prediction_data[,3:(ncol(prediction_data)-3)] == -999)) {
      
      showModal(modalDialog(paste("Missing feature values are not allowed. Provide all feature values to make predictions."),
                            easyClose = F,footer = div(modalButton('Close'))))
      return()
      
    } else {
      
      resids = pred_residuals()
      model = model_to_use()
      model_features = pred_model_features()
      iv_name = colnames(current_data())[1]
      rv_name = colnames(current_data())[response_var()]
      stop = ncol(prediction_data)-3
      
      if (input$num_preds == 1 && nrow(prediction_data) > 1) {
        pred_feat_data = data.frame(t(prediction_data[1,3:stop]))
      } else if (input$num_preds == 1 && nrow(prediction_data) <2) {
        pred_feat_data = data.frame(prediction_data[1,3:stop])
      } else {
        pred_feat_data = data.frame(prediction_data[,3:stop])
      }
      
      if (final_model_PCA()) {
        
        matched_mean = PCA_scaling_mean()[colnames(pred_feat_data)]
        matched_sd = PCA_scaling_sd()[colnames(pred_feat_data)]
        scaled_pred_data = scale(pred_feat_data, center = matched_mean, scale = matched_sd)
        
        part1 = as.matrix(scaled_pred_data)
        part2 = as.matrix(PCA_coefficients() %>% select(any_of(model_PCA_axes())))
        
        pred_pca_data = data.frame(part1 %*% part2)
        
      }
      
      if (input$model_choice == "Logistic_Regression") {
        
        if (final_model_PCA()) {
          predictions = predict(model, newx = as.matrix(pred_pca_data), type = "response", scale = LG_standardize())
        } else {
          predictions = predict(model, newx = as.matrix(pred_feat_data), type = "response", scale = LG_standardize())
        }
        
      } else if (input$model_choice == "XGB_Classifier") {
        
        if (final_model_PCA()) {
          predictions = predict(model, newdata = as.matrix(pred_pca_data, type="response", scale = XGBCL_standardize()))
        } else {
          predictions = predict(model, newdata = as.matrix(pred_feat_data, type="response", scale = XGBCL_standardize()))
        }
        
      } else if (input$model_choice == "XGBoost") {
        
        if (final_model_PCA()) {
          predictions = predict(model, newdata = as.matrix(pred_pca_data), scale = XGB_standardize())
        } else {
          predictions = predict(model, newdata = as.matrix(pred_feat_data), scale = XGB_standardize())
        }
        
      } else if (input$model_choice == "Elastic_Net") {
        
        if (final_model_PCA()) {
          predictions = predict(model, newx = as.matrix(pred_pca_data), scale = EN_standardize())
        } else {
          predictions = predict(model, newx = as.matrix(pred_feat_data), scale = EN_standardize())
        }
        
      }
      
      if (!is.null(resids)) {
        
        sorted_resids = sort(resids,decreasing = TRUE)
        
        # Define the percentile (e.g., 90th percentile)
        percentile = 100*(1-input$conf_bound)
        
        # Calculate the index for the ith percentile largest value
        index = ceiling((percentile / 100)*length(sorted_resids))
        
        # Find the ith percentile largest value
        crit_value = sorted_resids[index]
        
        upper_bound = round(predictions + crit_value,3)
        lower_bound = round(predictions - crit_value,3)
        
        if (input$model_choice == "Logistic_Regression" || input$model_choice == "XGB_Classifier") {
          upper_bound[upper_bound > 1] = 1
          lower_bound[lower_bound < 0] = 0
        }
        
      } else {
        
        upper_bound = rep(NA, length(predictions))
        lower_bound = rep(NA, length(predictions))
      }
      
      
      if (input$num_preds == 1) {
        new_pred_data = data.frame(cbind(prediction_data[1,1],prediction_data[1,2],pred_feat_data,round(predictions,3),lower_bound,upper_bound))
      } else {
        new_pred_data = data.frame(cbind(prediction_data[,1],prediction_data[,2],pred_feat_data,round(predictions,3),lower_bound,upper_bound))
      }
      
      colnames(new_pred_data) = c(iv_name,rv_name,model_features,"Prediction","Lower_Bound","Upper_Bound")
      pred_data(new_pred_data)
      
      output$pd_data = renderpreddata(pred_data(),date_format_string,column_props,current_pred_page(),output)
    }
  })
  
  # Disconnect the opened SQLite database
  session$onSessionEnded(function() {
    dbDisconnect(temp_db)
  })
}
shinyApp(ui, server)