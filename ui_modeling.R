ModelingPanel <- div(id="modeling_flex", style = "display:flex; gap:8px; align-items:flex-start;",
                     
    div(id="modelingsidepanel", style = "flex:0 0 400px; max-width:400px; min-width:400px; background-color:#e9ecef; padding:10px;",
    #h6(HTML("<i>NOTE: Model pipelines require >= 2 selected features/PCA components.</i>")),
    
    checkboxGroupButtons(
      inputId = "feats_to_use",
      label = "Features to Use: ",
      choices = "NULL",
      size = "xs",
      status = "custom"
    ),
    
    div(id = "pca_row",style = "display:flex; align-items:center; gap:8px; flex-wrap:nowrap;",
      tags$style(HTML("#pca_row .shiny-input-container { width:auto; margin-bottom:0; }")),
      actionButton("feats_select", "Unselect All", class = "btn btn-default btn-sm"),
      switchInput("use_pca_data", label = "Use PCA?", labelWidth = 85, value = FALSE,onLabel = "Yes", offLabel = "No", size = "small")),
    
    disabled(checkboxGroupButtons(
      inputId = "pcax_to_use",
      label = "PCA Axes to Use: ",
      choices = "NULL",
      size = "xs",
      status = "custom"
    )),
    
    bs_accordion(id = "Modeling") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "General Options",
        content = card(
          fluidRow(column(12,switchInput("loggy", label="Log10 Response?", labelWidth=100, value = FALSE, onLabel = "Yes", offLabel = "No", size = "small"))),
          h5(HTML("<i>Left-Censored Limits (Non-Detections)</i>")),
          fluidRow(column(6,numericInput("lc_lowval", label='Lower', value = 0)),
                   column(6, numericInput("lc_upval", label='Upper', value = 3))),
          h5(HTML("<i>Right-Censored Limits (TNTC)</i>")),
          fluidRow(column(6,numericInput("rc_lowval", label='Lower', value = 1000)),
                   column(6, numericInput("rc_upval", label='Upper', value = 10000))),
          fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 2px; margin-bottom: 2px;")),
          fluidRow(column(6, numericInput("train_pct",  label="% Training", value = 75, min=1,max=100,step=1)),
                   column(6, numericInput("MC_runs",  label="MC Runs", value = 2, min=2,max=10000,step=1))),
          fluidRow(column(6, numericInput("num_folds",  label="CV Folds", value = 5, min=2,max=20,step=1)),
                   column(6, numericInput("model_seed",  label="Rnd Seed", value = 1234, min=1,step=1))),
          fluidRow(column(12,switchInput("randomize", label="Shuffle Data?", labelWidth=100, value = TRUE, onLabel = "Yes", offLabel = "No", size = "small"))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
    h5(HTML("<i>Binary Response Techniques</i>"),style="text-align:center"),
    
    bs_accordion(id = "Discreet_Techniques") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Logistic Regression",
        content = card(
          fluidRow(column(12, switchInput("LG_standard", label="Standardize Features?", labelWidth=125, value = TRUE, onLabel = "Yes", offLabel = "No", size = "small"))),
          fluidRow(column(12, switchInput("LG_binarize", label="Binarize Response?", labelWidth=125, value = TRUE, onLabel = "Yes", offLabel = "No", size = "small"))),
          fluidRow(column(5,numericInput("LG_binarize_crit_value", label = "Threshold", value = 2,step=0.25)),
                   column(7,selectInput("LG_eval",label = "Evaluation Metric",selectize=FALSE,selected ="deviance",choices = c("deviance","auc")))),
          fluidRow(column(6,actionButton("run_pred_LG", "Predictions", class = "btn-default custom-btn",  style = 'width:100px !important; padding:2px !important;')),
                   column(6,actionButton("run_fitted_LG", "Fitting", class = "btn-default custom-btn",  style = 'width:100px !important; padding:2px !important;'))))) %>%
      
      bs_append (
        title = "XGB Classifier",
        content = card(
          fluidRow(column(12, switchInput("XGBCL_standard", label="Standardize Features?", labelWidth=125, value = FALSE, onLabel = "Yes", offLabel = "No", size = "small"))),
          fluidRow(column(12, switchInput("XGBCL_binarize", label="Binarize Response?", labelWidth=125, value = TRUE, onLabel = "Yes", offLabel = "No", size = "small"))),
          fluidRow(column(5,numericInput("XGBCL_binarize_crit_value", label = "Threshold", value = 2,step=0.25)),
                   column(7,selectInput("XGBCL_eval",label = "Evaluation Metric",selected ="logloss",choices = c("logloss","auc")))),
          fluidRow(column(5,actionButton("XGBCL_params", "HP Values",class = "btn-default custom2-btn", style = 'width:90px !important; padding:2px !important;')),
                   column(7,actionButton("XGBCL_optimize_HP", "HP Optimization",class = "btn-default custom2-btn", style = 'width:130px !important; padding:2px !important;'))),
          # fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 2px; margin-bottom: 2px;")),
          fluidRow(column(6,align="left",actionButton("run_XGBCL_select", "Feature Selection", class = "btn-default custom-btn", style = 'width:130px !important; padding:2px !important; vertical-align: -32px !important;')),
                   column(6, div(style = "display: inline-block;",numericInput("testcl_weight", label = "Test Weight", value = 0.33, min = 0, max=1, step=0.02)))),
          # fluidRow(column(12,align="right",actionButton("XGBCL_select_cancel", "Cancel", style = 'width:90px; padding:2px;'))),
          fluidRow(column(6,actionButton("run_pred_XGBCL", "Predictions", class = "btn-default custom-btn",  style = 'width:100px !important; padding:2px !important;')),
                   column(6,actionButton("run_fit_XGBCL", "Fitting", class = "btn-default custom-btn",  style = 'width:100px !important; padding:2px !important;'))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
    h5(HTML("<i>Continuous Response Techniques</i>"),style="text-align:center"),
    
    bs_accordion(id = "Continuous_Techniques") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Elastic Net",
        content = card(
          fluidRow(column(12, switchInput("EN_standard", label="Standardize Features?", labelWidth=125, value = TRUE, onLabel = "Yes", offLabel = "No", size = "small"))),
          fluidRow(column(6,actionButton("EN_pred", "Predictions", class = "btn-default custom-btn",  style = 'width:100px !important; padding:4px !important;')),
                   column(6,actionButton("EN_fit", "Fitting", class = "btn-default custom-btn",  style = 'width:100px !important; padding:4px !important;'))))) %>%
      
      bs_append (
        title = "XGBoost", content= card(
          fluidRow(column(12, switchInput("XGB_standard", label="Standardize Features?", labelWidth=125, value = FALSE, onLabel = "Yes", offLabel = "No", size = "small"))),
          fluidRow(column(5,actionButton("XGB_params", "HP Values",class = "btn-default custom2-btn", style = 'width:90px !important; padding:2px !important;')),
                   column(7,actionButton("XGB_optimize_HP", "HP Optimization",class = "btn-default custom2-btn", style = 'width:130px !important; padding:2px !important;'))),
          # fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 2px; margin-bottom: 2px;")),
          fluidRow(column(6,align="left",actionButton("run_XGB_select", "Feature Selection", class = "btn-default custom-btn", style = 'width:130px !important; padding:2px !important; vertical-align: -32px !important;')),
                   column(6, div(style = "display: inline-block;",numericInput("test_weight", label = "Test Weight", value = 0.33, min = 0, max=1, step=0.02)))),
          # fluidRow(column(12,align="right",actionButton("XGB_select_cancel", "Cancel", style = 'width:90px; padding:2px;'))),
          fluidRow(column(6,actionButton("run_XGB_predict", "Predictions", class = "btn-default custom-btn",  style = 'width:100px !important; padding:2px !important;')),
                   column(6,align="left",actionButton("XGB_final_fitting", class = "btn-default custom-btn",  "Fitting", style = 'width:100px !important; padding:2px !important;'))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
    fluidRow(column(12,actionButton("save_project", "Save Project File",class = "btn-default custom-btn btn-save",style = "width: 150px;"))),
    fluidRow(column(12,actionButton("open_saved_file","Open Saved Project/Prediction File",class = "btn-default custom-btn btn-tall",style = "width: 300px;"),
      div(style = "position: absolute; left: -9999px; top: -9999px; width: 1px; height: 1px; overflow: hidden;",
      fileInput("load_saved_file",label = NULL,accept = c(".RData", ".rda"),multiple = FALSE))))),
  
  div(id = "modeling_output", style = "flex:1 1 auto; min-width:0;",
    tabsetPanel(id = "modeling_tabs",
                
      tabPanel("LG: Predict",
        tags$style(HTML("#lg_pred_nav .row { flex-wrap: nowrap; }
        
        #lg_pred_nav .row > div:first-child {
          flex: 0 0 200px !important;
          max-width: 200px !important;
          width: 200px !important;
        }
        
        #lg_pred_nav .row > div:last-child {
          flex: 1 1 auto !important;
          max-width: calc(100% - 200px) !important;
          width: calc(100% - 200px) !important;
        }
        
        #lg_pred_nav .nav.flex-column .nav-link { white-space: nowrap; }")),
        
        div(id = "lg_pred_nav",
          bslib::navset_pill_list(widths = c(2, 10), well = FALSE,
            bslib::nav_panel("Results Table",DT::dataTableOutput("LG_preds"),
              tags$style(type = "text/css", "#LG_preds {height: calc(100vh - 70px) !important;}")),
            bslib::nav_panel("Coefficients",div(style = "width:600px;",DT::dataTableOutput("LG_pred_coeffs", width = "600px")),
              tags$style(type = "text/css", "#LG_pred_coeffs {height: calc(100vh - 70px) !important;}")),
            bslib::nav_panel("Prob Dens Plot",plotOutput("LG_pred_scatplot", height = "700px", width = "100%"),
              fluidRow(numericInput("LG_pred_dc", label = "Decision Criterion", value = 0.5, min = 0, max = 1, step = 0.01)),
              fluidRow(div(style = "width:600px;",DT::dataTableOutput("LG_pred_confuse", width = "600px"))),
              uiOutput("LG_pred_confuse_text"))))),
                
      tabPanel("LG: Fitting",
        tags$style(HTML("#lg_fit_nav .row { flex-wrap: nowrap; }
        
        #lg_fit_nav .row > div:first-child {
          flex: 0 0 200px !important;
          max-width: 200px !important;
          width: 200px !important;
        }
        
        #lg_fit_nav .row > div:last-child {
          flex: 1 1 auto !important;
          max-width: calc(100% - 200px) !important;
          width: calc(100% - 200px) !important;
        }
        
        #lg_fit_nav .nav.flex-column .nav-link { white-space: nowrap; }")),
               
        div(id = "lg_fit_nav",bslib::navset_pill_list(widths = c(2, 10),well   = FALSE,
          bslib::nav_panel("Results Table",DT::dataTableOutput("LG_fits"),
            tags$style(type = "text/css", "#LGfits {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Coefficients",div(style = "width:600px;",DT::dataTableOutput("LG_coeffs", width = "600px")),
            tags$style(type = "text/css", "#LGcoeffs {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Prob Dens Plot",plotOutput("LG_scatplot", height = "700px", width = "100%"),
            fluidRow(numericInput("LG_fit_dc", label = "Decision Criterion", value = 0.5, min = 0, max = 1, step = 0.01)),
            fluidRow(div(style = "width:600px;",DT::dataTableOutput("LG_confuse", width = "600px"))),
            uiOutput("LG_confuse_text"))))),
                
      tabPanel("XGBCL: Feat Select",fluidRow(column(9,DT::dataTableOutput('XGBCL_select'))),
            tags$style(type = "text/css", "#xgbselecttable {height: calc(100vh - 70px) !important;}")),
                
      tabPanel("XGBCL: Predict",
        tags$style(HTML("
          #xgbcl_pred_nav .row { flex-wrap: nowrap; }
          #xgbcl_pred_nav .row > div:first-child {
            flex: 0 0 200px !important;
            max-width: 200px !important;
            width: 200px !important;
          }
          #xgbcl_pred_nav .row > div:last-child {
            flex: 1 1 auto !important;
            max-width: calc(100% - 200px) !important;
            width: calc(100% - 200px) !important;
          }
          #xgbcl_pred_nav .nav.flex-column .nav-link { white-space: nowrap; }")),
        div(id = "xgbcl_pred_nav",
            bslib::navset_pill_list(widths = c(2, 10), well = FALSE,
            bslib::nav_panel("Results Table",DT::dataTableOutput("XGBCL_predictions"),
              tags$style(type = "text/css", "#XGBCL_predictions {height: calc(100vh - 70px) !important;}")),
            bslib::nav_panel("Hyperparameters",
              fluidRow(div(style = "width:400px;",DT::dataTableOutput("XGBCL_used_hp_pred", width = "400px"))),
                tags$style(type = "text/css", "#XGBCL_used_hp_pred {height: calc(100vh - 70px) !important;}")),
            bslib::nav_panel("Feature SHAPs",
                fluidRow(div(style = "width:400px;",DT::dataTableOutput("XGBCL_pred_shapes", width = "400px"))),
              tags$style(type = "text/css", "#XGBCL_pred_shapes {height: calc(100vh - 70px) !important;}")),
            bslib::nav_panel("Prob Dens Plot",plotOutput("XGBCL_pred_scatplot", height = "700px", width = "100%"),
              fluidRow(numericInput("XGBCL_pred_dc", label = "Decision Criterion",value = 0.5, min = 0, max = 1, step = 0.01)),
              fluidRow(div(style = "width:600px;",DT::dataTableOutput("XGBCL_pred_confuse", width = "600px"))),
                uiOutput("XGBCL_pred_confuse_text"))))),
                
      tabPanel("XGBCL: Fitting",
        tags$style(HTML("
          #xgbcl_fit_nav .row { flex-wrap: nowrap; }
          #xgbcl_fit_nav .row > div:first-child {
            flex: 0 0 200px !important;
            max-width: 200px !important;
            width: 200px !important;
          }
          #xgbcl_fit_nav .row > div:last-child {
            flex: 1 1 auto !important;
            max-width: calc(100% - 200px) !important;
            width: calc(100% - 200px) !important;
          }
          #xgbcl_fit_nav .nav.flex-column .nav-link { white-space: nowrap; }")),
        div(id = "xgbcl_fit_nav",
          bslib::navset_pill_list(widths = c(2, 10), well = FALSE,
          bslib::nav_panel("Results Table",DT::dataTableOutput("XGBCL_fits"),
            tags$style(type = "text/css", "#XGBCL_fits {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Hyperparameters",
            fluidRow(div(style = "width:400px;",DT::dataTableOutput("XGBCL_used_hp", width = "400px"))),
              tags$style(type = "text/css", "#XGBCL_used_hp {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Feature SHAPs",
            fluidRow(div(style = "width:400px;",DT::dataTableOutput("XGBCL_shapes", width = "400px"))),
              tags$style(type = "text/css", "#XGBCL_shapes {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Prob Dens Plot",
            plotOutput("XGBCL_scatplot", height = "700px", width = "100%"),
            fluidRow(numericInput("XGBCL_dec_crit", label = "Decision Criterion",value = 0.5, min = 0, max = 1, step = 0.01)),
            fluidRow(div(style = "width:600px;",DT::dataTableOutput("XGBCL_confuse", width = "600px"))),
            uiOutput("XGBCL_confuse_text"))))),
                
      tabPanel("EN: Predict",
        tags$style(HTML("
          #EN_pred_nav .row { flex-wrap: nowrap; }
          #EN_pred_nav .row > div:first-child {
            flex: 0 0 200px !important;
            max-width: 200px !important;
            width: 200px !important;
          }
          #EN_pred_nav .row > div:last-child {
            flex: 1 1 auto !important;
            max-width: calc(100% - 200px) !important;
            width: calc(100% - 200px) !important;
          }
          #EN_pred_nav .nav.flex-column .nav-link { white-space: nowrap; }")),
        div(id = "EN_pred_nav",
          bslib::navset_pill_list(widths = c(2, 10), well = FALSE,
            bslib::nav_panel("Results Table",DT::dataTableOutput("EN_preds"),
              tags$style(type = "text/css", "#EN_preds {height: calc(100vh - 70px) !important;}")),
            bslib::nav_panel("Coefficients",
              fluidRow(div(style = "width:400px;",DT::dataTableOutput("EN_pred_coeffs", width = "400px"))),
              tags$style(type = "text/css", "#EN_pred_coeffs {height: calc(100vh - 70px) !important;}")),
            bslib::nav_panel("Scatterplot",
              plotlyOutput("EN_pred_scatplot", height = "700px", width = "100%"),
              fluidRow(column(3, numericInput("EN_pred_stand", label = "Regulatory Standard",value = 3, min = 0, max = 5, step = 0.01)),
                column(3, numericInput("EN_pred_dc", label = "Decision Criterion",value = 3, min = 0, max = 5, step = 0.01))),
              fluidRow(div(style = "width:600px;",DT::dataTableOutput("EN_pred_confuse", width = "600px"))),
              uiOutput("EN_pred_confuse_text")),
            bslib::nav_panel("Lineplot",plotlyOutput("EN_pred_lineplot", height = "700px", width = "100%")),
            bslib::nav_panel("Residual Scatter",plotlyOutput("EN_pred_resid_scatter", height = "800px", width = "100%"))))),
      
      tabPanel("EN: Fitting",
        tags$style(HTML("
          #EN_fit_nav .row > div:first-child {
            flex: 0 0 200px !important;
            max-width: 200px !important;
            width: 200px !important;
          }
          
          #EN_fit_nav .row > div:last-child {
            flex: 1 1 auto !important;
            max-width: calc(100% - 200px) !important;
            width: calc(100% - 200px) !important;
          }
          
          #EN_fit_nav .nav.flex-column .nav-link { white-space: nowrap; }")),
            div(id = "EN_fit_nav",
              bslib::navset_pill_list(widths = c(2, 10), well = FALSE,
                bslib::nav_panel("Results Table",DT::dataTableOutput("EN_fits"),
                  tags$style(type = "text/css", "#ENfits {height: calc(100vh - 70px) !important;}")),
                bslib::nav_panel("Coefficients",
                  fluidRow(div(style = "width:400px;",DT::dataTableOutput("EN_coeffs", width = "400px"))),
                    tags$style(type = "text/css", "#EN_coeffs {height: calc(100vh - 70px) !important;}")),
                bslib::nav_panel("Scatterplot",
                  plotlyOutput("EN_scatplot", height = "700px", width = "100%"),
                  fluidRow(column(3, numericInput("EN_stand", label = "Regulatory Standard",value = 3, min = 0, max = 5, step = 0.01)),
                    column(3, numericInput("EN_dec_crit", label = "Decision Criterion",value = 3, min = 0, max = 5, step = 0.01))),
                  fluidRow(div(style = "width:600px;",DT::dataTableOutput("EN_confuse", width = "600px"))),
                  uiOutput("EN_confuse_text")),
                bslib::nav_panel("Lineplot",plotlyOutput("EN_lineplot", height = "700px", width = "100%")),
                bslib::nav_panel("Residual Scatter",plotlyOutput("EN_resid_scatplot", height = "800px", width = "100%"))))),
                
    tabPanel("XGB: Feat Select",fluidRow(column(9,DT::dataTableOutput('XGB_select'))),
        tags$style(type = "text/css", "#xgbselecttable {height: calc(100vh - 70px) !important;}")),
                
    tabPanel("XGB: Predict",
      tags$style(HTML("
        #xgb_pred_nav .row { flex-wrap: nowrap; }
        #xgb_pred_nav .row > div:first-child {
          flex: 0 0 200px !important;
          max-width: 200px !important;
          width: 200px !important;
        }
        #xgb_pred_nav .row > div:last-child {
          flex: 1 1 auto !important;
          max-width: calc(100% - 200px) !important;
          width: calc(100% - 200px) !important;
        }
        #xgb_pred_nav .nav.flex-column .nav-link { white-space: nowrap; }")),
      div(id = "xgb_pred_nav",
        bslib::navset_pill_list(widths = c(2, 10), well = FALSE,
          bslib::nav_panel("Results Table",DT::dataTableOutput("XGB_predictions"),
            tags$style(type = "text/css", "#XGB_predictions {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Hyperparameters",
            fluidRow(div(style = "width:400px;",DT::dataTableOutput("XGB_used_hp_pred", width = "400px"))),
              tags$style(type = "text/css", "#XGB_used_hp_pred {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Feature SHAPs",
            fluidRow(div(style = "width:400px;",DT::dataTableOutput("XGB_pred_shapes", width = "400px")))),
          bslib::nav_panel("Scatterplot",plotlyOutput("XGB_pred_scatplot", height = "800px", width = "100%"),
            fluidRow(column(3, numericInput("XGB_pred_stand", label = "Regulatory Standard", value = 3, min = 0, max = 5, step = 0.01)),
              column(3, numericInput("XGB_pred_dc",label = "Decision Criterion", value = 3, min = 0, max = 5, step = 0.01))),
            fluidRow(div(style = "width:600px;",DT::dataTableOutput("XGB_pred_confuse", width = "600px"))),
            uiOutput("XGB_pred_confuse_text")),
          bslib::nav_panel("Lineplot",plotlyOutput("XGB_pred_lineplot", height = "700px", width = "100%")),
          bslib::nav_panel("Residual Scatter",plotlyOutput("XGB_pred_resid_scatplot", height = "800px", width = "100%"))))),
    
    tabPanel("XGB: Fitting",
      tags$style(HTML("
        #xgb_fit_nav .row { flex-wrap: nowrap; }
        #xgb_fit_nav .row > div:first-child {
          flex: 0 0 200px !important;
          max-width: 200px !important;
          width: 200px !important;
        }
        #xgb_fit_nav .row > div:last-child {
          flex: 1 1 auto !important;
          max-width: calc(100% - 200px) !important;
          width: calc(100% - 200px) !important;
        }
        #xgb_fit_nav .nav.flex-column .nav-link { white-space: nowrap; }")),
      div(id = "xgb_fit_nav",
        bslib::navset_pill_list(widths = c(2, 10), well = FALSE,
          bslib::nav_panel("Results Table",DT::dataTableOutput("XGB_fits"),
            tags$style(type = "text/css", "#xgbfits {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Hyperparameters",
            fluidRow(div(style = "width:400px;",DT::dataTableOutput("XGB_used_hp", width = "400px"))),
              tags$style(type = "text/css", "#XGB_used_hp {height: calc(100vh - 70px) !important;}")),
          bslib::nav_panel("Feature SHAPs",
            fluidRow(div(style = "width:400px;",DT::dataTableOutput("XGB_shapes", width = "400px")))),
          bslib::nav_panel("Scatterplot",plotlyOutput("XGB_scatplot", height = "800px", width = "100%"),
            fluidRow(column(3, numericInput("XGB_stand", label = "Regulatory Standard", value = 3, min = 0, max = 5, step = 0.01)),
              column(3, numericInput("XGB_dec_crit",label = "Decision Criterion", value = 3, min = 0, max = 5, step = 0.01))),
            fluidRow(div(style = "width:600px;",DT::dataTableOutput("XGB_confuse", width = "600px"))),
            uiOutput("XGB_confuse_text")),
          bslib::nav_panel("Lineplot",plotlyOutput("XGB_lineplot", height = "700px", width = "100%")),
          bslib::nav_panel("Residual Scatter",plotlyOutput("XGB_resid_scatplot", height = "800px", width = "100%"))))))))