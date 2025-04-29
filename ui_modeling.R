ModelingPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "modelingsidepanel",
    width = 3,
    tags$style(type = "text/css", "#modelingsidepanel {width: 350px !important;}"),
    
    checkboxGroupInput(
      "coves_to_use",
      "Features to Use:",
      choices = "",
      inline = TRUE
    ),
    
    tags$hr(style = "border-color: #2c3e50;"),
    
    bs_accordion(id = "Modeling") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "General Options",
        content = card(
          fluidRow(column(12, tags$h4("Left-Censored Limits"))),
          fluidRow(column(4,numericInput("lc_lowval", label='Lower', value = 0, min=0)),
                   column(4, numericInput("lc_upval", label='Upper', value = 3, min=0)),
                   column(4)),
          fluidRow(column(12, tags$h4("Right-Censored Limits"))),
          fluidRow(column(5,numericInput("rc_lowval", label='Lower', value = 1000, min=1)),
                   column(5, numericInput("rc_upval", label='Upper', value = 10000, min=1)),
                   column(2)),
          fluidRow(align="left",column(6, numericInput("train_prop",  label="Train Prop", value = 0.75, min=0,max=1,step=0.05)),
                   column(6, numericInput("MC_runs",  label="Monte Carlo Runs", value = 10, min=1,step=1))),
          fluidRow(align="left",column(6, checkboxInput("loggy", "Log Response", FALSE)),
                   column(6, checkboxInput("randomize", "Shuffle Data", FALSE))),
          fluidRow(align="left",column(12, numericInput("model_seed",  label="Seed", value = 1234, min=1,step=1))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
    tags$hr(style = "border-color: #2c3e50;"),
    
    bs_accordion(id = "Discreet_Techniques") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Logistic Regression",
        content = card(
            fluidRow(column(12,checkboxInput("LG_standard", "Standardize Features", FALSE))),
            fluidRow(column(12,checkboxInput("binarize", "Create Binary Response", TRUE))),
            fluidRow(column(5, numericInput("binarize_crit_value", label = "Threshold", value = 2,step=0.25))),
            fluidRow(column(12,actionButton("run_pred_lg", "LG Predictions", style = 'width:160px; padding:2px;'))),
            fluidRow(column(12,actionButton("run_fitted_lg", "LG Final Fit", style = 'width:160px; padding:2px;'))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
    tags$hr(style = "border-color: #2c3e50;"),
    
    bs_accordion(id = "Continuous_Techniques") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Elastic Net",
        content = card(
          fluidRow(
            column(12,actionButton("elastic_pred", "Model Predictions", style = 'width:180px; padding:4px;'))),
          fluidRow(
            column(12,actionButton("elastic_fit", "Final Fitting", style = 'width:150px; padding:4px;'))))) %>%
      
      bs_append (
        title = "XGBoost", content= card(
          fluidRow(class="shortrow",
                   column(6,div(style = 'height:15px;',checkboxInput("xgb_standardize", "Stnrdz Features", FALSE))),
                   column(6,actionButton("xgb_params", "HP Values", style = 'background-color:#eee; width:90px; padding:2px; vertical-align: -12px;'))),
          fluidRow(class="shortrow",column(12,div(style="height:15px;",tags$hr(style = "border-color: #2c3e50;")))),
          fluidRow(column(6,align="left",actionButton("run_xgb_select", "Feature Selection", style = 'width:130px; padding:2px; vertical-align: -40px')),
                   column(6, div(style = "display: inline-block;",numericInput("test_weight", label = "Test Weight", value = 0.65, min = 0, max=1, step=0.05)))),
          # fluidRow(column(12,align="right",actionButton("xgb_select_cancel", "Cancel", style = 'width:90px; padding:2px;'))),
          fluidRow(class="shortrow",column(12,div(style="height:15px;",tags$hr(style = "border-color: #2c3e50;")))),
          fluidRow(column(12,actionButton("xgb_optimize_HP", "HP Optimization", style = 'background-color:#eee; width:130px; padding:2px;'))),
          fluidRow(column(12,actionButton("run_xgb_predict", "Model Predictions", style = 'width:130px; padding:2px;'))),
          fluidRow(class="shortrow",column(12,div(style="height:15px;", tags$hr(style = "border-color: #2c3e50;")))),
          fluidRow(column(12,align="left",actionButton("xgb_final_fitting", "Final Fitting", style = 'width:130px; padding:2px;')))),
        tags$head(tags$style(".shortrow{height:15px;}"))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c())),
  
  mainPanel = mainPanel(
    width = 9,
    id = "modeling_output",
    tabsetPanel(id = "modeling_tabs",
                tabPanel("LG: Predict",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('LG_preds'),
                                                    tags$style(type = "text/css", "#LGpreds {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Coefficients",fluidRow(column(4,DT::dataTableOutput('LG_pred_coeffs'))),
                                                    tags$style(type = "text/css", "#LGpredcoeffs {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot", fluidRow(column(2,numericInput("LG_pred_dc", label = "Decision Criterion", value = 0.5, min = 0, max=1, step=0.01))),
                                                    plotOutput("LG_pred_scatplot", height="700px",width="100%"),
                                                    fluidRow(column(7,DT::dataTableOutput('LG_pred_confuse'))),
                                                    uiOutput("LG_pred_confuse_text")))),
                tabPanel("LG: Fitting",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('LG_fits'),
                                                    tags$style(type = "text/css", "#LGfits {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Coefficients",fluidRow(column(4,DT::dataTableOutput('LG_fit_coeffs'))),
                                                    tags$style(type = "text/css", "#LGpredcoeffs {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot", fluidRow(column(2,numericInput("LG_fit_dc", label = "Decision Criterion", value = 0.5, min = 0, max=1, step=0.01))),
                                                    plotOutput("LG_fit_scatplot", height="700px",width="100%"),
                                                    fluidRow(column(7,DT::dataTableOutput('LG_fit_confuse'))),
                                                    uiOutput("LG_fit_confuse_text")))),
                tabPanel("EN: Predict",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('EN_preds'),
                                                    tags$style(type = "text/css", "#ENpreds {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Coefficients",fluidRow(column(4,DT::dataTableOutput('EN_pred_coeffs'))),
                                                    tags$style(type = "text/css", "#ENpredcoeffs {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot", fluidRow(column(2,numericInput("EN_pred_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                                            column(2,numericInput("EN_pred_dc", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
                                                    plotlyOutput("EN_pred_scatplot", height="700px",width="100%"),
                                                    fluidRow(column(7,DT::dataTableOutput('EN_pred_confuse'))),
                                                    uiOutput("EN_pred_confuse_text")),
                                          nav_panel("Lineplot", plotlyOutput("EN_pred_lineplot", height="700px",width="100%")),
                                          nav_panel("Residual Scatter", plotlyOutput("EN_pred_resid_scatter", height="800px",width="100%")))),
                tabPanel("EN: Fitting",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('EN_fits'),
                                                    tags$style(type = "text/css", "#ENfits {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Coefficients",fluidRow(column(4,DT::dataTableOutput('EN_coeffs'))),
                                                    tags$style(type = "text/css", "#ENcoeffs {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot", fluidRow(column(2,numericInput("EN_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                                            column(2,numericInput("EN_dec_crit", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
                                                    plotlyOutput("EN_scatplot", height="800px",width="100%"),
                                                    fluidRow(column(7,DT::dataTableOutput('EN_confuse'))),
                                                    uiOutput("EN_confuse_text")),
                                          nav_panel("Lineplot", plotlyOutput("EN_lineplot", height="700px",width="100%")),
                                          nav_panel("Residual Scatter", plotlyOutput("EN_resid_scatplot", height="800px",width="100%")))),
                tabPanel("XGB: Feat Select",fluidRow(column(9,DT::dataTableOutput('xgb_select'))),
                         tags$style(type = "text/css", "#xgbselecttable {height: calc(100vh - 70px) !important;}")),
                tabPanel("XGB: HP Optim",fluidRow(column(12,DT::dataTableOutput('xgb_optim_hp'))),
                         tags$style(type = "text/css", "#xgboptimhp {height: calc(100vh - 70px) !important;}")),
                tabPanel("XGB: Predict",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('xgb_predictions'),
                                                    tags$style(type = "text/css", "#xgb_preds {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("HP Values",fluidRow(column(10,DT::dataTableOutput('xgb_used_hp'))),
                                                    tags$style(type = "text/css", "#xgbusedhp {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot", fluidRow(column(2,numericInput("xgb_pred_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                                            column(2,numericInput("xgb_pred_dc", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
                                                    plotlyOutput("xgb_pred_scatplot", height="800px",width="100%"),
                                                    fluidRow(column(7,DT::dataTableOutput('xgb_pred_confuse'))),
                                                    uiOutput("xgb_pred_confuse_text")),
                                          nav_panel("Lineplot", plotlyOutput("xgb_pred_lineplot", height="700px",width="100%")),
                                          nav_panel("Residual Scatter", plotlyOutput("xgb_pred_resid_scatplot", height="800px",width="100%")))),
                tabPanel("XGB: Fitting",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('xgb_fits'),
                                                    tags$style(type = "text/css", "#xgb_preds {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("HP Values",fluidRow(column(10,DT::dataTableOutput('xgb_used_hp_fit'))),
                                                    tags$style(type = "text/css", "#xgbusedhpfit {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("SHAP Values", fluidRow(column(4,DT::dataTableOutput('xgb_shapes')))),
                                          nav_panel("Scatterplot", fluidRow(column(2,numericInput("xgb_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                                            column(2,numericInput("xgb_dec_crit", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
                                                    plotlyOutput("xgb_scatplot", height="800px",width="100%"),
                                                    fluidRow(column(7,DT::dataTableOutput('xgb_confuse'))),
                                                    uiOutput("xgb_confuse_text")),
                                          nav_panel("Lineplot", plotlyOutput("xgb_lineplot", height="700px",width="100%")),
                                          nav_panel("Residual Scatter", plotlyOutput("xgb_resid_scatplot", height="800px",width="100%")))),
    )
  )
)