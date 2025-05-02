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
          h5(HTML("<i>Left-Censored Limits (Non-Detects)</i>")),
          fluidRow(column(4,numericInput("lc_lowval", label='Lower', value = 0, min=0)),
                   column(4, numericInput("lc_upval", label='Upper', value = 3, min=0)),
                   column(4)),
          h5(HTML("<i>Right-Censored Limits (TNTC)</i>")),
          fluidRow(column(5,numericInput("rc_lowval", label='Lower', value = 1000, min=1)),
                   column(5, numericInput("rc_upval", label='Upper', value = 10000, min=1)),
                   column(2)),
          fluidRow(column(6, numericInput("train_pct",  label="% Training", value = 75, min=1,max=100,step=1)),
                   column(6, numericInput("MC_runs",  label="Monte Carlo Runs", value = 10, min=1,step=1))),
          fluidRow(column(6, numericInput("num_folds",  label="CV Folds", value = 5, min=2,max=20,step=1)),
                   column(6, numericInput("model_seed",  label="Rnd Seed", value = 1234, min=1,step=1))),
          fluidRow(column(6, checkboxInput("loggy", "Log Response", FALSE)),
                   column(6, checkboxInput("randomize", "Shuffle Data", FALSE))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
    tags$hr(style = "border-color: #2c3e50;"),
    h5(HTML("<i>Binary Response Techniques</i>"),style="text-align:center"),
    
    bs_accordion(id = "Discreet_Techniques") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Logistic Regression",
        content = card(
            fluidRow(column(12,checkboxInput("LG_standard", "Standardize Features", FALSE))),
            fluidRow(column(12,checkboxInput("LG_binarize", "Create Binary Response", TRUE))),
            fluidRow(column(5, numericInput("LG_binarize_crit_value", label = "Threshold", value = 2,step=0.25))),
            fluidRow(column(12,actionButton("run_pred_LG", "Predictions", style = 'width:100px; padding:2px;'))),
            fluidRow(column(12,actionButton("run_fitted_LG", "Fitting", style = 'width:100px; padding:2px;'))))) %>%
      
      bs_append (
        title = "XGB Classifier",
        content = card(
          fluidRow(column(6,checkboxInput("XGBCL_standard", "Standardize Features", FALSE))),
          fluidRow(column(6,checkboxInput("XGBCL_binarize", "Create Binary Response", TRUE)),
                   column(6, numericInput("XGBCL_binarize_crit_value", label = "Threshold", value = 2,step=0.25))),
          fluidRow(column(12,actionButton("XGBCL_params", "HP Values", style = 'background-color:#eee; width:90px; padding:2px; vertical-align: -12px;'))),
          fluidRow(class="shortrow",column(12,div(style="height:15px;",tags$hr(style = "border-color: #2c3e50;")))),
          fluidRow(column(12,actionButton("XGBCL_optimize_HP", "HP Optimization", style = 'background-color:#eee; width:130px; padding:2px;'))),
          fluidRow(column(12,actionButton("run_pred_XGBCL", "Predictions", style = 'width:100px; padding:2px;'))),
          fluidRow(column(12,actionButton("run_fit_XGBCL", "Fitting", style = 'width:100px; padding:2px;'))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
    tags$hr(style = "border-color: #2c3e50;"),
    h5(HTML("<i>Continuous Response Techniques</i>"),style="text-align:center"),
    
    bs_accordion(id = "Continuous_Techniques") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Elastic Net",
        content = card(
          fluidRow(column(12,checkboxInput("EN_standard", "Standardize Features", FALSE))),
          fluidRow(column(12,actionButton("EN_pred", "Predictions", style = 'width:100px; padding:4px;'))),
          fluidRow(column(12,actionButton("EN_fit", "Fitting", style = 'width:100px; padding:4px;'))))) %>%
      
      bs_append (
        title = "XGBoost", content= card(
          fluidRow(class="shortrow",
                   column(6,div(style = 'height:15px;',checkboxInput("XGB_standardize", "Stnrdz Features", FALSE))),
                   column(6,actionButton("XGB_params", "HP Values", style = 'background-color:#eee; width:90px; padding:2px; vertical-align: -12px;'))),
          fluidRow(class="shortrow",column(12,div(style="height:15px;",tags$hr(style = "border-color: #2c3e50;")))),
          fluidRow(column(6,align="left",actionButton("run_XGB_select", "Feature Selection", style = 'width:130px; padding:2px; vertical-align: -40px')),
                   column(6, div(style = "display: inline-block;",numericInput("test_weight", label = "Test Weight", value = 0.65, min = 0, max=1, step=0.05)))),
          # fluidRow(column(12,align="right",actionButton("XGB_select_cancel", "Cancel", style = 'width:90px; padding:2px;'))),
          fluidRow(class="shortrow",column(12,div(style="height:15px;",tags$hr(style = "border-color: #2c3e50;")))),
          fluidRow(column(12,actionButton("XGB_optimize_HP", "HP Optimization", style = 'background-color:#eee; width:130px; padding:2px;'))),
          fluidRow(column(12,actionButton("run_XGB_predict", "Predictions", style = 'width:100px; padding:2px;'))),
          fluidRow(column(12,align="left",actionButton("XGB_final_fitting", "Fitting", style = 'width:100px; padding:2px;')))),
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
                                          nav_panel("Coefficients",fluidRow(column(6,DT::dataTableOutput('LG_pred_coeffs'))),
                                                    tags$style(type = "text/css", "#LGpredcoeffs {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot",plotOutput("LG_pred_scatplot", height="700px",width="100%"),
                                                    fluidRow(column(2,numericInput("LG_pred_dc", label = "Decision Criterion", value = 0.5, min = 0, max=1, step=0.01))),
                                                    fluidRow(column(7,DT::dataTableOutput('LG_pred_confuse'))),
                                                    uiOutput("LG_pred_confuse_text")))),
                tabPanel("LG: Fitting",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('LG_fits'),
                                                    tags$style(type = "text/css", "#LGfits {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Coefficients",fluidRow(column(6,DT::dataTableOutput('LG_coeffs'))),
                                                    tags$style(type = "text/css", "#LGcoeffs {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot",plotOutput("LG_scatplot", height="700px",width="100%"),
                                                    fluidRow(column(2,numericInput("LG_fit_dc", label = "Decision Criterion", value = 0.5, min = 0, max=1, step=0.01))),
                                                    fluidRow(column(7,DT::dataTableOutput('LG_confuse'))),
                                                    uiOutput("LG_confuse_text")))),
                tabPanel("XGBCL: HP Optim",fluidRow(column(9,DT::dataTableOutput('XGBCL_optim_hp'))),
                         tags$style(type = "text/css", "#xgbcloptimhp {height: calc(100vh - 70px) !important;}")),
                tabPanel("XGBCL: Predict",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('XGBCL_predictions'),
                                                    tags$style(type = "text/css", "#XGBCLpreds {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("HP Values",fluidRow(column(10,DT::dataTableOutput('XGBCL_used_hp_pred'))),
                                                    tags$style(type = "text/css", "#xgbclusedhppred {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("SHAP Values",fluidRow(column(6,DT::dataTableOutput('XGBCL_pred_shapes'))),
                                                    tags$style(type = "text/css", "#XGBCLpredshapes {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot",plotOutput("XGBCL_pred_scatplot", height="700px",width="100%"),
                                                    fluidRow(column(2,numericInput("XGBCL_pred_dc", label = "Decision Criterion", value = 0.5, min = 0, max=1, step=0.01))),
                                                    fluidRow(column(7,DT::dataTableOutput('XGBCL_pred_confuse'))),
                                                    uiOutput("XGBCL_pred_confuse_text")))),
                tabPanel("XGBCL: Fitting",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('XGBCL_fits'),
                                                    tags$style(type = "text/css", "#XGBCLfits {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("HP Values",fluidRow(column(10,DT::dataTableOutput('XGBCL_used_hp'))),
                                                    tags$style(type = "text/css", "#xgbclusedhp {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("SHAP Values",fluidRow(column(6,DT::dataTableOutput('XGBCL_shapes'))),
                                                    tags$style(type = "text/css", "#XGBCLshapes {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot",plotOutput("XGBCL_scatplot", height="700px",width="100%"),
                                                    fluidRow(column(2,numericInput("XGBCL_dec_crit", label = "Decision Criterion", value = 0.5, min = 0, max=1, step=0.01))),
                                                    fluidRow(column(7,DT::dataTableOutput('XGBCL_confuse'))),
                                                    uiOutput("XGBCL_confuse_text")))),
                tabPanel("EN: Predict",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('EN_preds'),
                                                    tags$style(type = "text/css", "#ENpreds {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Coefficients",fluidRow(column(4,DT::dataTableOutput('EN_pred_coeffs'))),
                                                    tags$style(type = "text/css", "#ENpredcoeffs {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("Scatterplot",plotlyOutput("EN_pred_scatplot", height="700px",width="100%"),
                                                    fluidRow(column(2,numericInput("EN_pred_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                             column(2,numericInput("EN_pred_dc", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
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
                                          nav_panel("Scatterplot",plotlyOutput("EN_scatplot", height="800px",width="100%"),
                                                    fluidRow(column(2,numericInput("EN_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                             column(2,numericInput("EN_dec_crit", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
                                                    fluidRow(column(7,DT::dataTableOutput('EN_confuse'))),
                                                    uiOutput("EN_confuse_text")),
                                          nav_panel("Lineplot", plotlyOutput("EN_lineplot", height="700px",width="100%")),
                                          nav_panel("Residual Scatter", plotlyOutput("EN_resid_scatplot", height="800px",width="100%")))),
                tabPanel("XGB: Feat Select",fluidRow(column(9,DT::dataTableOutput('XGB_select'))),
                         tags$style(type = "text/css", "#xgbselecttable {height: calc(100vh - 70px) !important;}")),
                tabPanel("XGB: HP Optim",fluidRow(column(9,DT::dataTableOutput('XGB_optim_hp'))),
                         tags$style(type = "text/css", "#xgboptimhp {height: calc(100vh - 70px) !important;}")),
                tabPanel("XGB: Predict",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('XGB_predictions'),
                                                    tags$style(type = "text/css", "#xgbpreds {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("HP Values",fluidRow(column(10,DT::dataTableOutput('XGB_used_hp_pred'))),
                                                    tags$style(type = "text/css", "#xgbusedhppred {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("SHAP Values", fluidRow(column(5,DT::dataTableOutput('XGB_pred_shapes')))),
                                          nav_panel("Scatterplot",plotlyOutput("XGB_pred_scatplot", height="800px",width="100%"),
                                                    fluidRow(column(2,numericInput("XGB_pred_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                             column(2,numericInput("XGB_pred_dc", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
                                                    fluidRow(column(7,DT::dataTableOutput('XGB_pred_confuse'))),
                                                    uiOutput("XGB_pred_confuse_text")),
                                          nav_panel("Lineplot", plotlyOutput("XGB_pred_lineplot", height="700px",width="100%")),
                                          nav_panel("Residual Scatter", plotlyOutput("XGB_pred_resid_scatplot", height="800px",width="100%")))),
                tabPanel("XGB: Fitting",
                         navset_pill_list(widths = c(2,10), well=F,
                                          nav_panel("Results Table",DT::dataTableOutput('XGB_fits'),
                                                    tags$style(type = "text/css", "#xgbfits {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("HP Values",fluidRow(column(10,DT::dataTableOutput('XGB_used_hp'))),
                                                    tags$style(type = "text/css", "#xgbusedhp {height: calc(100vh - 70px) !important;}")),
                                          nav_panel("SHAP Values", fluidRow(column(4,DT::dataTableOutput('XGB_shapes')))),
                                          nav_panel("Scatterplot",plotlyOutput("XGB_scatplot", height="800px",width="100%"),
                                                    fluidRow(column(2,numericInput("XGB_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                             column(2,numericInput("XGB_dec_crit", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
                                                    fluidRow(column(7,DT::dataTableOutput('XGB_confuse'))),
                                                    uiOutput("XGB_confuse_text")),
                                          nav_panel("Lineplot", plotlyOutput("XGB_lineplot", height="700px",width="100%")),
                                          nav_panel("Residual Scatter", plotlyOutput("XGB_resid_scatplot", height="800px",width="100%")))),
    )
  )
)