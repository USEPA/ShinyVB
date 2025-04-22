source("bs_multi.R")

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
          fluidRow(
            column(12, tags$h4("Left-Censored Limits"))),
          fluidRow(
            column(4,numericInput("lc_lowval", label='Lower', value = 0, min=0)),
            column(4, numericInput("lc_upval", label='Upper', value = 3, min=0)),
            column(4)),
          fluidRow(
            column(12, tags$h4("Right-Censored Limits"))),
          fluidRow(
            column(5,numericInput("rc_lowval", label='Lower', value = 1000, min=1)),
            column(5, numericInput("rc_upval", label='Upper', value = 10000, min=1)),
            column(2)),
          fluidRow(align="left",
            column(6, numericInput("train_prop",  label="Train Prop", value = 0.75, min=0,max=1,step=0.05)),
            column(6, numericInput("MC_runs",  label="Monte Carlo Runs", value = 10, min=1,step=1))),
          fluidRow(align="left",
            column(12, checkboxInput("loggy", "Log Response", FALSE))),
          fluidRow(align="left",
            column(12, checkboxInput("randomize", "Randomize Data", FALSE))),
          fluidRow(align="left",
                   column(12, numericInput("model_seed",  label="Seed", value = 1234, min=1,step=1))))) %>%
    
    bs_accordion_multi(multi=FALSE,open=c()),
    
    tags$hr(style = "border-color: #2c3e50;"),
    
    bs_accordion(id = "Discreet_Techniques") %>%
      
      bs_set_opts(panel_type = "primary") %>%

      bs_append (
        title = "Logistic Regression",
        content = card(
          
          fluidRow(
            column(12,checkboxInput("normalize", "Normalize Features", TRUE))
          ),
          fluidRow(
            column(12, numericInput("logist_train_pct",  label="Proportion Training Data", value = 0.75, min=0.25,max=1,step=0.05))
            ),
          fluidRow(
            column(6, numericInput("logist_regularz",  label="Regularization", value = 0, min=0,max=1,step=0.05)),
            column(6, numericInput("logist_mixture",  label="Lasso Penalty", value = 1, min=0,max=1,step=0.05))
          ),
          fluidRow(
            column(12, inputPanel(
              selectInput(
                "logist_engine",
                label = "Engine",
                selected ="glm",
                choices = c("glm","glmnet"))))
            ),
          fluidRow(
            column(12, inputPanel(
              selectInput(
                "logist_pred",
                label = "Prediction",
                selected ="Probability",
                choices = c("Probability","Binary"))))
            ),
          fluidRow(
            column(12,actionButton("logist_analysis", "Logistic Analysis", style = 'width:160px; padding:2px;'))))) %>%
      
    bs_accordion_multi(multi=FALSE,open=c()),
    
    tags$hr(style = "border-color: #2c3e50;"),
    
    bs_accordion(id = "Continuous_Techniques") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Elastic Net",
        content = card(
          fluidRow(
            column(12,actionButton("elastic", "Run EN", style = 'width:100px; padding:4px;'))))) %>%
      
      bs_append (
        title = "XGBoost", content= card(
          fluidRow(class="shortrow",
            column(12,div(style = 'height:15px;',checkboxInput("xgb_standardize", "Standardize Features", TRUE)))),
          fluidRow(class="shortrow",column(12,div(style="height:15px;",tags$hr(style = "border-color: #2c3e50;")))),
          fluidRow(
            column(12,div(style = "display: inline-block;",actionButton("xgb_params", "Hyperparameters", style = 'background-color:#eee; width:140px; padding:2px; margin-right:20px; vertical-align: -5px;')),
                 div(style = "display: inline-block;",numericInput("test_weight", label = "Test Weight", value = 0.65, min = 0, max=1, step=0.05)))),
          fluidRow(
            column(7,align="left",actionButton("run_xgb_select", "Feature Selection", style = 'width:130px; padding:2px;')),
            column(5,align="right",actionButton("xgb_select_cancel", "Cancel", style = 'width:90px; padding:2px;'))),
          fluidRow(class="shortrow",column(12,tags$hr(style = "border-color: #2c3e50;"))),
          fluidRow(
            column(12,align="left",actionButton("xgb_HP_and_errors", "HP Tuning and Prediction Errors", style = 'background-color:#eee; width:220px; padding:2px;'))),
          fluidRow(class="shortrow",column(12,div(style="height:15px;", tags$hr(style = "border-color: #2c3e50;")))),
          fluidRow(
            column(12,align="left",actionButton("xgb_final_fitting", "Final Fitting", style = 'background-color:#eee; width:130px; padding:2px;')))),
          tags$head(tags$style(".shortrow{height:15px;}"))) %>%
      
    bs_accordion_multi(multi=FALSE,open=c())),
  
  mainPanel = mainPanel(
    width = 9,
    id = "modeling_output",
    tabsetPanel(id = "modeling_tabs",
      tabPanel("Elastic Net",
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
                    nav_panel("Residual Scatterplot", plotlyOutput("EN_resid_scatplot", height="800px",width="100%")))),
      tabPanel("Logistic: Feat. Importance", "Logistic Regression Covariate Importance"),
      tabPanel("Logistic: Performance", "Logistic Regression Performance"),
      tabPanel("XGB: Feature Selection",fluidRow(column(9,DT::dataTableOutput('xgb_select'))),
               tags$style(type = "text/css", "#xgbselecttable {height: calc(100vh - 70px) !important;}")),
      tabPanel("XGB: HP and Errors",
               navset_pill_list(widths = c(2,10), well=F,
                    nav_panel("Results Table",DT::dataTableOutput('xgb_predictions'),
                              tags$style(type = "text/css", "#xgb_preds {height: calc(100vh - 70px) !important;}")),
                    nav_panel("Scatterplot", plotlyOutput("xgb_pred_scatplot", height="800px",width="100%")),
                    nav_panel("Lineplot", plotlyOutput("xgb_pred_lineplot", height="700px",width="100%")),
                    nav_panel("Residual Scatterplot", plotlyOutput("xgb_pred_resid_scatplot", height="800px",width="100%")))
      ),
      tabPanel("XGB: Final Fitting",
               navset_pill_list(widths = c(2,10), well=F,
                    nav_panel("Results Table",DT::dataTableOutput('xgb_fits'),
                            tags$style(type = "text/css", "#xgbfits {height: calc(100vh - 70px) !important;}")),
                    nav_panel("Scatterplot", fluidRow(column(2,numericInput("xgb_stand", label = "Standard", value = 3, min = 0, max=5, step=0.01)),
                                                      column(2,numericInput("xgb_dec_crit", label = "Decision Criterion", value = 3, min = 0, max=5, step=0.01))),
                              plotlyOutput("xgb_scatplot", height="800px",width="100%"),
                              fluidRow(column(7,DT::dataTableOutput('xgb_confuse'))),
                              uiOutput("xgb_confuse_text")),
                    nav_panel("Lineplot", plotlyOutput("xgb_lineplot", height="700px",width="100%")),
                    nav_panel("Residual Scatterplot", plotlyOutput("xgb_resid_scatplot", height="800px",width="100%")))),
    )
  )
)