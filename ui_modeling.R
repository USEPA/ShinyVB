source("bs_multi.R")

ModelingPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "modelingsidepanel",
    width = 3,
    tags$style(type = "text/css", "#modelingsidepanel {width: 350px !important;}"),
    
    checkboxGroupInput(
      "coves_to_use",
      "Covariates to Use:",
      choices = "",
      inline = TRUE
    ),
    
    tags$hr(style = "border-color: #2c3e50;"),
    
    bs_accordion(id = "Modeling") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Modeling Options",
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
            column(5, numericInput("train_prop",  label="Training Proportion", value = 0.75, min=0,max=1,step=0.05)),
            column(7)),
          fluidRow(align="left",
            column(12, numericInput("MC_runs",  label="Monte Carlo Runs", value = 10, min=1,step=1))),
          fluidRow(align="left",
            column(12, checkboxInput("loggy", "Log Response", FALSE))),
          fluidRow(align="left",
            column(12, checkboxInput("randomize", "Randomize Data", FALSE)))
          )) %>%
      
      bs_append (
        title = "LARS",
        content = card(
          
          fluidRow(
            column(12,checkboxInput("normalize", "Normalize Covariates", TRUE))
          ),
          fluidRow(
            column(6, inputPanel(
              selectInput(
                "lars_tech",
                label = "Fitting Technique",
                selected ="lasso",
                choices = c("lasso","lar","forward.stagewise","stepwise"))))
          ),
          fluidRow(
            column(12, numericInput("max_lars_steps",  label="Maximum Steps", value = 250, min=1,step=1))
          ),
          fluidRow(
            column(12,actionButton("lars_coeff", "Covariate Importance", style = 'width:160px; padding:2px;'))),
          fluidRow(
            column(12,actionButton("lars_uncert", "Predictive Performance", style = 'width:170px; padding:2px;'))
          ))) %>%
      
      bs_append (
        title = "Logistic Regression",
        content = card(
          
          fluidRow(
            column(12,checkboxInput("normalize", "Normalize Covariates", TRUE))
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
                choices = c("Probability","Value"))))
            ),
          fluidRow(
            column(12,actionButton("logist_analysis", "Logistic Analysis", style = 'width:160px; padding:2px;')))
          )) %>%
      
      bs_append (
        title = "XGBoost", content= card(
          fluidRow(
            column(12,align="left",actionButton("xgb_hyper_ranges", "Hyperparameter Optimization", style = 'background-color:#eee; width:200px; padding:2px;'))),
          fluidRow(
            column(12,align="left",actionButton("xgb_params", "Set Hyperparameters", style = 'background-color:#eee; width:150px; padding:2px;'))),
          fluidRow(
            column(12,checkboxInput("xgb_standardize", "Min/Max Standardization", TRUE))),
          fluidRow(
            column(12,align="left",actionButton("xgb_select", "Covariate Selection", style = 'width:140px; padding:2px;'))),
          # fluidRow(
          #   tags$head(
          #     tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
          #       #inline .form-group { display: table-row;}")),
          #   column(12,tags$div(id="inline",numericInput("test_weight", label = "Test Weight:", value = 0.65, min = 0, max=1, step=0.05)))),
          fluidRow(
            column(12,numericInput("test_weight", label = "Test Weight", value = 0.65, min = 0, max=1, step=0.05))),
          fluidRow(
            column(12,align="left",actionButton("xgb_uncert", "Predictive Performance", style = 'width:170px; padding:2px;')))))),
  
  mainPanel = mainPanel(
    width = 9,
    id = "modeling_output",
    tabsetPanel(id = "modeling_tabs",
      tabPanel("General Plots",plotOutput("plot", height="100%",width="100%")),
      tabPanel("LARS: Covariates", DT::dataTableOutput('lars_coeffs'),
                tags$style(type = "text/css", "#larscoeffstable {height: calc(100vh - 70px) !important;}")),
      tabPanel("LARS: Pred Perform", "LARS Prediction Uncertainty"),
      tabPanel("Logistic: Covariates", "Logistic Regression Covariate Importance"),
      tabPanel("Logistic: Pred Perform", "Logistic Regression Prediction Uncertainty"),
      tabPanel("XGB: Hyperparm Optim",DT::dataTableOutput('xgb_hyper'),
                tags$style(type = "text/css", "#xgbhypertable {height: calc(100vh - 70px) !important;}")),
      tabPanel("XGB: Covariates",DT::dataTableOutput('xgb_select'),
                tags$style(type = "text/css", "#xgbselecttable {height: calc(100vh - 70px) !important;}")),
      tabPanel("XGB: Pred Perform", "XGB Prediction Uncertainty")
    )
  )
)