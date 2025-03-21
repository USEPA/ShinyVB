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
            column(12, numericInput("MC_runs",  label="Monte Carlo Runs", value = 10, min=1,step=1))),
          fluidRow(align="left",
            column(12, checkboxInput("loggy", "Log Response?", FALSE))),
          fluidRow(align="left",
            column(12, checkboxInput("randomize", "Randomize Data?", FALSE)))
          )) %>%
      
      bs_append (
        title = "LARS",
        content = card(
          
          fluidRow(
            column(12,checkboxInput("standardize", "Standardize Covariates?", TRUE))
          ),
          
          fluidRow(
            column(9, inputPanel(
              selectInput(
                "lars_tech",
                label = "Fitting Technique",
                selected ="lasso",
                choices = c("lasso","lar","forward.stagewise","stepwise")))),
            column(3)
          ),
          fluidRow(
            column(12, numericInput("max_lars_steps",  label="Maximum Steps", value = 250, min=1,step=1))
          ),
          fluidRow(
            column(12,actionButton("lars_coeff", "Covariate Importance", style = 'width:160px; padding:2px;'))),
          fluidRow(
            column(12,actionButton("lars_uncert", "Predictive Uncertainty", style = 'width:170px; padding:2px;'))
          ))) %>%
      
      bs_append (
        title = "XGB Modeling", content= card(
          fluidRow(
            column(12,align="left",actionButton("xgb_hyper_ranges", "Hyperparameter Optimization", style = 'background-color:#eee; width:200px; padding:2px;'))),
          fluidRow(
            column(12,align="left",actionButton("xgb_params", "Set Hyperparameters", style = 'background-color:#eee; width:150px; padding:2px;'))),
          fluidRow(
            column(12,tags$hr(style = "border-color: #2c3e50;"))),
          fluidRow(
            column(12,align="left",actionButton("xgb_select", "Covariate Selection", style = 'width:140px; padding:2px;'))),
          fluidRow(
            column(12,align="left",actionButton("xgb_uncert", "Predictive Uncertainty", style = 'width:170px; padding:2px;'))))) %>%
      
      bs_accordion_multi(multi = FALSE, open = c())
    
  ),
  
  mainPanel = mainPanel(
    width = 9,
    id = "modeling_output",
    tabsetPanel(id = "modeling_tabs",
      tabPanel("General Plots",plotOutput("plot", height="100%",width="100%")),
      tabPanel("LARS: Covariates", DT::dataTableOutput('lars_coeffs'),
                tags$style(type = "text/css", "#larscoeffstable {height: calc(100vh - 70px) !important;}")),
      tabPanel("LARS: Pred. Uncertainty", "LARS Prediction Uncertainty"), 
      tabPanel("XGB: Hyperparameter Optimization",DT::dataTableOutput('xgb_hyper'),
                tags$style(type = "text/css", "#xgbhypertable {height: calc(100vh - 70px) !important;}")),
      tabPanel("XGB: Covariate Selection", "XGB Feature Selection"),
      tabPanel("XGB: Pred. Uncertainty", "XGB Prediction Uncertainty"),
    )
  )
)
