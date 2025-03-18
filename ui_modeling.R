source("bs_multi.R")

ModelingPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "modelingsidepanel",
    width = 2,
    tags$style(type = "text/css", "#datasidepanel {height: calc(100vh - 70px) !important;}"),
    
    checkboxGroupInput(
      "coves_to_use",
      "Covariates to Use:",
      choices = "",
      inline = TRUE
    ),
    
    tags$hr(style = "border-color: #2c3e50;"),
    
    bs_accordion(id = "modeling") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "Modeling Options",
        content = card(
          
          fluidRow(
            column(6, HTML('Non-Detect Limit')),
            column(6, HTML('Number of MC Runs'))
          ),
          fluidRow(
            column(4, numericInput("nd_val", label=NULL, value = 3, min=1)),
            column(2,p()),
            column(4, numericInput("MC_runs",  label=NULL, value = 10, min=1)),
            column(2,p())
          ),
          fluidRow(
            column(6, HTML('Upper Measure Limit')),
            column(6,  HTML('UML Multiplier'))
          ),
          fluidRow(
            column(4, numericInput("tntc_val",  label=NULL, value = 1000, min=1)),
            column(2,),
            column(4, numericInput("tntc_multy",  label=NULL, value = 10, min=1)),
            column(2)
          ),
          
          checkboxInput("loggy", "Response Variable Logged?", FALSE),
          
          checkboxInput("randomize", "Randomize Dataset Before Analysis?", FALSE)
          
        )) %>%
      
      bs_append (
        title = "LARS",
        content = card(
          
          fluidRow(
            column(12,checkboxInput("standardize", "Standardize the Covariates?", TRUE))
          ),
          
          fluidRow(
            column(9, inputPanel(
              selectInput(
                "lars_tech",
                label = "LARS Technique",
                selected ="lasso",
                choices = c("lasso","lar","forward.stagewise","stepwise")))),
            column(3)
          ),
          fluidRow(
            column(4, numericInput("max_steps",  label="Maximum Steps", value = 250, min=1)),
            column(8)
          ),
          fluidRow(
          column(6,actionButton("lars_coeff", "Estimate Coeffs", style = 'width:140px; padding:2px;')),
          column(6,actionButton("lars_uncert", "Predictive Success", style = 'width:140px; padding:2px;'))
          ))) %>%
      
      bs_append (
        title = "XGB Modeling",
        content = card(
          fluidRow(
            column(12,actionButton("xgb_params", "Hyperparameters", style = 'width:100px; padding:5px;'))
          ),
          actionButton("xgb_select", "Coefficient Estimation", style = 'width:150px; padding:5px;'),
          actionButton("xgb_uncert", "Prediction Uncertainty", style = 'width:150px; padding:5px;')
        )
      ) %>%
      
      bs_accordion_multi(multi = FALSE, open = c())
    
  ),
  
  mainPanel = mainPanel(
    width = 10,
    id = "output",
    DT::dataTableOutput('model_output'),
    tags$style(type = "text/css", "#userdatatable {height: calc(100vh - 70px) !important;}")
  )
)