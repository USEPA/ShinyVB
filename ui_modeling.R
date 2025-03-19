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
        title = "XGB Modeling", content= card(
          fluidRow(
            column(6, selectInput("tree_method",
                                  label = "Tree Method",
                                  selected ="hist",
                                  choices = c("hist","exact","approx"))),
            column(6, selectInput("xgb_tech",
                                  label = "Booster",
                                  selected ="gbtree",
                                  choices = c("gbtree","dart","gblinear")))),
          fluidRow(
            column(12,align="center",actionButton("xgb_hyper_ranges", "Hyperparameter Optimization", style = 'background-color:#eaeaea; width:200px; padding:2px;'))),
          fluidRow(
            column(12,align="center",actionButton("xgb_params", "Set Hyperparameters", style = 'background-color:#eaeaea; width:200px; padding:2px;'))),
          fluidRow(
            column(12,tags$hr(style = "border-color: #2c3e50;"))),
          fluidRow(
            column(12,align="center",actionButton("xgb_select", "Feature Selection", style = 'width:200px; padding:2px;'))),
          fluidRow(
            column(12,align="center",actionButton("xgb_uncert", "Prediction Uncertainty", style = 'width:200px; padding:2px;'))))) %>%
      
      bs_accordion_multi(multi = FALSE, open = c())
    
  ),
  
  mainPanel = mainPanel(
    width = 10,
    id = "modeling_output",
    navset_tab(id = "modeling_tabs",
      nav_panel("LARS Coefficient Results", DT::dataTableOutput('lars_coeffs'),
                tags$style(type = "text/css", "#larscoeffstable {height: calc(100vh - 70px) !important;}")),
      nav_panel("LARS Prediction Uncertainty", "LARS Prediction Uncertainty"), 
      nav_panel("XGB Hyperparameter Grid Search", DT::dataTableOutput('xgb_hyper'),
                tags$style(type = "text/css", "#xgbhypertable {height: calc(100vh - 70px) !important;}")),
      nav_panel("XGB Feature Selection", "XGB Feature Selection"),
      nav_panel("XGB Prediction Uncertainty", "XGB Prediction Uncertainty"),
    )
  )
)