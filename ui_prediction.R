source("bs_multi.R")

PredictionPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "predictionsidepanel",
    width = 2,
    tags$style(type = "text/css", "#datasidepanel {height: calc(100vh - 70px) !important;}"),
    
    bs_accordion(id = "prediction") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (
        title = "LARS Models",
        content = card(
          inputPanel(
            selectInput(
              "rainplot",
              label = "Raincloud Plot",
              selected =
                "-",
              choices = c("-")
            )
          ),
          
          actionButton("lars_out", "Lars Output", style = 'width:150px; padding:5px;')
        )
      ) %>%
      
      bs_append (
        title = "XGB Models",
        content = card(
          inputPanel(
            selectInput(
              "direct",
              label = "Direction",
              selected =
                "-",
              choices = c("-")
            )
          ),
          
          actionButton("xgb_out", "XGB Output", style = 'width:150px; padding:5px;')
        )
      ) %>%
      
      bs_accordion_multi(multi = FALSE, open = c())
    
  ),
  
  mainPanel = mainPanel(
    width = 10,
    id = "prediction",
    DT::dataTableOutput('prediction'),
    tags$style(type = "text/css", "#userdatatable {height: calc(100vh - 70px) !important;}")
  )
)