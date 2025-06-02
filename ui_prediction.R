PredictionPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "predictionsidepanel",
    width = 3,
    tags$style(type = "text/css", "#predictionsidepanel {height: calc(100vh - 70px) !important; width: 385px !important;}"),
    
    prettyRadioButtons(
      inputId = "model_to_use",
      label = "Model to Use:",
      choices = "NULL",
      selected = NULL,
      status = "primary",
      shape = "round",
      outline = FALSE,
      fill = FALSE,
      thick = FALSE,
      animation = NULL,
      icon = NULL,
      plain = FALSE,
      bigger = FALSE,
      inline = FALSE,
      width = NULL
    ),
  
  numericInput("num_preds", label='Number of Predictions', value = 2,min=1,max=1000,step=1),
  numericInput("conf_bound", label='Confidence Bound', value = 0.95,min=0.1,max=0.99,step=0.01),
  actionButton("make_preds", "Make Predictions", style = 'width:120px; padding:2px;'),
  div(fileInput("pred_file", "Select your prediction data file", buttonLabel = "Browse",accept = c("text/csv",
        "text/comma-separated-values,text/plain",".csv",".xlsx")), style="font-size:80%; font-family:Arial; width: 350px;")),
  
  mainPanel = mainPanel(width=9, id = "prediction_main_panel",
                    fluidRow(column(12,"",DT::dataTableOutput('pd_data'))),
                    fluidRow(column(12,"",DT::dataTableOutput('pd_pca_data'))),
                    fluidRow(column(12,verbatimTextOutput("resid_text",placeholder = TRUE))))
)