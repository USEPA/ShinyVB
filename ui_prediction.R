PredictionPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "predictionsidepanel",
    width = 3,
    tags$style(type = "text/css", "#predictionsidepanel {height: calc(100vh - 70px) !important; width: 385px !important;}"),
    
    radioButtons(
      inputId = "model_choice",
      label = "Model to Use:",
      choices = "None",
      selected = "None"
    ),
  
  numericInput("num_preds", label='Number of Predictions', value = 2,min=2,max=1000,step=1),
  numericInput("conf_bound", label='Confidence Bound', value = 0.95,min=0.1,max=0.99,step=0.01),
  actionButton("make_preds", "Make Predictions", class = "btn-default custom-btn", style = 'width:130px; padding:6px;'),
  fluidRow(column(12,textOutput("pca_model_text"))),
  fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;")),
  div(fileInput("pred_file", "Import Data File", buttonLabel = "Browse",accept = c("text/csv",
        "text/comma-separated-values,text/plain",".csv",".xlsx")), style="font-family:Arial; width: 350px;"),
  fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;")),
  fluidRow(column(12,div(downloadButton("save_prediction", "Save Prediction File")),style="padding: 8px;")),
  fluidRow(column(12,div(fileInput("load_prediction", "Load Project/Prediction File", buttonLabel = "Browse", accept = ".RData"),
                               style="font-family:Arial; width: 350px;")))),
  
  mainPanel = mainPanel(width=9, id = "prediction_main_panel",
                    fluidRow(column(12,"",DT::dataTableOutput('pd_data'))),
                    fluidRow(column(12,textOutput("resid_text"))))
)
