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
  
  fluidRow(
    column(6,numericInput("num_preds", label='# Predictions', value = 2,min=2,max=1000,step=1)),
    column(6,numericInput("conf_bound", label='Conf. Bound', value = 0.95,min=0.1,max=0.99,step=0.01))),
  actionButton("make_preds", "Make Predictions", class = "btn-default custom-btn", style = 'width:130px; padding:6px;'),
  fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;")),
  div(fileInput("pred_file", "Import Data File", buttonLabel = "Browse",accept = c("text/csv",
        "text/comma-separated-values,text/plain",".csv",".xlsx")), style="font-family:Arial; width: 350px;"),
  h5(HTML("NOTE: The first two columns in an imported prediction file must be the ID and Response Variable; any column names are acceptable,
          and these can contain missing data. However, the file must contain columns with names exactly equal to each feature name in the model;
           no particular column order is required and additional extraneous/unused columns can be present.")),
  fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;")),
  fluidRow(column(12,div(downloadButton("save_prediction", "Save Prediction File")),style="padding: 8px;")),
  fluidRow(column(12,div(fileInput("load_prediction", "Load Project/Prediction File", buttonLabel = "Browse", accept = ".RData"),
                               style="font-family:Arial; width: 350px;")))),
  
  mainPanel = mainPanel(width=9, id = "prediction_main_panel",
                    fluidRow(column(12,uiOutput("model_text"))),
                    fluidRow(column(12,textOutput("pca_model_text"))),
                    fluidRow(column(12,uiOutput("resid_text"))),
                    fluidRow(column(12,DT::dataTableOutput('pd_data'))),
                    fluidRow(column(12,DT::dataTableOutput('pd_feat_ranges'))))
)
