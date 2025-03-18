source("bs_multi.R")
source("ui_prediction.R")
source("ui_modeling.R")
source("ui_data.R")
source("ui_map.R")

# Define UI
ui = fluidPage(

  tags$style(HTML("
  
    .tabbable > .nav > li > a                  {background-color: lightgray;  color:black}
    
    .tabbable > .nav > li[class=active]    > a {background-color: #2c3e50; color:white}

    .btn-default {
       margin: 2px 2px;
       font-size: 14px;
       color: #000000;
       border-color: #2c3e50;
       text-align: center;
    }
    .bslib-card .card-body {
       overflow:hidden;
    }
    
    .btn-default:hover {
       background-color: #2c3e50;
       color: #18bc9c;
    }
    
    .panel-heading:hover {
      background-color: #cccccc;
      color: #18bc9c;
    }
    
    .panel-body {
      background-color: #ecf0f1;
    }
    
    .shiny-input-panel {
        margin: 0px 0px;
        padding: 0px 0px;
        border: 0px solid #e3e3e3;
        background-color: #ecf0f1;
    }
    
    .modal-footer {
        text-align: center;
    }
                  
                  ")),
  
  navbarPage(
    title = "Virtual Beach",
    id="shinyVB",
    theme = shinytheme("flatly"),

    tabPanel(title = "Map",MapPanel),
    
    tabPanel(title = "Data",DataPanel),
    
    tabPanel(title = "Modeling",ModelingPanel),
    
    tabPanel(title = "Prediction",PredictionPanel)
  )
)