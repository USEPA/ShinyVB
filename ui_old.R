# Define UI
ui = fluidPage(
  
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: lightgray;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: gray; color:white}
  ")),

  tags$style(HTML("
  .shiny-input-panel {
    padding: 0px 0px;
    margin-top: 0px;
    margin-bottom: 0px;
    background-color:#ecf0f1;
    border: 0px solid #e3e3e3;
    border-radius: 0px;
  }

  .btn-default {
    color: #000000;
  }
  .btn-default:hover {
    background-color: #2c3e50;
    color: #18bc9c;
  }             ")),
  
  navbarPage(
    title = "Virtual Beach",
    fluid = TRUE,
    collapsible = TRUE,
    theme = shinytheme("flatly"),

    tabPanel(
      title = "Map",
      sidebarLayout(
        sidebarPanel = sidebarPanel(id="mapsidepanel",width=2,tags$h4("Beach Orientation Calculator"),
          tags$hr(style = "border-color: darkblue;"),
          htmlOutput("bo_text"),
          tags$hr(style = "border-color: darkblue;"),
          "Beach Orientation:",
          verbatimTextOutput("beach_orient",placeholder=T)),
          
        mainPanel = mainPanel(id="mapmainpanel",width=10,
          leafletOutput("map"),
          tags$style(type = "text/css", "#map {height: calc(100vh - 70px) !important;}")
        ),
      )
    ),
    
    tabPanel(
      tags$head(tags$style(HTML(".form-control { height:auto; padding:5px 5px;}"))),
      tags$style(type='text/css', ".selectize-input { padding: 1px; margin-bottom: 0; min-height: 0;} .selectize-dropdown { line-height: 13px; }"),
      title = "Data",
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "datasidepanel",
          width = 2,
          tags$style(type = "text/css", "#datasidepanel {height: calc(100vh - 70px) !important;}"),
          
          div(
            fileInput("file1", "Select your data file", buttonLabel = "Find", width="400px",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ), style="font-size:80%; font-family:Arial;"
          ),
          
          #   Input: Checkbox if file has header ----
          checkboxInput("header", "Header", TRUE),
          
          # Input: Select separator ----
          radioButtons(
            inline=T,
            "sep",
            "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Space = " ",
              Tab = "\t"
            ),
            selected = ","
          ),
          
          inputPanel(selectInput(
            "id",
            label = "ID Column",
            selected =
              "-",
            choices = c("-"))),
          
          actionButton("restore", "Restore Input Data", style='padding:2px; font-size:80%; margin-right:20px'),
          
          actionButton("impute", "Impute Covariates", style='padding:2px; font-size:80%'),
            
        inputPanel(
          selectInput(
            "rainplot",
            label = "Raincloud Plot",
            selected =
              "-",
            choices = c("-"))),
        
        inputPanel(
          selectInput(
            "scatterx",
            label = "Scatterplot X",
            selected =
              "-",
            choices = c("-"))),
        
        inputPanel(
          selectInput(
            "scattery",
            label = "Scatterplot Y",
            selected =
              "-",
            choices = c("-"))),
          
      inputPanel(
        selectInput(
          "speed",
          label = "Wind/Current/Wave Speed",
          selected =
            "-",
          choices = c("-"))),
      
      inputPanel(
        selectInput(
          "direct",
          label = "Wind/Current/Wave Direction",
          selected =
            "-",
          choices = c("-"))),
      
      textInput(
        "A_name",
        "Enter Name for A Component", value = "AComp"),
      
      textInput(
        "O_name",
        "Enter Name for O Component", value = "OComp"),
      
      actionButton("create", "Create A/O Components", style='padding:2px; font-size:80%')),
            
    mainPanel = mainPanel(width=10,
                          id = "userdatatable",
                          DT::dataTableOutput('data'),
                          tags$style(type = "text/css", "#userdatatable {height: calc(100vh - 70px) !important;}")
    )
  )
),
    
    navbarMenu("Modeling",
      tabPanel("Options",
              tabsetPanel(
                tabPanel(title="LARS/LASSO",p("This is where you'll find LARS options"),textOutput("lars")),
                tabPanel(title="XGBoost",p("This is where you'll find XGBoost options"),textOutput("xgb")))),
      tabPanel(title="Output",p("This is where you'll find Modeling output"),textOutput("output")),
      tabPanel(title="Prediction",p("This is where you'll find model prediction stuff"),textOutput("prediction")))
  )
)