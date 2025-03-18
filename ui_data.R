source("bs_multi.R")

DataPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "datasidepanel",
    width = 2,
    tags$style(type = "text/css", "#datasidepanel {height: calc(100vh - 70px) !important;}"),
    
    inputPanel(
          selectInput(
            "IDasDate",
            label = "ID Date Format",
            selected =
              "-",
            choices = c("-","MDY","YMD","MDYHM"))),
    
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
    
    div(
      fileInput("file1", "Select your data file", buttonLabel = "Browse",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ), style="font-size:80%; font-family:Arial; width: 350px;"
    ),
    
    inputPanel(selectInput(
      "id",
      label = "ID Column",
      selected =
        "-",
      choices = c("-"))),
    
    actionButton("impute_check", "Impute Covariates", style='width:145px; padding:5px; margin-right:10px;'),
    
    actionButton("restore", "Restore Input Data", style='width:150px; padding:5px;'),
    
    bs_accordion(id="plotting") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (title="Plotting", content = card(
        
        div(style = "height: 500px",
            
            fluidRow(
              column(12,
                     inputPanel(
                       selectInput(
                         "rainplot",
                         label = "Raincloud Plot",
                         selected =
                           "-",
                         choices = c("-"))))),
            
            fluidRow(
              column(12,
                     inputPanel(
                       selectInput(
                         "lineplot",
                         label = "Line/Time Series Plot",
                         selected =
                           "-",
                         choices = c("-"))))),
            
            fluidRow(
              column(12,
                     inputPanel(
                       selectInput(
                         "scatterx",
                         label = "Scatterplot X",
                         selected =
                           "-",
                         choices = c("-"))))),
            
            fluidRow(
              column(12,
                     inputPanel(
                       selectInput(
                         "scattery",
                         label = "Scatterplot Y",
                         selected =
                           "-",
                         choices = c("-")))))))) %>%
      
      bs_append (title="Wind/Wave/Current Decomposition", content = card(
        
        fluidRow(
          column(12,
                 inputPanel(
          selectInput(
            "speed",
            label = "Speed",
            selected =
              "-",
            choices = c("-"))))),
        
        fluidRow(
          column(12,
                 inputPanel(
          selectInput(
            "direct",
            label = "Direction",
            selected =
              "-",
            choices = c("-"))))),
        
        fluidRow(
          column(12,
                 textInput(
          "A_name",
          "Enter Name for A Component", value = "AComp"))),
        
        fluidRow(
          column(12,
                 textInput(
          "O_name",
          "Enter Name for O Component", value = "OComp"))),
        
        fluidRow(
          column(12,
                 actionButton("create", "Create A/O Components", style='width:185px; padding:5px;'))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c())
  ),
  
  mainPanel = mainPanel(width=10,
                        id = "userdatatable",
                        DT::dataTableOutput('data'),
                        tags$style(type = "text/css", "#userdatatable {height: calc(100vh - 70px) !important;}")
  )
)