source("bs_multi.R")

DataPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "datasidepanel",
    width = 3,
    tags$style(type = "text/css", "#datasidepanel {height: calc(100vh - 70px) !important; width: 385px !important;}"),
    
    fluidRow(
      column(12,div(style = "display: inline-block;",inputPanel(selectInput("IDasDate",label = "ID/Date Format",
            selected ="-",choices = c("-","MDY","YMD","MDYHM")))),
            div(style = "display: inline-block; margin-left: 1px; margin-right: 1px; vertical-align: -30px;", 
                checkboxInput("header", label = "Header?", TRUE)))),
    radioButtons(inline=T,"sep","Separator",
                               choices = c(Comma = ",",Semicolon = ";",Space = " ",Tab = "\t"),
                               selected = ","),
    div(
      fileInput("file1", "Select your data file", buttonLabel = "Browse",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ), style="font-size:80%; font-family:Arial; width: 350px;"
    ),
    
    fluidRow(
      column(6,inputPanel(selectInput("id",label = "ID Column",selected ="-",choices = c("-")))),
      column(6,inputPanel(selectInput("col_props",label = "Column Properties",selected ="-",choices = c("-"))))),
    
    fluidRow(
      column(12,actionButton("impute_check", "Impute Features"),actionButton("restore", "Restore Input Data"))),
    
    bs_accordion(id="plotting") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (title="Plotting", content = card(
        
        div(style = "height: 425px",
            
            # fluidRow(
            #   column(6,h5("Plot Output Tab"))
            # ),
            # fluidRow(
            #   column(6, selectInput(
            #   "send_plot",
            #   label = NULL,
            #   selected =
            #     "Plot1",
            #   choices = c("Plot1", "Plot2", "Plot3","Plot4"))),
            #   column(6, actionButton("plot_save", "Save Plot", style='width: 75px; padding:2px;'))),
            
            fluidRow(
              column(5,selectInput("scatterx",label = "Scatter X",selected ="-",choices = c("-"))),
              column(7,selectInput("scattery",label = "Scatter Y",selected ="-",choices = c("-")))),
            
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
            fluidRow(column(12,actionButton("corr_check", "Feature Correlations", style='width: 170px; padding:8px;')))))) %>%
      
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
  
  mainPanel = mainPanel(width=9, id = "data_main_panel",
                        tabsetPanel(id = "data_tabs",
                                    tabPanel("Data Table",DT::dataTableOutput('data'),
                                             tags$style(type = "text/css", "#userdatatable {height: calc(100vh - 70px) !important;}")),
                                    tabPanel("Correlations",actionButton("save_corrr", "Save Plot", style='width: 100px; padding:2px;'),plotOutput("corrplot",width="100%",height="700px")),
                                    tabPanel("Raincloud",actionButton("save_rainn", "Save Plot", style='width: 100px; padding:2px;'),
                                             plotOutput("rainplot", height = "800px", width="100%")),
                                    tabPanel("Line Plot",actionButton("save_linee", "Save Plot", style='width: 100px; padding:2px;'),
                                             plotlyOutput("lineplott", height="750px",width="100%")),
                                    tabPanel("Scatterplot",actionButton("save_scatt", "Save Plot", style='width: 100px; padding:2px;'),
                                             plotlyOutput("scatplot", height="900px",width="70%")))
  )
)