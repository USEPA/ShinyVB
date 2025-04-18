source("bs_multi.R")

DataPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "datasidepanel",
    width = 3,
    tags$style(type = "text/css", "#datasidepanel {height: calc(100vh - 70px) !important; width: 385px !important;}"),
    
    fluidRow(column(12,inputPanel(selectInput("IDasDate",label = "ID/Date Format",selected ="Other",choices = c("Other","MDY","YMD","MDYHM"))))),
    fluidRow(column(12,radioButtons(inline=T,"sep","Separator",choices = c(Comma = ",",Semicolon = ";",Space = " ",Tab = "\t"),selected = ","))),
    
    div(fileInput("file1", "Select your data file", buttonLabel = "Browse",accept = c("text/csv",
                  "text/comma-separated-values,text/plain",".csv")), style="font-size:80%; font-family:Arial; width: 350px;"),
    
    fluidRow(
      column(5,disabled(actionButton("restore", "Restore Inputs", style='width: 120px; padding:8px; vertical-align: -30px;'))),
      column(7,disabled(inputPanel(selectInput("col_props",label = "Column Properties",selected ="-",choices = c("-")))))),
    fluidRow(
      column(6,numericInput("data_seed", "Random Seed", value=1234, min=1,max=1000000,step=1)),
      column(6,radioButtons(inline=T,"select_choice","Table Selection",choices = c(Features="Features",Rows="Rows"),selected = "Features"))),
    fluidRow(
      column(6,disabled(actionButton("ignore_rows", "Disable Selected Rows", style='padding: 6px;'))),
      column(6,disabled(actionButton("enable_rows", "Enable Selected Rows", style='padding: 6px;')))),
    fluidRow(column(12,tags$hr(style = "border-color: #2c3e50;"))),
    fluidRow(
      column(5,disabled(actionButton("impute_check", "Impute Features"))),
      column(1),
      column(7,disabled(actionButton("corr_check", "Feature Correlations", style='width: 175px;')))),
    fluidRow(
      column(6,disabled(actionButton("run_iso_forest","Outliers: IsoForest", style='width: 150px; align: left; vertical-align: -38px;'))),
      column(6,numericInput("iso_ndim", "IsoForest Dims", value=2, min=1,max=5,step=1))),
    
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
                         choices = c("-")))))
            ))) %>%
      
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
                                    tabPanel("Outlier Metric",DT::dataTableOutput('iso_outliers'),
                                             tags$style(type = "text/css", "#iso_outliers {height: calc(100vh - 70px) !important;}")),
                                    tabPanel("Correlations",actionButton("save_corrr", "Save Plot", style='width: 100px; padding:2px;'),
                                             plotOutput("corrplot",width="100%",height="1200px")),
                                    tabPanel("Raincloud",actionButton("save_rainn", "Save Plot", style='width: 100px; padding:2px;'),
                                             plotOutput("rainplot", height = "800px", width="100%")),
                                    tabPanel("Line Plot",actionButton("save_linee", "Save Plot", style='width: 100px; padding:2px;'),
                                             plotlyOutput("lineplott", height="750px",width="100%")),
                                    tabPanel("Scatterplot",actionButton("save_scatt", "Save Plot", style='width: 100px; padding:2px;'),
                                             plotlyOutput("scatplot", height="900px",width="70%")))
  )
)