DataPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "datasidepanel",
    width = 3,
    tags$style(type = "text/css", "#datasidepanel {height: calc(100vh - 70px) !important; width: 385px !important;}"),
    
    fluidRow(column(6,inputPanel(selectInput("IDasDate",label = "ID/Date Format",selected ="MDY",choices = c("MDY","YMD","MDYHM","Numeric","Character")))),
             column(6,disabled(actionButton("restore", "Restore Inputs", class = "btn-default custom-btn", style='width: 120px; padding:8px; vertical-align: -33px;')))),
    fluidRow(column(12,radioButtons(inline=T,"sep","Separator",choices = c("Comma" = ",","Semicolon" = ";","Space" = " ","Tab" = "\t"),selected = ","))),
    
    div(fileInput("file1", "Select your data file", buttonLabel = "Browse",accept = c("text/csv",
                  "text/comma-separated-values,text/plain",".csv",".xlsx")), style="font-size:80%; font-family:Arial; width: 350px;"),
    h5(HTML("<i>Response Variable Censor Values</i>")),
    fluidRow(column(6,numericInput("lc_val", "Left-Censored", value=-9999)),
             column(6,numericInput("rc_val", "Right-Censored", value=9999))),
    fluidRow(column(6,disabled(inputPanel(selectInput("set_column_props",label = "Column Properties",selected ="-",choices = c("-"))))),
             column(6,radioButtons(inline=F,"select_choice",label="",choices = c("Change Response"="Change_Response","Edit Cells"="Edit_Cells",
                              "[Dis/En]able Rows" = "D/E_Rows"),selected = "Change_Response"))),
    fluidRow(column(6,disabled(actionButton("ignore_rows", "Disable Selected Rows", class = "btn-default custom-btn",  style='padding: 6px;'))),
             column(6,disabled(actionButton("enable_rows", "Enable Selected Rows", class = "btn-default custom-btn",  style='padding: 6px;')))),

    
    bs_accordion(id="plotting") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (title="Feature Processing", content = card(
            
        fluidRow(column(12,disabled(actionButton("corr_check", "Correlations",class = "btn-default custom2-btn", style = 'width:125px !important; padding:3px !important;')))),
        fluidRow(column(6,disabled(actionButton("pca_check", "PCA", class = "btn-default custom-btn",  style='width: 110px; vertical-align: -38px;'))),
                 column(6,class="align-center", numericInput("num_axes", "# PCA Axes", value=2, min=2,max=20,step=1))),
        fluidRow(column(6,disabled(actionButton("run_iso_forest","Outliers: IsoForest", class = "btn-default custom-btn",  style='width: 150px; align: left; vertical-align: -38px;'))),
                 column(6,numericInput("iso_ndim", "IF Dimensions", value=2, min=1,max=10,step=1))))) %>%
      
      bs_append (title="Plotting", content = card(
        
        div(style = "height: 400px",
            h5(HTML("<i>Censored Response Data</i>")),
            fluidRow(column(12,radioButtons(inline=T,"cens_choice",label=NULL,choices = c(Hide = "hide",Use = "use",Replace = "replace"),selected = "hide"))),
            fluidRow(column(6,numericInput("lc_replace", "Replace LC", value=-1, step=0.1)),
                     column(6,numericInput("rc_replace", "Replace RC", value=100, step=0.1))),
            
            fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;")),

            fluidRow(column(6,selectInput("scatterx",label = "Scatter X",selectize=FALSE, selected ="-",choices = c("-"))),
                     column(6,selectInput("scattery",label = "Scatter Y",selectize=FALSE, selected ="-",choices = c("-")))),
            
            fluidRow(column(12,inputPanel(selectInput("rainplot",label = "Raincloud Plot",selectize=FALSE, selected ="-",choices = c("-"))))),
            
            fluidRow(column(12,inputPanel(selectInput("lineplot",label = "Line/Time Series Plot",selectize=FALSE, selected ="-",choices = c("-")))))))) %>%
      
      bs_append (title="Wind/Wave/Current Decomposition", content = card(
        
        fluidRow(column(12,inputPanel(selectInput("speed",label = "Speed",selectize=FALSE, selected ="-",choices = c("-"))))),
        
        fluidRow(column(12,inputPanel(selectInput("direct",label = "Direction",selectize=FALSE, selected ="-",choices = c("-"))))),
        
        h5(HTML("<i>Name for A-Component</i>")),
        
        fluidRow(column(12,textInput("A_name", label=NULL,value = "AComp"))),
        
        h5(HTML("<i>Name for O-Component</i>")),
        
        fluidRow(column(12,textInput("O_name", label=NULL,value = "OComp"))),

        fluidRow(column(12,actionButton("create", "Create A/O Components", class = "btn-default custom-btn",  style='width:185px; padding:5px;'))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
      fluidRow(column(12,downloadButton("save_project", "Save Project File"))),
      fluidRow(column(12,fileInput("load_project", "Load Project/Prediction File", buttonLabel = "Browse", accept = ".RData")))
  ),
  
  mainPanel = mainPanel(width=9, id = "data_main_panel",
                        tabsetPanel(id = "data_tabs",
                                    tabPanel("Data Table",DT::dataTableOutput('data'),
                                             tags$style(type = "text/css", "#userdatatable {height: calc(100vh - 70px) !important;}")),
                                    tabPanel("PCA Results",
                                             fluidRow(column(12,DT::dataTableOutput('PCA_coeffs')),
                                                      column(12,DT::dataTableOutput('PCA_summary')))),
                                    tabPanel("PCA Data Table",DT::dataTableOutput('PCAdata'),tags$style(type = "text/css", "#pcatables {height: calc(100vh - 70px) !important;}")),
                                    tabPanel("IsoForest Outliers",DT::dataTableOutput('iso_outliers'),
                                             tags$style(type = "text/css", "#iso_outliers {height: calc(100vh - 70px) !important;}")),
                                    tabPanel("Correlations",plotOutput("corrplot",width="100%",height="700px")),
                                    tabPanel("Raincloud",plotOutput("rainplot", height = "700px", width="100%")),
                                    tabPanel("Line Plot",plotlyOutput("lineplott", height="700px",width="100%")),
                                    tabPanel("Scatterplot",plotlyOutput("scatplot", height="700px",width="100%")))
  )
)