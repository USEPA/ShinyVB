DataPanel = div(id="data_flex", style = "display:flex; gap:8px; align-items:flex-start;",
            div(id = "datasidepanel", style = paste("flex:0 0 400px; min-width:400px; max-width:400px;",
              "background-color:#e9ecef; padding:10px; box-sizing:border-box;",
              "height: calc(100vh - 70px); height: calc(100dvh - 70px);","overflow-y:auto;"),
    tags$style(type = "text/css", "#datasidepanel {height: calc(100vh - 70px) !important;}"),
    
    fluidRow(column(6,inputPanel(selectInput("ID_Format",label = "ID Format",selected ="Date",choices = c("Date","Numeric","Character")))),
             column(6,disabled(actionButton("restore", "Restore Inputs", class = "btn-default custom-btn", style='width: 120px; padding:5px; vertical-align: -33px;')))),
    
    div(fileInput("file1", "Select your data file", buttonLabel = "Browse",accept = c("text/csv",
                  "text/comma-separated-values,text/plain",".csv",".xlsx")), style="font-size:80%; font-family:Arial; width: 350px;"),
    h5(HTML("<i>Response Variable Censor Values</i>")),
    fluidRow(column(6,numericInput("lc_val", "Left-Censored", value=-9999)),
             column(6,numericInput("rc_val", "Right-Censored", value=9999))),
    fluidRow(column(6,disabled(inputPanel(selectInput("set_column_props",label = "Column Properties",selected ="-",choices = c("-"))))),
             column(6,radioButtons(inline=F,"select_choice",label="",choices = c("Change Response"="Change_Response","Edit Cells"="Edit_Cells",
                              "[Dis/En]able Rows" = "D/E_Rows"),selected = "Change_Response"))),
    fluidRow(column(6,disabled(actionButton("ignore_rows", "Disable Selected Rows", class = "btn-default custom-btn",  style='padding: 5px;'))),
             column(6,disabled(actionButton("enable_rows", "Enable Selected Rows", class = "btn-default custom-btn",  style='padding: 5px;')))),

    
    bs_accordion(id="plotting") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (title="Feature Processing", content = card(
            
        fluidRow(column(5,disabled(actionButton("corr_check", "Correlations",class = "btn-default custom2-btn", style = 'width:130px !important; padding:2px !important;'))),
                 column(7,disabled(actionButton("transforms", "Transformations",class = "btn-default custom2-btn", style = 'width:140px !important; padding:2px !important;')))),
        fluidRow(column(5,disabled(actionButton("interacts", "Interactions",class = "btn-default custom2-btn", style = 'width:120px !important; padding:2px !important;'))),
                 column(7,style = "padding-left: 8px;",tags$style(HTML("#r_thresh { height:35px !important; padding: 2px 2px !important; text-align: center !important; font-size: 16px !important; }
                  .inline-label { margin-right: 8px; font-size: 16px !important; }")),
                  div(style = "display:flex; align-items:center; gap:5px;",
                    tags$label("Crit Corr", `for` = "r_thresh", class = "inline-label"),
                    div(style = "width: 75px; margin-top:3px;",   # nudge the box down
                      numericInput("r_thresh", label = NULL, value = 0.7, min = 0, max = 1, step = 0.01))))),
        fluidRow(column(6, disabled(actionButton("pca_check", "PCA", class = "btn-default custom-btn",style='width: 150px; vertical-align: -38px;'))),
                column(6, class="align-center",div(style = "margin-top:4px;",numericInput("num_axes", "# Axes", value=2, min=2, max=20, step=1)))),
        fluidRow(column(6, disabled(actionButton("run_iso_forest", "IsoForest Leverage", class = "btn-default custom-btn",
            style='width: 150px; align: left; vertical-align: -38px;'))),
                column(6,div(style = "margin-top:4px;",numericInput("iso_ndim", "# Dimensions", value=2, min=1, max=10, step=1)))))) %>%
      
      bs_append (title="Plotting", content = card(
        
        div(style = "height: 420px",
            h5(HTML("<i>Censored Response Data</i>")),
            fluidRow(column(12,radioButtons(inline=T,"cens_choice",label=NULL,choices = c(Hide = "hide",Use = "use",Replace = "replace"),selected = "hide"))),
            fluidRow(column(6,numericInput("lc_replace", "LC Replace Value", value=0, step=0.1)),
                     column(6,numericInput("rc_replace", "RC Replace Value", value=100, step=0.1))),
            
            fluidRow(tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;")),

            fluidRow(column(6,selectInput("scatterx",label = "Scatter X",selectize=FALSE, selected ="-",choices = c("-"))),
                     column(6,selectInput("scattery",label = "Scatter Y",selectize=FALSE, selected ="-",choices = c("-")))),
            
            fluidRow(column(12,inputPanel(selectInput("rainplot",label = "Raincloud Plot",selectize=FALSE, selected ="-",choices = c("-"))))),
            
            fluidRow(column(12,inputPanel(selectInput("lineplot_feature",label = "Line/Time Series Plot",selectize=FALSE, selected ="-",choices = c("-")))))))) %>%
      
      bs_append (title="Wind/Wave/Current Decomposition", content = card(
        
        fluidRow(column(8,selectInput("speed", label = "Magnitude", selectize = FALSE, selected = "-", choices = c("-")),
                  selectInput("direct", label = "Direction", selectize = FALSE, selected = "-", choices = c("-"))),
                column(4,radioButtons("component_type", label = NULL,choices = c("Wind", "Currents", "Waves"),selected = "Wind", inline = FALSE))),
        fluidRow(div(style = "display: flex; align-items: center; height: 100%;",
            column(6, numericInput("beach_angle", "Beach Orientation", value = 0, min = 0, max = 359, step = 1)),
            column(6, actionButton("create_ao", "Create A/O",class = "btn-default custom-btn",style = "width:130px; padding:10px; margin-top:10px;")))))) %>%
      
      bs_accordion_multi(multi=FALSE,open=c()),
    
      fluidRow(column(12,actionButton("save_project_data", "Save Project File"))),
      fluidRow(column(12,fileInput("load_file", "Load Project/Prediction File", buttonLabel = "Browse", accept = ".RData"))),

  ),
  
  div(id = "data_main_panel", style = "flex:1 1 auto; min-width:0;",
                        tabsetPanel(id = "data_tabs",
                                    tabPanel("Data Table",DT::dataTableOutput('data'),
                                             tags$style(type = "text/css", "#userdatatable {height: calc(100vh - 70px) !important;}")),
                                    tabPanel("PCA Results",
                                             fluidRow(column(12,DT::dataTableOutput('PCA_coeffs')),
                                                      column(12,DT::dataTableOutput('PCA_summary')))),
                                    tabPanel("PCA Data Table",DT::dataTableOutput('PCAdata'),tags$style(type = "text/css", "#pcatables {height: calc(100vh - 70px) !important;}")),
                                    tabPanel("IsoForest Leverage",DT::dataTableOutput('iso_leverage'),
                                             tags$style(type = "text/css", "#iso_leverage {height: calc(100vh - 70px) !important;}")),
                                    tabPanel("Correlations",uiOutput("correlations_ui")),
                                    tabPanel("Transformations",
                                      div(id = "trans_container",
                                        div(id = "trans_table_wrap",DT::dataTableOutput("trans_table")),
                                        div(id = "trans_side",actionButton("apply_transforms", "Add/Remove Selected Terms",class = "btn-primary", style = "width: 100%;"))),
                                      tags$style(type = "text/css", HTML("
                                        /* Side-by-side layout that hugs content (prevents sidebar from flying to far right) */
                                        #trans_container {
                                          display: inline-flex;
                                          align-items: flex-start;
                                          gap: 12px;
                                        }
                                        /* Table area: fixed width to keep header/body aligned */
                                        #trans_table_wrap {
                                          width: 800px;
                                          min-width: 800px;
                                          max-width: 800px;
                                        }
                                        #trans_table_wrap table.dataTable { table-layout: fixed; }
                                        #trans_table_wrap .dataTables_scrollHeadInner { width: 800px !important; }
                                        #trans_table_wrap .dataTables_scrollHeadInner table { width: 800px !important; }
                                        #trans_table_wrap .dataTables_scrollBody > table { width: 800px !important; }

                                        /* Center headers over cells */
                                        #trans_table_wrap .dataTables_scrollHead th,
                                        #trans_table_wrap table.dataTable thead th { text-align: center !important; }
                                        #trans_table_wrap table.dataTable tbody td { text-align: center; }

                                        /* Prevent table wrapper from covering sidebar */
                                        #trans_table { height: auto !important; }
                                        #trans_table_wrap .dataTables_wrapper { overflow: visible !important; }
                                        #trans_table_wrap .dataTables_scroll  { overflow: visible !important; }

                                        /* Sticky sidebar just to the right of the table */
                                        #trans_side {
                                          flex: 0 0 180px;
                                          position: sticky;
                                          top: 12px;
                                          align-self: flex-start;
                                          z-index: 10;
                                        }"))),
                                    
                                    tabPanel("Interactions",
                                      div(id = "inter_container",
                                        div(id = "inter_table_wrap",DT::dataTableOutput("interactions_table")),
                                        div(id = "interactions_side",actionButton("add_interactions", "Add/Remove Selected Terms",class = "btn-primary", style = "width: 100%;"))),
                                      tags$style(type = "text/css", HTML("
                                        #inter_container {
                                          display: inline-flex;
                                          align-items: flex-start;
                                          gap: 12px;
                                        }
                                        #inter_table_wrap {
                                          width: 800px;
                                          min-width: 800px;
                                          max-width: 800px;
                                        }
                                        #inter_table_wrap table.dataTable { table-layout: fixed; }
                                        #inter_table_wrap .dataTables_scrollHeadInner { width: 800px !important; }
                                        #inter_table_wrap .dataTables_scrollHeadInner table { width: 800px !important; }
                                        #inter_table_wrap .dataTables_scrollBody > table { width: 800px !important; }

                                        /* Center headers over cells */
                                        #inter_table_wrap .dataTables_scrollHead th,
                                        #inter_table_wrap table.dataTable thead th { text-align: center !important; }
                                        #inter_table_wrap table.dataTable tbody td { text-align: center; }

                                        /* Prevent table wrapper from covering sidebar */
                                        #interactions_table { height: auto !important; }
                                        #inter_table_wrap .dataTables_wrapper { overflow: visible !important; }
                                        #inter_table_wrap .dataTables_scroll  { overflow: visible !important; }

                                        /* Sticky sidebar just to the right of the table */
                                        #interactions_side {
                                          flex: 0 0 180px;
                                          position: sticky;
                                          top: 12px;
                                          align-self: flex-start;
                                          z-index: 10;
                                          }"))),
                                    tabPanel("Raincloud",plotOutput("rainplot", height = "700px", width="100%")),
                                    tabPanel("Line Plot",plotlyOutput("lineplott", height="700px",width="100%")),
                                    tabPanel("Scatterplot",plotlyOutput("scatplot", height="700px",width="100%")))
  )
)