MapPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(id="mapsidepanel",width=3,
                              #tags$style(type = "text/css", "#mapsidepanel {width: 350px !important;}"),
                              tags$h4("Beach Orientation Calculator"),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 2px; margin-bottom: 2px;"),
                              htmlOutput("bo_text"),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 2px; margin-bottom: 2px;"),
                              fluidRow(
                                column(4,tags$h4("Beach Orientation:")),
                                column(8,div(style="width:60px;",verbatimTextOutput("beach_orient",placeholder = TRUE)))),
                              tags$head(tags$style(HTML("#beach_orient {title:borient;width: 70px;height: 45px; position:left;}"))),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 2px; margin-bottom: 2px;"),
                              fluidRow(
                                column(4,actionButton("show_sites", "Show Beaches", style='width: 150px;')),
                                column(4,actionButton("show_stations", "Show Stations", style='width: 150px;')),
                                column(4,actionButton("retrieve_data", "Retrieve Data", style='width: 150px;'))),
                              ),
  
  mainPanel = mainPanel(id="mapmainpanel",width=9,
                        #add_busy_gif(src = "https://jeroen.github.io/images/banana.gif",height = 70,width = 70),                  
                        leafletOutput("map"),
                        tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}")
  ),
)
