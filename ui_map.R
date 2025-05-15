MapPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(id="mapsidepanel",width=3,
                              #tags$style(type = "text/css", "#mapsidepanel {width: 350px !important;}"),
                              tags$h4("Beach Orientation Calculator"),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;"),
                              htmlOutput("bo_text"),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;"),
                              fluidRow(
                                column(4,tags$h4("Beach Orientation:")),
                                column(8,div(style="width:60px;",verbatimTextOutput("beach_orient",placeholder = TRUE)))),
                              tags$head(tags$style(HTML("#beach_orient {title:borient;width: 70px;height: 45px; position:left;}"))),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 4px; margin-bottom: 4px;"),
                              fluidRow(column(6,h5(HTML("<i>Zoom Level >= 11</i>"))),
                                       column(6,h5(HTML("<i>Zoom Level >= 13</i>")))),
                              fluidRow(column(6,actionButton("show_stations", "Show Stations", class="align-center", style='width: 130px; height: 30px;')),
                                column(6,actionButton("show_shorelines", "Show Beaches",  class="align-center",style='width: 130px; height: 30px;'))),
                              fluidRow(column(6,actionButton("clear_stations", "Clear Stations",  class="align-center", style='width: 130px; height: 30px;')),
                                column(6,actionButton("clear_shorelines", "Clear Beaches",  class="align-center",style='width: 130px; height: 30px;'))),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 4px; margin-bottom: 4px;"),
                              actionButton("retrieve_data", "Retrieve WQX Data",  class="align-center", style='width: 150px; height: 35px;')),

  
  mainPanel = mainPanel(id="mapmainpanel",width=9,
                        #add_busy_gif(src = "https://jeroen.github.io/images/banana.gif",height = 70,width = 70),                  
                        leafletOutput("map"),
                        tags$div(id = "text-box", "Zoom Level: ", textOutput("zoom_level", inline = TRUE)),
                        tags$style(HTML("#text-box {
                              position: absolute;
                              top: 80px;
                              left: 20px;
                              z-index: 1000;
                              background-color: rgba(255, 255, 255, 0.8);
                              padding: 10px;
                              border-radius: 5px;
                              box-shadow: 0 0 5px #888888;}")),
                        tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}")
  ),
)