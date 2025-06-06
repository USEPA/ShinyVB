MapPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(id="mapsidepanel",width=3,
                              #tags$style(type = "text/css", "#mapsidepanel {width: 350px !important;}"),
                              tags$h4("Beach Orientation Calculator"),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;"),
                              htmlOutput("bo_text"),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 6px; margin-bottom: 6px;"),
                              fluidRow(
                                column(4,tags$h4("Beach Orientation:")),
                                column(8,div(style="width:60px;",verbatimTextOutput("beach_orient",placeholder = TRUE)))),
                              tags$head(tags$style(HTML("#beach_orient {title:borient;width: 70px;height: 45px; position:left;}"))),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 4px; margin-bottom: 4px;"),
                              fluidRow(column(6,h5(HTML("<i>Zoom Level >= 11</i>"))),
                                       column(6,h5(HTML("<i>Zoom Level >= 13</i>")))),
                              fluidRow(column(6, switchInput("show_stations", label="Show Stations?", labelWidth=100, value = FALSE, onLabel = "Yes", offLabel = "No", size = "small")),
                                       column(6, switchInput("show_shorelines", label="Show Beaches?", labelWidth=100, value = FALSE, onLabel = "Yes", offLabel = "No", size = "small"))),
                              fluidRow(
                                column(12,tags$h5("Monitoring stations and beach shorelines are only shown in the visible area of the map. If you pan to a different area, deselect then reselect
                                                  the 'Show' buttons.")))),

  
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