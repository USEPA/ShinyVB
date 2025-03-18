MapPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(id="mapsidepanel",width=2,tags$h4("Beach Orientation Calculator"),
                              tags$hr(style = "border-color: darkblue;"),
                              htmlOutput("bo_text"),
                              tags$hr(style = "border-color: darkblue;"),
                              
                              fluidRow(
                                column(6, tags$h4("Beach Orientation:"),
                                       tags$style(type="text/css", "{text-align:right;}")),
                                column(3, verbatimTextOutput("beach_orient",placeholder=T)),
                                column(3))),
  
  mainPanel = mainPanel(id="mapmainpanel",width=10,
                        add_busy_gif(src = "https://jeroen.github.io/images/banana.gif",
                                     height = 70,width = 70),                  
                        leafletOutput("map"),
                        tags$style(type = "text/css", "#map {height: calc(100vh - 70px) !important;}")
  ),
)