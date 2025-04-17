MapPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(id="mapsidepanel",width=3,tags$style(type = "text/css"), #, "#mapsidepanel {width: 350px !important;}"),
                              tags$h4("Beach Orientation Calculator"),
                              tags$hr(style = "border-color: darkblue; width:300px !important;"),
                              htmlOutput("bo_text"),
                              tags$hr(style = "border-color: darkblue;"),
                              fluidRow(
                                column(4,tags$h4("Beach Orientation:")),
                                column(8,div(style="width:60px;",verbatimTextOutput("beach_orient",placeholder = TRUE)))),
                              tags$head(tags$style(HTML("#beach_orient {title:borient;width: 70px;height: 45px; position:left;}")))),
  
  mainPanel = mainPanel(id="mapmainpanel",width=9,
                        add_busy_gif(src = "https://jeroen.github.io/images/banana.gif",
                                     height = 70,width = 70),                  
                        leafletOutput("map"),
                        tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}")
  ),
)