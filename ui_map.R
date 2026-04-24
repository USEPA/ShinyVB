MapPanel = div(id="map_flex", style = "display:flex; gap:8px; align-items:flex-start;",
        div(id = "mapsidepanel", style = paste("flex:0 0 450px; min-width:450px; max-width:450px;",
        "background-color:#e9ecef; padding:10px; box-sizing:border-box;",
        "height: calc(100vh - 70px); height: calc(100dvh - 70px);","overflow-y:auto;"),
        
        tags$h4("Beach Orientation Calculator"),
        tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;"),
        htmlOutput("bo_text"),
        tags$hr(style = "border-color: #2c3e50; margin-top: 6px; margin-bottom: 6px;"),
        fluidRow(column(4,tags$h4("Beach Orientation:")),
          column(8,div(style="width:60px;",verbatimTextOutput("beach_orient",placeholder = TRUE)))),
          tags$head(tags$style(HTML("#beach_orient {title:borient;width: 70px;height: 45px; position:left;}"))),
          tags$hr(style = "border-color: #2c3e50; margin-top: 4px; margin-bottom: 4px;"),
        fluidRow(column(6,h5(HTML("<i>Zoom Level >= 11</i>"))),
          column(6,h5(HTML("<i>Zoom Level >= 12</i>")))),
        fluidRow(column(6, switchInput("show_stations", label="Show Stations?", labelWidth=100, value = FALSE, onLabel = "Yes", offLabel = "No", size = "small")),
          column(6, switchInput("show_shorelines", label="Show Beaches?", labelWidth=100, value = FALSE, onLabel = "Yes", offLabel = "No", size = "small"))),
        fluidRow(column(12,tags$h5("Monitoring stations and beach shorelines are only shown in the visible area of the map.
         If you pan to a different area, deselect then reselectthe 'Show' buttons.")))),

        div(id = "mapmainpanel",
          style = "flex:1 1 auto; min-width:0; position:relative;",
          leafletOutput("map"),
          tags$div(id = "text-box", "Zoom Level: ", textOutput("zoom_level", inline = TRUE)),
          tags$style(HTML("
            #text-box {
              position: absolute;
              top: 20px;
              left: 50px;
              z-index: 1001;
              background-color: rgba(255, 255, 255, 0.8);
              padding: 10px;
              border-radius: 5px;
              box-shadow: 0 0 5px #888888;
              }")),
          tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}")))