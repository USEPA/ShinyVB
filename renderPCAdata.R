renderPCAdata = function(data,
                         date_format_string,
                         output) {
  
  if (is.null(data)) {
    output$PCAdata = NULL
    return()
  } else {
    
    data = data.frame(data)
    
    sig_digies = 3
    
    if (date_format_string != "Numeric" && date_format_string != "Character") {
      
      output$PCAdata = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = 1),
            target = "column",
            mode = "single"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            dom='ltBp',
            paging = TRUE,
            pageLength = 20,
            scrollX = TRUE,
            scrollY = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "}"
            )
          )
        ) %>%
          formatStyle(1, backgroundColor = 'lightgray') %>%
          formatStyle(2, backgroundColor = "#b0bed9") %>%
          formatRound(columns = 2:ncol(data), digits = sig_digies) %>%
          formatDate(1, date_format_string)
      })
      
    } else if (date_format_string == "Numeric") {
      output$PCAdata = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = 1),
            target = "column",
            mode = "single"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            dom='ltBp',
            paging = TRUE,
            pageLength = 20,
            scrollX = TRUE,
            scrollY = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "}"
            )
          )
        ) %>%
          formatStyle(1, backgroundColor = 'lightgray') %>%
          formatStyle(2, backgroundColor = "#b0bed9") %>%
          formatRound(columns = 1:ncol(data),digits = sig_digies)
      })
    } else if (date_format_string == "Character") {
      output$PCAdata = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = 1),
            target = "column",
            mode = "single"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            dom='ltBp',
            paging = TRUE,
            pageLength = 20,
            scrollX = TRUE,
            scrollY = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "}"
            )
          )
        ) %>%
          formatStyle(1, backgroundColor = 'lightgray') %>%
          formatStyle(2, backgroundColor = "#b0bed9") %>%
          formatRound(columns = 2:ncol(data),digits = sig_digies)
      })
    }
  }
}