renderPCAdata = function(data,
                         date_format_string,
                         ignored_rows,
                         output) {
  
  if (is.null(data)) {
    output$PCAdata = NULL
    return()
  } else {
    
    data = data.frame(data)
    
    sig_digies = 3
    
    col_names = colnames(data)
    
    all_rows = row.names(data)
    iggies = ifelse(all_rows %in% ignored_rows, 'gray', '')
    
    row_IDs = data[,1]
    
    if (date_format_string != "Non-Date") {
      
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
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatStyle(1, backgroundColor = 'lightgray') %>%
          formatStyle(2, backgroundColor = "#b0bed9") %>%
          formatRound(columns = 2:ncol(data), digits = sig_digies) %>%
          formatDate(1, date_format_string)
      })
      
    } else {
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
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatStyle(1, backgroundColor = 'lightgray') %>%
          formatStyle(2, backgroundColor = "#b0bed9") %>%
          formatRound(columns = 1:ncol(data),digits = sig_digies)
      })
    }
  }
}