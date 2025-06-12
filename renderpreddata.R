renderpreddata = function(data,
                          date_format_string,
                          feat_props,
                          current_page,
                          output) {
  
  data = data.frame(data)
  sig_digies = c()
  col_names = colnames(data)
  
  for (i in 1:(ncol(data)-3)) {
    if (colnames(data)[i] %in% keys(feat_props)) {
      sig_digies = append(sig_digies, values(feat_props, keys = col_names[i])[1])
    } else {
      sig_digies = 3
    }
  }
  
  PL = 20
  
  sig_digies = c(sig_digies,3,3,3)
  
  if (date_format_string != "Numeric" && date_format_string != "Character") {
    
    col_list = seq(1, ncol(data))
    sig_digies = sig_digies[-1]
    
    output$pd_data = DT::renderDT(server = T, {
      datatable(
        data,
        rownames = F,
        extensions = 'Buttons',
        selection = list(
          selected = list(rows = NULL, cols = 1),
          target = "column",
          mode = "single"
        ),
        editable = T,
        options = list(
          autoWidth = F,
          dom='ltBp',
          paging = TRUE,
          pageLength = PL,
          displayStart = current_page * PL - PL,
          scrollX = TRUE,
          scrollY = TRUE,
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(list(
            targets = '_all', className = 'dt-center',
            createdCell = JS(
              "function(td, cellData, rowData, row, col) {",
              "$(td).css({'min-width': '75px'});",
              "}"
            )
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'pd_data', {priority: 'event'});",
            "}"
          )
        )
      ) %>%
        formatStyle(columns = 1:2, backgroundColor = "#b0bed9") %>%
        formatStyle(columns = 3:(ncol(data)-3),backgroundColor = styleEqual(-999, "red", default = "#aaeeaa")) %>%
        formatStyle(columns = (ncol(data)-2):ncol(data), backgroundColor = "lightgray") %>%
        formatRound(columns = 2:ncol(data), digits = sig_digies) %>%
        formatDate(1, date_format_string)
    })
    
  } else if (date_format_string == "Numeric") {
    output$pd_data = DT::renderDT(server = T, {
      datatable(
        data,
        rownames = F,
        extensions = 'Buttons',
        selection = list(
          selected = list(rows = NULL, cols = 1),
          target = "column",
          mode = "single"
        ),
        editable = T,
        options = list(
          autoWidth = F,
          dom='ltBp',
          paging = TRUE,
          pageLength = PL,
          displayStart = current_page * PL - PL,
          scrollX = TRUE,
          scrollY = TRUE,
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(list(
            targets = '_all', className = 'dt-center',
            createdCell = JS(
              "function(td, cellData, rowData, row, col) {",
              "$(td).css({'min-width': '75px'});",
              "}"
            )
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'pd_data', {priority: 'event'});",
            "}"
          )
        )
      ) %>%
        formatStyle(columns = 1:2, backgroundColor = "#b0bed9") %>%
        formatStyle(columns = 3:(ncol(data)-3),backgroundColor = styleEqual(-999, "red", default = "#aaeeaa")) %>%
        formatStyle(columns = (ncol(data)-2):ncol(data), backgroundColor = "lightgray") %>%
        formatRound(columns = 1:ncol(data),digits = sig_digies)
    })
  } else if (date_format_string == "Character") {
    output$pd_data = DT::renderDT(server = T, {
      datatable(
        data,
        rownames = F,
        extensions = 'Buttons',
        selection = list(
          selected = list(rows = NULL, cols = 1),
          target = "column",
          mode = "single"
        ),
        editable = T,
        options = list(
          autoWidth = F,
          dom='ltBp',
          paging = TRUE,
          pageLength = PL,
          displayStart = current_page * PL - PL,
          scrollX = TRUE,
          scrollY = TRUE,
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(list(
            targets = '_all', className = 'dt-center',
            createdCell = JS(
              "function(td, cellData, rowData, row, col) {",
              "$(td).css({'min-width': '75px'});",
              "}"
            )
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'pd_data', {priority: 'event'});",
            "}"
          )
        )
      ) %>%
        formatStyle(columns = 1:2, backgroundColor = "#b0bed9") %>%
        formatStyle(columns = 3:(ncol(data)-3),backgroundColor = styleEqual(-999, "red", default = "#aaeeaa")) %>%
        formatStyle(columns = (ncol(data)-2):ncol(data), backgroundColor = "lightgray") %>%
        formatRound(columns = 2:ncol(data),digits = sig_digies)
    })
  }
}