renderpreddata = function(data,
                          feat_props,
                          current_page,
                          init_ID_format,
                          output) {
  
  data = data.frame(data)
  sig_digies = c()
  col_names = colnames(data)
  
  for (i in 2:(ncol(data)-4)) {
    if (colnames(data)[i] %in% keys(feat_props)) {
      sig_digies = append(sig_digies, values(feat_props, keys = col_names[i])[[1]])
    } else {
      sig_digies = append(sig_digies, 3)
    }
  }
  
  PL = 5000
  
  sig_digies1 = c(sig_digies,3,3,3)
  
  if (init_ID_format == "YMD") {
    data[,1] = ymd(data[,1])
    date_format_string = "toLocaleDateString"
  } else if (init_ID_format == "MDY") {
    data[,1] = mdy(data[,1])
    date_format_string = "toLocaleDateString"
  } else if (init_ID_format == "MDYHM") {
    data[,1] = parse_date_time(data[,1],c('%m/%d/%y %H:%M'),exact=TRUE)
    date_format_string = "toLocaleString"
  } else if (init_ID_format == "Character") {
    date_format_string = "Character"
  } else if (init_ID_format == "Numeric") {
    date_format_string = "Numeric"
  }
  
  if (date_format_string != "Numeric" && date_format_string != "Character") {
    
    output$pd_data = DT::renderDT(server = T, {
      datatable(
        data,
        rownames = F,
        extensions = 'Buttons',
        selection = "none",
        editable = list(target = "cell", disable = list(columns = c((ncol(data)-3):ncol(data)))),
        options = list(
          autoWidth = F,
          dom='ltBp',
          paging = FALSE,
          pageLength = PL,
          displayStart = current_page * PL - PL,
          scrollX = TRUE,
          scrollY = TRUE,
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(targets = 0, className = 'dt-center', type = 'string'),
            list(targets = '_all', className = 'dt-center',createdCell = JS("function(td, cellData, rowData, row, col) {","$(td).css({'min-width': '75px'});","}"))),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'pd_data', {priority: 'event'});","}"))) |>
        formatStyle(columns = 1:2, backgroundColor = "#b0bed9") |>
        formatStyle(columns = 3:(ncol(data)-4),color = styleEqual(-999, "white", default = "black"),
                    backgroundColor = styleEqual(-999, "#BA0C2F", default = "#aaeeaa")) |>
        formatStyle(columns = (ncol(data)-3):ncol(data), backgroundColor = "lightgray") |>
        formatRound(columns = 2:(ncol(data)-1),digits = sig_digies1) %>%
        formatDate(columns=1, date_format_string)
    })
    
  } else if (date_format_string == "Numeric") {
    
    sig_digies2 = c(3,sig_digies1)
    
    output$pd_data = DT::renderDT(server = T, {
      datatable(
        data,
        rownames = F,
        extensions = 'Buttons',
        selection = "none",
        editable = list(target = "cell", disable = list(columns = c((ncol(data)-3):ncol(data)))),
        options = list(
          autoWidth = F,
          dom='ltBp',
          paging = FALSE,
          pageLength = PL,
          displayStart = current_page * PL - PL,
          scrollX = TRUE,
          scrollY = TRUE,
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(targets = 0, className = 'dt-center', type = 'string'),
            list(targets = '_all', className = 'dt-center',createdCell = JS("function(td, cellData, rowData, row, col) {","$(td).css({'min-width': '75px'});","}"))),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'pd_data', {priority: 'event'});","}"))) |>
        formatStyle(columns = 1:2, backgroundColor = "#b0bed9") |>
        formatStyle(columns = 3:(ncol(data)-4),color = styleEqual(-999, "white", default = "black"),
                    backgroundColor = styleEqual(-999, "#BA0C2F", default = "#aaeeaa")) |>
        formatStyle(columns = (ncol(data)-3):ncol(data), backgroundColor = "lightgray") |>
        formatRound(columns = 1:(ncol(data)-1),digits = sig_digies2)
    })
    
  } else if (date_format_string == "Character") {
    
    output$pd_data = DT::renderDT(server = T, {
      datatable(
        data,
        rownames = F,
        extensions = 'Buttons',
        selection = "none",
        editable = list(target = "cell", disable = list(columns = c((ncol(data)-3):ncol(data)))),
        options = list(
          autoWidth = F,
          dom='ltBp',
          paging = FALSE,
          pageLength = PL,
          displayStart = current_page * PL - PL,
          scrollX = TRUE,
          scrollY = TRUE,
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(targets = 0, className = 'dt-center', type = 'string'),
            list(targets = '_all', className = 'dt-center',createdCell = JS("function(td, cellData, rowData, row, col) {","$(td).css({'min-width': '75px'});","}"))),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'pd_data', {priority: 'event'});","}"))) |>
        formatStyle(columns = 1:2, backgroundColor = "#b0bed9") |>
        formatStyle(columns = 3:(ncol(data)-4),color = styleEqual(-999, "white", default = "black"),
                    backgroundColor = styleEqual(-999, "#BA0C2F", default = "#aaeeaa")) |>
        formatStyle(columns = (ncol(data)-3):ncol(data), backgroundColor = "lightgray") |>
        formatRound(columns = 2:(ncol(data)-1),digits = sig_digies1)
    })
  }
}