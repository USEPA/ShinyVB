renderdata = function(data,
                      response_var,
                      id_var,
                      select_choice,
                      date_format_string,
                      feat_props,
                      ignored_rows,
                      current_page,
                      output) {
  
  data = data.frame(data)
  
  sig_digies = c()
  
  col_names = colnames(data)
  
  all_rows = row.names(data)
  iggies = ifelse(all_rows %in% ignored_rows, 'gray', '')
  
  row_IDs = data[,1]
  
  PL = 20
  
  for (i in 1:ncol(data)) {
    sig_digies = append(sig_digies, values(feat_props, keys = col_names[i])[1])
  }
  
  if (select_choice == "Change_Response") {
    
    if (date_format_string != "Numeric" && date_format_string != "Character") {
      
      col_list = seq(1, ncol(data))
      remaining = col_list[-id_var]
      sig_digies = sig_digies[-id_var]
      
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "column",
            mode = "single"
          ),
          editable = F,
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
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = remaining, digits = sig_digies) %>%
          formatDate(id_var, date_format_string)
      })
      
    } else if (date_format_string == "Numeric") {
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "column",
            mode = "single"
          ),
          editable = F,
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
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = 1:ncol(data),digits = sig_digies)
      })
    } else if (date_format_string == "Character") {
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "column",
            mode = "single"
          ),
          editable = F,
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
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = 2:ncol(data),digits = sig_digies)
      })
    }
  } else if (select_choice == "Edit_Cells") {
    if (date_format_string != "Numeric" && date_format_string != "Character") {
      col_list = seq(1, ncol(data))
      remaining = col_list[-id_var]
      sig_digies = sig_digies[-id_var]
      
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = "none",
          editable = list(target = "cell", disable = list(columns = 0)),
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
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = remaining, digits = sig_digies) %>%
          formatDate(id_var, date_format_string)
      })
      
    } else if (date_format_string == "Numeric") {
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = "none",
          editable = list(target = "cell", disable = list(columns = 0)),
          options = list(
            autoWidth = F,
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            dom='ltBp',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = 1:ncol(data),digits = sig_digies)
      })
    } else if (date_format_string == "Character") {
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = "none",
          editable = list(target = "cell", disable = list(columns = 0)),
          options = list(
            autoWidth = F,
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            dom='ltBp',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = 2:ncol(data),digits = sig_digies)
      })
    }
  } else if (select_choice == "D/E_Rows") {
    if (date_format_string != "Numeric" && date_format_string != "Character") {
      col_list = seq(1, ncol(data))
      remaining = col_list[-id_var]
      sig_digies = sig_digies[-id_var]
      
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "row",
            mode = "multiple"
          ),
          editable = F,
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
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = remaining, digits = sig_digies) %>%
          formatDate(id_var, date_format_string)
      })
      
    } else if (date_format_string == "Numeric") {
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "row",
            mode = "multiple"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            dom='ltBp',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = 1:ncol(data),digits = sig_digies)
      })
    } else if (date_format_string == "Character") {
      output$data = DT::renderDataTable(server = T, {
        datatable(
          data,
          rownames = F,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "row",
            mode = "multiple"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            dom='ltBp',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(
              targets = '_all', className = 'dt-center'
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatRound(columns = 2:ncol(data),digits = sig_digies)
      })
    }
  }
}