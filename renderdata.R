renderdata = function(data,
                      response_var,
                      id_var,
                      select_choice,
                      date_format_string,
                      feat_props,
                      ignored_rows,
                      output) {
  
  data = data.frame(data)
  
  sig_digies = c()
  
  col_names = colnames(data)
  
  all_rows = row.names(data)
  iggies = ifelse(all_rows %in% ignored_rows, 'gray', '')
  
  row_IDs = data[,1]
  
  for (i in 1:ncol(data)) {
    sig_digies = append(sig_digies, values(feat_props, keys = col_names[i])[1])
  }
  
  if (select_choice == "Features") {
    
    if (date_format_string != "Other") {
      
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
          editable = T,
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
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatRound(columns = remaining, digits = sig_digies) %>%
          formatDate(id_var, date_format_string)
      })
      
    } else {
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
          editable = T,
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
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatRound(columns = 1:ncol(data),digits = sig_digies)
      })
    }
  } else {
    if (date_format_string != "Other") {
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
          editable = T,
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
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatRound(columns = remaining, digits = sig_digies) %>%
          formatDate(id_var, date_format_string)
      })
      
    } else {
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
          editable = T,
          options = list(
            autoWidth = F,
            paging = TRUE,
            pageLength = 20,
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
              "}"
            )
          )
        ) %>%
          formatStyle(1,target = "row",backgroundColor = styleEqual(row_IDs,iggies)) %>%
          formatStyle(id_var, backgroundColor = 'lightgray') %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatRound(columns = 1:ncol(data),digits = sig_digies)
      })
    }
  }
}