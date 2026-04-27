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
  
  # Build digits vector (coerce to numeric; default to 2 where NA)
  col_names = colnames(data)
  sig_digies <- numeric(length(col_names))
  for (i in seq_along(col_names)) {
    val <- tryCatch(values(feat_props, keys = col_names[i])[1], error = function(e) NA_real_)
    val <- suppressWarnings(as.numeric(val))
    sig_digies[i] <- ifelse(is.na(val), 2, val)
  }
  
  all_rows = row.names(data)
  iggies = ifelse(all_rows %in% ignored_rows, 'gray', '')
  
  PL = 20
  
  # Helper to ensure digits length matches columns length
  fit_digits <- function(digits, n) {
    if (length(digits) == n) return(digits)
    if (length(digits) == 1) return(rep(digits, n))
    # Fallback: recycle safely
    rep_len(digits, n)
  }
  
  # Date-like display modes
  is_date_like <- date_format_string %in% c("Date", "Date_MDY_HM")
  
  # Build a view copy for table rendering
  data_view <- data
  
  if (identical(date_format_string, "Date_MDY_HM") ||
      (identical(date_format_string, "Date") && is.character(data_view[[id_var]]))) {
    data_view[[id_var]] <- format_id_MDY_HM(data_view[[id_var]])
  }
  
  row_IDs_view <- data_view[, 1]
  
  if (select_choice == "Change_Response") {
    
    if (is_date_like) {
      col_list = seq_len(ncol(data_view))
      remaining = col_list[-id_var]
      digits_rem <- fit_digits(sig_digies[-id_var], length(remaining))
      
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "column",
            mode = "single"
          ),
          editable = FALSE,
          options = list(
            autoWidth = FALSE,
            dom='ltBp',
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray', `white-space` = "nowrap") %>%
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = remaining, digits = digits_rem)
      })
      
    } else if (identical(date_format_string, "Numeric")) {
      digits_all <- fit_digits(sig_digies, ncol(data_view))
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "column",
            mode = "single"
          ),
          editable = FALSE,
          options = list(
            autoWidth = FALSE,
            dom='ltBp',
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
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
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = seq_len(ncol(data_view)), digits = digits_all)
      })
      
    } else if (identical(date_format_string, "Character")) {
      # Format all columns except ID
      digits_non_id <- fit_digits(sig_digies[-1], ncol(data_view) - 1L)
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "column",
            mode = "single"
          ),
          editable = FALSE,
          options = list(
            autoWidth = FALSE,
            dom='ltBp',
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray', `white-space` = "nowrap") %>%
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = 2:ncol(data_view), digits = digits_non_id)
      })
    }
    
  } else if (select_choice == "Edit_Cells") {
    
    if (is_date_like) {
      col_list = seq_len(ncol(data_view))
      remaining = col_list[-id_var]
      digits_rem <- fit_digits(sig_digies[-id_var], length(remaining))
      
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = "none",
          editable = list(target = "cell", disable = list(columns = 0)),
          options = list(
            autoWidth = FALSE,
            dom='ltBp',
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray', `white-space` = "nowrap") %>%
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = remaining, digits = digits_rem)
      })
      
    } else if (identical(date_format_string, "Numeric")) {
      digits_all <- fit_digits(sig_digies, ncol(data_view))
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = "none",
          editable = list(target = "cell", disable = list(columns = 0)),
          options = list(
            autoWidth = FALSE,
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            dom='ltBp',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
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
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = seq_len(ncol(data_view)), digits = digits_all)
      })
      
    } else if (identical(date_format_string, "Character")) {
      digits_non_id <- fit_digits(sig_digies[-1], ncol(data_view) - 1L)
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = "none",
          editable = list(target = "cell", disable = list(columns = 0)),
          options = list(
            autoWidth = FALSE,
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            dom='ltBp',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray', `white-space` = "nowrap") %>%
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = 2:ncol(data_view), digits = digits_non_id)
      })
    }
    
  } else if (select_choice == "D/E_Rows") {
    
    if (is_date_like) {
      col_list = seq_len(ncol(data_view))
      remaining = col_list[-id_var]
      digits_rem <- fit_digits(sig_digies[-id_var], length(remaining))
      
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "row",
            mode = "multiple"
          ),
          editable = FALSE,
          options = list(
            autoWidth = FALSE,
            dom='ltBp',
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray', `white-space` = "nowrap") %>%
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = remaining, digits = digits_rem)
      })
      
    } else if (identical(date_format_string, "Numeric")) {
      digits_all <- fit_digits(sig_digies, ncol(data_view))
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "row",
            mode = "multiple"
          ),
          editable = FALSE,
          options = list(
            autoWidth = FALSE,
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            dom='ltBp',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
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
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = seq_len(ncol(data_view)), digits = digits_all)
      })
      
    } else if (identical(date_format_string, "Character")) {
      digits_non_id <- fit_digits(sig_digies[-1], ncol(data_view) - 1L)
      output$data = DT::renderDataTable(server = TRUE, {
        datatable(
          data_view,
          rownames = FALSE,
          extensions = 'Buttons',
          selection = list(
            selected = list(rows = NULL, cols = response_var - 1),
            target = "row",
            mode = "multiple"
          ),
          editable = FALSE,
          options = list(
            autoWidth = FALSE,
            paging = TRUE,
            pageLength = PL,
            displayStart = current_page * PL - PL,
            scrollX = TRUE,
            scrollY = TRUE,
            dom='ltBp',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(list(targets = '_all', className = 'dt-center')),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        ) %>%
          formatStyle(response_var, backgroundColor = "#b0bed9") %>%
          formatStyle(id_var, backgroundColor = 'lightgray', `white-space` = "nowrap") %>%
          formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
          formatRound(columns = 2:ncol(data_view), digits = digits_non_id)
      })
    }
  }
}