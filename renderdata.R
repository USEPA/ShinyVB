renderdata <- function(data,response_var,select_choice,feat_props,ignored_rows,current_page,output) {
  
  # Ensure a plain data.frame and preserve names
  data <- data.frame(data, check.names = FALSE)
  if (ncol(data) < 1L) { output$data <- NULL; return(invisible()) }
  
  # Enforce character ID for display
  data[[1]] <- as.character(data[[1]])
  
  # Build digits vector (coerce to numeric; default to 2 where NA)
  col_names <- colnames(data)
  sig_digies <- numeric(length(col_names))
  for (i in seq_along(col_names)) {
    val <- tryCatch(values(feat_props, keys = col_names[i])[1], error = function(e) NA_real_)
    val <- suppressWarnings(as.numeric(val))
    sig_digies[i] <- ifelse(is.na(val), 2, val)
  }
  
  # Row highlighting for ignored rows
  all_rows <- row.names(data)
  iggies   <- ifelse(all_rows %in% ignored_rows, "gray", "")
  
  # Page length
  PL <- 25
  
  # Helper to ensure digits length matches columns length
  fit_digits <- function(digits, n) {
    if (length(digits) == n) return(digits)
    if (length(digits) == 1) return(rep(digits, n))
    rep_len(digits, n)
  }
  
  # Build a view copy for table rendering
  data_view    <- data
  row_IDs_view <- data_view[, 1, drop = TRUE]
  
  # Columns to round: all except ID (first column)
  n_cols       <- ncol(data_view)
  non_id_cols  <- if (n_cols > 1L) 2:n_cols else integer(0)
  digits_non_id <- if (length(non_id_cols)) fit_digits(sig_digies[-1], length(non_id_cols)) else numeric(0)
  
  # Common DataTable options
  dt_opts <- list(
    autoWidth    = FALSE,
    dom          = "ltBp",
    paging       = TRUE,
    pageLength   = PL,
    displayStart = current_page * PL - PL,
    scrollX      = TRUE,
    scrollY      = TRUE,
    buttons      = c("copy", "csv", "excel"),
    columnDefs   = list(list(targets = "_all", className = "dt-center")),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
      "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
      "}"
    )
  )
  
  if (identical(select_choice, "Change_Response")) {
    # Column selection mode (highlight response column)
    output$data <- DT::renderDataTable(server = TRUE, {
      DT::datatable(
        data_view,
        rownames  = FALSE,
        extensions = "Buttons",
        selection = list(
          selected = list(rows = NULL, cols = response_var - 1L), # 0-based in JS
          target   = "column",
          mode     = "single"
        ),
        editable  = FALSE,
        options   = dt_opts
      ) %>%
        DT::formatStyle(response_var, backgroundColor = "#b0bed9") %>%
        DT::formatStyle(1, backgroundColor = "lightgray", `white-space` = "nowrap") %>%
        DT::formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
        DT::formatRound(columns = non_id_cols, digits = digits_non_id)
    })
    
  } else if (identical(select_choice, "Edit_Cells")) {
    # Cell editing mode (disable editing of ID column)
    output$data <- DT::renderDataTable(server = TRUE, {
      DT::datatable(
        data_view,
        rownames  = FALSE,
        extensions = "Buttons",
        selection = "none",
        editable  = list(target = "cell", disable = list(columns = 0)), # 0-based: disable ID column
        options   = dt_opts
      ) %>%
        DT::formatStyle(response_var, backgroundColor = "#b0bed9") %>%
        DT::formatStyle(1, backgroundColor = "lightgray", `white-space` = "nowrap") %>%
        DT::formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
        DT::formatRound(columns = non_id_cols, digits = digits_non_id)
    })
    
  } else if (identical(select_choice, "D/E_Rows")) {
    # Row selection mode (for delete/enable)
    output$data <- DT::renderDataTable(server = TRUE, {
      DT::datatable(
        data_view,
        rownames  = FALSE,
        extensions = "Buttons",
        selection = list(
          selected = list(rows = NULL, cols = response_var - 1L),
          target   = "row",
          mode     = "multiple"
        ),
        editable  = FALSE,
        options   = dt_opts
      ) %>%
        DT::formatStyle(response_var, backgroundColor = "#b0bed9") %>%
        DT::formatStyle(1, backgroundColor = "lightgray", `white-space` = "nowrap") %>%
        DT::formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
        DT::formatRound(columns = non_id_cols, digits = digits_non_id)
    })
    
  } else {
    # Fallback: treat as Change_Response
    output$data <- DT::renderDataTable(server = TRUE, {
      DT::datatable(
        data_view,
        rownames  = FALSE,
        extensions = "Buttons",
        selection = list(
          selected = list(rows = NULL, cols = response_var - 1L),
          target   = "column",
          mode     = "single"
        ),
        editable  = FALSE,
        options   = dt_opts
      ) %>%
        DT::formatStyle(response_var, backgroundColor = "#b0bed9") %>%
        DT::formatStyle(1, backgroundColor = "lightgray", `white-space` = "nowrap") %>%
        DT::formatStyle(1, target = "row", backgroundColor = styleEqual(row_IDs_view, iggies)) %>%
        DT::formatRound(columns = non_id_cols, digits = digits_non_id)
    })
  }
}