renderpreddata <- function(data, feat_props, current_page, output) {
  # Preserve original column names
  data <- data.frame(data, check.names = FALSE)
  if (ncol(data) < 1L) { output$pd_data <- NULL; return(invisible()) }
  
  # Enforce character ID for display
  data[[1]] <- as.character(data[[1]])
  
  n <- ncol(data)
  
  # Indices (guarded)
  idx_id         <- 1L
  idx_resp       <- 2L
  idx_pred_end   <- n
  idx_pred_start <- max(1L, n - 3L)           # last 4 columns
  idx_feat_start <- 3L
  idx_feat_end   <- max(2L, idx_pred_start - 1L)
  
  # Build digits vector for non-ID columns (defaults to 3)
  # - For columns 2..idx_feat_end: from feat_props (default 3)
  # - For columns idx_feat_end+1 .. n-1 (the result trio): use 3
  digits_non_id <- integer(0)
  if (n > 1L) {
    for (i in 2:(n - 1L)) {
      if (i <= idx_feat_end) {
        nm <- colnames(data)[i]
        val <- tryCatch(values(feat_props, keys = nm)[[1]], error = function(e) NA_real_)
        val <- suppressWarnings(as.numeric(val))
        digits_non_id <- c(digits_non_id, ifelse(is.na(val), 3, val))
      } else {
        digits_non_id <- c(digits_non_id, 3)
      }
    }
  }
  
  PL <- 500
  
  # ColumnDefs (center all; ensure min widths)
  base_column_defs <- list(
    list(targets = 0, className = "dt-center", type = "string"),
    list(
      targets     = "_all",
      className   = "dt-center",
      createdCell = JS(
        "function(td, cellData, rowData, row, col) {",
        "$(td).css({'min-width': '75px'});",
        "}"
      )
    )
  )
  
  dt_opts <- list(
    autoWidth    = FALSE,
    dom          = "ltBp",
    paging       = FALSE,
    pageLength   = PL,
    displayStart = current_page * PL - PL,
    scrollX      = TRUE,
    scrollY      = TRUE,
    buttons      = c("copy", "csv", "excel"),
    columnDefs   = base_column_defs,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
      "Shiny.setInputValue('tableRendered', 'pd_data', {priority: 'event'});",
      "}"
    )
  )
  
  # Base datatable with styling
  make_dt <- function(x) {
    dt <- DT::datatable(
      x,
      rownames   = FALSE,
      extensions = "Buttons",
      selection  = "none",
      editable   = list(target = "cell", disable = list(columns = c((ncol(x) - 3):ncol(x)))),
      options    = dt_opts
    ) |>
      # ID + Response columns background
      DT::formatStyle(columns = 1:2, backgroundColor = "#b0bed9") |>
      # Last four columns background
      DT::formatStyle(columns = (ncol(x) - 3):ncol(x), backgroundColor = "lightgray")
    
    # Feature region styling for -999 (only if feature range exists)
    if (idx_feat_end >= idx_feat_start && ncol(x) >= idx_feat_end) {
      dt <- dt |>
        DT::formatStyle(
          columns = idx_feat_start:idx_feat_end,
          color = DT::styleEqual(-999, "white", default = "black"),
          backgroundColor = DT::styleEqual(-999, "#BA0C2F", default = "#aaeeaa")
        )
    }
    dt
  }
  
  # Render: ID (col 1) nowrap, round all non-ID numeric columns (2..n-1)
  output$pd_data <- DT::renderDT(server = TRUE, {
    make_dt(data) |>
      DT::formatStyle(1, `white-space` = "nowrap") |>
      DT::formatRound(
        columns = if (n > 2L) 2:(n - 1L) else integer(0),
        digits = digits_non_id
      )
  })
}