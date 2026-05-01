renderPCAdata <- function(data, output) {
  if (is.null(data)) {
    output$PCAdata <- NULL
    return(invisible())
  }
  
  # Preserve names; enforce character ID
  data <- data.frame(data, check.names = FALSE)
  if (ncol(data) < 1L) { output$PCAdata <- NULL; return(invisible()) }
  data[[1]] <- as.character(data[[1]])
  
  # Rounding digits for non-ID columns
  sig_digies <- 3L
  n <- ncol(data)
  non_id_cols <- if (n >= 2L) 2:n else integer(0)
  
  dt_opts <- list(
    autoWidth    = FALSE,
    dom          = "ltBp",
    paging       = TRUE,
    pageLength   = 20,
    scrollX      = TRUE,
    scrollY      = TRUE,
    buttons      = c("copy", "csv", "excel"),
    columnDefs   = list(list(targets = "_all", className = "dt-center")),
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
      "}"
    )
  )
  
  output$PCAdata <- DT::renderDataTable(server = TRUE, {
    dt <- DT::datatable(
      data,
      rownames  = FALSE,
      extensions = "Buttons",
      selection = list(
        selected = list(rows = NULL, cols = 1),  # 0-based -> highlights 2nd column
        target   = "column",
        mode     = "single"
      ),
      editable  = FALSE,
      options   = dt_opts
    )
    
    # Style ID and the second column when present
    if (n >= 1L) {
      dt <- dt |>
        DT::formatStyle(1, backgroundColor = "lightgray", `white-space` = "nowrap")
    }
    if (n >= 2L) {
      dt <- dt |>
        DT::formatStyle(2, backgroundColor = "#b0bed9") |>
        DT::formatRound(columns = non_id_cols, digits = sig_digies)
    }
    
    dt
  })
}