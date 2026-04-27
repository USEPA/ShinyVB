renderPCAdata <- function(data,
                          date_format_string,
                          output) {
  
  if (is.null(data)) {
    output$PCAdata <- NULL
    return()
  }
  
  data <- data.frame(data, check.names = FALSE)
  sig_digies <- 3
  
  # Match renderdata(): treat "Date" and "Date_MDY_HM" as date-like
  is_date_like <- date_format_string %in% c("Date", "Date_MDY_HM")
  
  # Build a view copy (don’t mutate the original)
  data_view <- data
  
  if (identical(date_format_string, "Date_MDY_HM") ||
      (identical(date_format_string, "Date") && is.character(data_view[[1]]))) {
    data_view[[1]] <- format_id_MDY_HM(data_view[[1]])
  }
  
  if (is_date_like) {
    
    output$PCAdata <- DT::renderDataTable(server = TRUE, {
      DT::datatable(
        data_view,
        rownames = FALSE,
        extensions = "Buttons",
        selection = list(
          selected = list(rows = NULL, cols = 1),
          target  = "column",
          mode    = "single"
        ),
        editable = FALSE,
        options = list(
          autoWidth = FALSE,
          dom       = "ltBp",
          paging    = TRUE,
          pageLength= 20,
          scrollX   = TRUE,
          scrollY   = TRUE,
          buttons   = c("copy", "csv", "excel"),
          columnDefs= list(list(targets = "_all", className = "dt-center")),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "}"
          )
        )
      ) %>%
        DT::formatStyle(1, backgroundColor = "lightgray", `white-space` = "nowrap") %>%
        DT::formatStyle(2, backgroundColor = "#b0bed9") %>%
        DT::formatRound(columns = 2:ncol(data_view), digits = sig_digies)
      # Note: We do NOT call DT::formatDate here because we already formatted
    })
    
  } else if (identical(date_format_string, "Numeric")) {
    
    output$PCAdata <- DT::renderDataTable(server = TRUE, {
      DT::datatable(
        data,
        rownames = FALSE,
        extensions = "Buttons",
        selection = list(
          selected = list(rows = NULL, cols = 1),
          target  = "column",
          mode    = "single"
        ),
        editable = FALSE,
        options = list(
          autoWidth = FALSE,
          dom       = "ltBp",
          paging    = TRUE,
          pageLength= 20,
          scrollX   = TRUE,
          scrollY   = TRUE,
          buttons   = c("copy", "csv", "excel"),
          columnDefs= list(list(targets = "_all", className = "dt-center")),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "}"
          )
        )
      ) %>%
        DT::formatStyle(1, backgroundColor = "lightgray", `white-space` = "nowrap") %>%
        DT::formatStyle(2, backgroundColor = "#b0bed9") %>%
        DT::formatRound(columns = 1:ncol(data), digits = sig_digies)
    })
    
  } else if (identical(date_format_string, "Character")) {
    
    output$PCAdata <- DT::renderDataTable(server = TRUE, {
      DT::datatable(
        data,
        rownames = FALSE,
        extensions = "Buttons",
        selection = list(
          selected = list(rows = NULL, cols = 1),
          target  = "column",
          mode    = "single"
        ),
        editable = FALSE,
        options = list(
          autoWidth = FALSE,
          dom       = "ltBp",
          paging    = TRUE,
          pageLength= 20,
          scrollX   = TRUE,
          scrollY   = TRUE,
          buttons   = c("copy", "csv", "excel"),
          columnDefs= list(list(targets = "_all", className = "dt-center")),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "}"
          )
        )
      ) %>%
        DT::formatStyle(1, backgroundColor = "lightgray", `white-space` = "nowrap") %>%
        DT::formatStyle(2, backgroundColor = "#b0bed9") %>%
        DT::formatRound(columns = 2:ncol(data), digits = sig_digies)
    })
    
  } else {
    # Fallback
    output$PCAdata <- DT::renderDataTable(server = TRUE, {
      DT::datatable(data_view, rownames = FALSE)
    })
  }
}