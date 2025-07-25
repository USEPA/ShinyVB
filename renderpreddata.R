renderpreddata = function(data,
                          feat_props,
                          current_page,
                          output) {
  
  data = data.frame(data)
  sig_digies = c()
  col_names = colnames(data)
  
  for (i in 1:(ncol(data)-4)) {
    if (colnames(data)[i] %in% keys(feat_props)) {
      sig_digies = append(sig_digies, values(feat_props, keys = col_names[i])[1])
    } else {
      sig_digies = 3
    }
  }
  
  PL = 20
  
  sig_digies = c(sig_digies,3,3,3)
  
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
      formatStyle(columns = 3:(ncol(data)-4),color = styleEqual(-999, "white", default = "black"),
                  backgroundColor = styleEqual(-999, "#BA0C2F", default = "#aaeeaa")) %>%
      formatStyle(columns = (ncol(data)-3):ncol(data), backgroundColor = "lightgray") %>%
      formatRound(columns = 2:ncol(data)-1,digits = sig_digies)
  })
}