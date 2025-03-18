renderdata = function(current_data,response_var,id_var) {
  
  rv_col = response_var
  rv_col = rv_col - 1
  idd = id_var
  
  pushed_data = as.data.frame(current_data)
  ncolls = ncol(pushed_data)
  
  rendered = DT::renderDataTable(server=T,{datatable(pushed_data,rownames=F,selection=list(selected = list(rows = NULL, cols = rv_col),
                                                                                              target = "row+column",mode="single"),editable=T,
                                                        options = list(
                                                          autoWidth=F,
                                                          paging = TRUE,
                                                          pageLength = 25,
                                                          scrollX = TRUE,
                                                          scrollY = TRUE,
                                                          columnDefs = list(list(
                                                            targets = '_all', className = 'dt-center'
                                                          )),
                                                          initComplete = JS(
                                                            "function(settings, json) {",
                                                            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
                                                            "}"))) %>%
      formatStyle(idd,backgroundColor = 'lightgray') %>%
      formatRound(columns=1:ncolls, digits=c(1,1,1,1,2,1,5,2,2,1,1,1,1,1,1))
  })
  rendered
}