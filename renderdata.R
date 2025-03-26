library(DT)
library(hash)

renderdata = function(current_data,response_var,id_var,date_format_string,feat_props,output){
  
  sig_digies = c()
  
  col_names = colnames(current_data)
  
  for (i in 1:ncol(current_data)) {
    sig_digies = append(sig_digies,values(feat_props,keys=col_names[i])[1])
  }
  
  if (date_format_string != "-") {
    
    col_list = seq(1,ncol(current_data))
    remaining = col_list[-id_var]
    sig_digies = sig_digies[-id_var]
    
    output$data = DT::renderDataTable(server=T,{
      datatable(current_data,rownames=F,selection=list(selected = list(rows = NULL, cols = response_var-1),
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
        formatStyle(id_var,backgroundColor = 'lightgray') %>%
        formatStyle(response_var,backgroundColor = "#b0bed9") %>%
        formatRound(columns=remaining, digits=sig_digies) %>%
        formatDate(id_var,date_format_string)
    })
    
  } else {
    
    output$data = DT::renderDataTable(server=T,{
      datatable(current_data,rownames=F,selection=list(selected = list(rows = NULL, cols = response_var-1),
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
        formatStyle(id_var,backgroundColor = 'lightgray') %>%
        formatStyle(response_var,backgroundColor = "#b0bed9") %>%
        formatRound(columns=1:ncol(current_data), digits=sig_digies)
    })
  }
}