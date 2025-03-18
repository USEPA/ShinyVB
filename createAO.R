createAO = function(col_names,speed,direct,A_name,O_name,current_data,bo) {
  
  col_list = col_names
  
  if (speed != "-" & direct != "-") {
    
    Aname = A_name
    Oname = O_name
    
    if (!(Aname %in% col_list) & !(Oname %in% col_list)) {
      
      new_data = current_data
      speed_dat = new_data[,speed]
      dir_dat = new_data[,direct]
      
      A_comp = -speed_dat*cos((dir_dat - bo)*pi/180)
      O_comp = speed_dat*sin((dir_dat - bo)*pi/180)
      
      new_data = cbind(new_data,A_comp,O_comp)
      
      colnames(new_data) = c(col_list,Aname,Oname)
      
      return(new_data)

    } else {
      showModal(modalDialog(div("ERROR: BOTH new component columns must have different names than any currently existing column names.",style="font-size:160%"),easyClose = T))
    }
  } else {
    showModal(modalDialog(div("ERROR: A speed and direction data column must be specified.",style="font-size:160%"),easyClose = T))
  }
}