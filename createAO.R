createAO = function(col_names,speed,direct,A_name,O_name,current_data,bo) {

  Aname = A_name
  Oname = O_name

  speed_dat = current_data[,speed]
  dir_dat = current_data[,direct]

  A_comp = -speed_dat*cos((dir_dat - bo)*pi/180)
  O_comp = speed_dat*sin((dir_dat - bo)*pi/180)

  new_data = cbind(current_data,A_comp,O_comp)
  colnames(new_data) = c(col_names,Aname,Oname)

  return(new_data)
}