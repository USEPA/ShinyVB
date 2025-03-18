library(missForest)

imputing = function(current_data,id_var,response_var) {
  
  temp_data = current_data

  col_list = seq(1,ncol(temp_data))
    
  min_col_removed = min(id_var,response_var)
  max_col_removed = max(id_var,response_var)
    
  removed = c(min_col_removed,max_col_removed)
    
  remaining = col_list[! col_list %in% removed]
    
  impute_data1 = temp_data[,-removed]
  impute_data2=missForest(impute_data1)
  impute_data3=impute_data2$ximp
    
  impute_data_final=matrix(data= 0.0,nrow=nrow(temp_data),ncol=ncol(temp_data))
    
  colnames(impute_data_final) = colnames(temp_data)
    
  j=1
    
  for (i in remaining) {
    impute_data_final[,i] = impute_data3[,j]
    j = j + 1
  }
    
  impute_data_final[,min_col_removed] = temp_data[,min_col_removed]
  impute_data_final[,max_col_removed] = temp_data[,max_col_removed]
    
  impute_data_final
}
