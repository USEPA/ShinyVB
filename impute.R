library(missForest)

imputing = function(current_data,id_var,response_var,ignored_rows,seed) {
  
  temp_data = current_data

  col_list = seq(1,ncol(temp_data))
  row_list = seq(1,nrow(temp_data))
    
  min_col_removed = min(id_var,response_var)
  max_col_removed = max(id_var,response_var)
    
  removed_cols = c(min_col_removed,max_col_removed)
  removed_rows = ignored_rows
    
  remaining_cols = col_list[! col_list %in% removed_cols]
  remaining_rows = row_list[! row_list %in% removed_rows]
  
  if (is.null(removed_rows)) {
    impute_data1 = temp_data[,-removed_cols]
  } else {
    impute_data1 = temp_data[-removed_rows,-removed_cols]
  }
  
  set.seed(seed)
  
  impute_data2=missForest(impute_data1)
  impute_data3=impute_data2$ximp
    
  impute_data_final=matrix(data= 0.0,nrow=nrow(temp_data),ncol=ncol(temp_data))
    
  colnames(impute_data_final) = colnames(temp_data)
    
  k=1
  l=1
    
  for (i in remaining_rows) {
    for (j in remaining_cols) {
      impute_data_final[i,j] = impute_data3[k,l]
    l = l + 1
    }
    k = k + 1
  }
  
  for (i in removed_rows) {
    for (j in removed_cols) {
      impute_data_final[i,j] = temp_data[i,j]
    }
  }
    
  impute_data_final
}