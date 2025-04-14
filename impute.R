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
  
  impute_data0 = temp_data[,-removed_cols]
  
  if (is.null(removed_rows)) {
    impute_data1 = impute_data0
  } else {
    impute_data1 = impute_data0[-removed_rows,]
  }
  
  set.seed(seed)
  
  impute_data2=missForest(impute_data1)
  impute_data3=impute_data2$ximp
  impute_data4=data.frame(impute_data3)
    
  imputed_data0=matrix(0,nrow=nrow(impute_data0),ncol=ncol(impute_data0))
  imputed_data0=data.frame(imputed_data0)
  
  row_counter=1
  
  for (i in 1:nrow(imputed_data0)) {
    
    if (i %in% removed_rows) {
      imputed_data0[i,] = impute_data0[i,]
    } else {
      imputed_data0[i,] = impute_data4[row_counter,]
      row_counter = row_counter + 1
    }
  }
  
  imputed_data_final=matrix(0,nrow=nrow(temp_data),ncol=ncol(temp_data))
  imputed_data_final=data.frame(imputed_data_final)
  
  col_counter=1
  
  for (j in 1:ncol(temp_data)) {
    
    if (j %in% removed_cols) {
      imputed_data_final[,j] = temp_data[,j]
    } else {
      imputed_data_final[,j] = imputed_data0[,col_counter]
      col_counter = col_counter + 1
    }
  }

  colnames(imputed_data_final) = colnames(temp_data)
  imputed_data_final
}