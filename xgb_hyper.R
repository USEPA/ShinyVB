library(rsample)
library(shiny)
library(bslib)
library(xgboost)
library(dplyr)
library(permimp)

xgb_hyper = function(current_data,response_var,coves_to_use,nd_val,tntc_val,tntc_multy,MC_runs,loggy,randomize,
                     tree_method,xgb_tech,rate_drop_list,skip_drop_list,eta_list,gamma_list,max_depth_list,min_child_weight_list,subsamp_list,
                     colsamp_list,nrounds_list,nfold_list,early_stops_list,normalize_type,sample_type) {
  
  cove_data=current_data[,coves_to_use]
  ncoves = ncol(cove_data)
  cove_names = colnames(cove_data)
  data = cbind(current_data[,response_var],cove_data)
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data=data[!is.na(data[,1]),]
  
  if (xgb_tech == "dart") {
    hyper_grid = expand.grid(
        booster = xgb_tech,
        rate_drop = rate_drop_list,
        skip_drop = skip_drop_list,
        normalize_type = normalize_type,
        sample_type = sample_type,
        
        tree_method = tree_method,
        eta = eta_list,
        gamma = gamma_list,
        max_depth = max_depth_list,
        min_child_weight = min_child_weight_list,
        subsample = subsamp_list,
        colsample_bytree = colsamp_list,
        early_stopping_rounds = early_stops_list,
        nrounds=nrounds_list,
        nfold=nfold_list)
  } else {
    hyper_grid = expand.grid(
      booster = xgb_tech,
      
      tree_method = tree_method,
      eta = eta_list,
      gamma = gamma_list,
      max_depth = max_depth_list,
      min_child_weight = min_child_weight_list,
      subsample = subsamp_list,
      colsample_bytree = colsamp_list,
      early_stopping_rounds = early_stops_list,
      nrounds=nrounds_list,
      nfold=nfold_list)
  }
  
  print(hyper_grid)
  
  grid_rows=nrow(hyper_grid)
  
  results=matrix(0, nrow = grid_rows, ncol=MC_runs)
  
  withProgress(
    message = 'Calculation in progress',
    detail = paste0("Gridrow:",i=1,"/",grid_rows,"; Current iteration:",a=1,"/",MC_runs),
    value = 0,
    {
      
      for(i in 1:grid_rows) {
        
        if (xgb_tech == "dart") {
          
          params = list(
            booster = hyper_grid$booster[i],
            rate_drop = hyper_grid$rate_drop[i],
            skip_drop = hyper_grid$skip_drop[i],
            normalize_type = hyper_grid$normalize_type[i],
            sample_type = hyper_grid$sample_type[i],
            
            tree_method = hyper_grid$tree_method[i],
            eta = hyper_grid$eta[i],
            gamma = hyper_grid$gamma[i],
            max_depth = hyper_grid$max_depth[i],
            min_child_weight = hyper_grid$min_child_weight[i],
            subsample = hyper_grid$subsample[i],
            colsample_bytree = hyper_grid$colsample_bytree[i])

        } else {
          params = list(
            booster = hyper_grid$booster[i],
            tree_method = hyper_grid$tree_method[i],
            eta = hyper_grid$eta[i],
            gamma = hyper_grid$gamma[i],
            max_depth = hyper_grid$max_depth[i],
            min_child_weight = hyper_grid$min_child_weight[i],
            subsample = hyper_grid$subsample[i],
            colsample_bytree = hyper_grid$colsample_bytree[i])
        }
        
        for (a in 1:MC_runs) {
          
          # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
          if (loggy==TRUE) {
            
            for (j in 1:nrow(data)){
              if (data[j,1]=="TNTC") {
                data[j,1]=log10(runif(1, min = tntc_val, max = tntc_mult*tntc_val))
              }
              
              if (data[j,1]=="ND") {
                data[j,1]=log10(runif(1, min = 0, max = nd_val))
              }
            }
          } else {
            
            for (j in 1:nrow(data)){
              if (data[j,1]=="TNTC") {
                data[j,1]=(runif(1, min = tntc_val, max = tntc_mult*tntc_val))
              }
              
              if (data[j,1]=="ND") {
                data[j,1]=(runif(1, min = 0, max = nd_val))
              }
            }
          }
          
          # RANDOMIZE DATA
          if (randomize==TRUE) {
            random_index = sample(1:nrow(data), nrow(data))
            data = data[random_index, ]
          }
          
          model = xgb.cv(data = as.matrix(data[,-1]), label=data[,1], params=params, nrounds=hyper_grid$nrounds[i],early_stopping_rounds = hyper_grid$early_stopping_rounds[i],nfold=hyper_grid$nfold[i], verbose=0)
          
          results[i,a] = min(model$evaluation_log$test_rmse_mean)
          
          incProgress(1/(grid_rows*MC_runs),detail = paste0("Gridrow:",i,"/",grid_rows,"; Current iteration:",a,"/",MC_runs))
        }
      }
    })
  
  # Return the best hyperparameters
  summary_results=round(rowMeans(results),digits=5)
  winner=which.min(summary_results)
  
  output = cbind(summary_results,hyper_grid)
  
  colnames(output)=c("Mean Test RMSE",colnames(hyper_grid))
  
  return(output)
}