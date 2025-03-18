library(rsample)
library(shiny)
library(bslib)
library(xgboost)
library(dplyr)
library(permimp)

xgb_hyper = reactive(function(current_data,response_var,coves_to_use,nd_val,tntc_val,tntc_multy,MC_runs,
                     loggy,randomize,xgb_tech,drop_rates,etas,gammas,max_depths,min_child_weights,subsamples,
                     colsample_bytrees,nroundss,early_stopping_roundss,nfolds,progress_list) {
  
  cove_data=current_data[,coves_to_use]
  ncoves = ncol(cove_data)
  cove_names = colnames(cove_data)
  data = cbind(current_data[,response_var],cove_data)
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data=data[!is.na(data[,1]),]
  
  hyper_grid = expand.grid(
    booster = xgb_tech,
    rate_drop = drop_rates,
    eta = etas,
    gamma= gammas,
    max_depth = max_depths,
    min_child_weight = min_child_weights,
    subsample = subsamples,
    colsample_bytree = colsample_bytrees,
    early_stopping_rounds = early_stopping_roundss,
    nrounds=nroundss,
    nfold=nfolds,
  )
  
  grid_rows=as.numeric(nrow(hyper_grid))
  
  results=matrix(0, nrow = grid_rows, ncol=MC_runs)
  
  withProgress( 
    message = 'Calculation in progress', 
    detail = paste0("Gridrow ",a=1," iteration ",i=1, "completed."), 
    value = 0,
    {
      
      for(i in 1:grid_rows) {
        
        params = list(
          booster = hyper_grid$xgb_tech,
          rate_drop = hyper_grid$drop_rates[i],
          eta = hyper_grid$etas[i],
          gamma = hyper_grid$gammas[i],
          max_depth = hyper_grid$max_depths[i],
          min_child_weight = hyper_grid$min_child_weights[i],
          subsample = hyper_grid$subsamples[i],
          colsample_bytree = hyper_grid$colsample_bytrees[i],
          early_stopping_rounds = hyper_grid$early_stopping_roundss[i],
          nrounds = hyper_grid$nroundss[i],
          nfold = hyper_grid$nfolds[i],
        )
        
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
          
          model = xgb.cv(data = as.matrix(data[,-1]), label=data[,1], params=params, verbose=0)
          
          results[i,a] = min(model$evaluation_log$test_rmse_mean)
          
          incProgress(1/(length(grid_rows)*MC_runs),detail = paste0("Gridrow ",a," iteration ",i, "completed."))
        }
      }
      text("Done computing!")
    }
  )
  
  # Return the best hyperparameters
  summary_results=rowMeans(results)
  winner=which.min(summary_results)
  
  return(best_values=hyper_grid[winner,])
})