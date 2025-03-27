library(rsample)
library(xgboost)
library(dplyr)
library(SHAPforxgboost)
library(permimp)
library(Metrics)
library(lime)
library(pdp)
library(DBI)
library(RSQLite)

xgb_select = function(xgb_select_data,resvar,coves_to_use,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,xgb_standardize,xgb_tree_method,xgb_booster,dart_normalize_type,
                      dart_sample_type,rate_drop,skip_drop,eta,gamma,max_depth,min_child_weight,subsamp,colsamp,nrounds,early_stop,test_weight,temp_db) {
  
  selector = "SHAP"
  cove_data=xgb_select_data[,coves_to_use]
  
  if(xgb_booster == "-") {
    xgb_booster = "gbtree"
  }

  if (xgb_standardize==TRUE) {
    
    for (i in 1:nrow(cove_data)) {
      for (j in 1:ncol(cove_data)) {
        if (is.numeric(cove_data[i,j])==TRUE) {
          cove_data[i,j]=(cove_data[i,j] - min(na.omit(cove_data[,j]))) / (max(na.omit(cove_data[,j])) - min(na.omit(cove_data[,j])))
        }
      }
    }
  }
  
  ncoves = ncol(cove_data)
  data = cbind(xgb_select_data[,resvar],cove_data)
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data=data[!is.na(data[,1]),]
  
  # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
  if (loggy==TRUE) {
    
    for (j in 1:nrow(data)){
      if (data[j,1]=="TNTC") {
        data[j,1]=log10(runif(1, min = rc_lowval, max = rc_upval))
      }
      
      if (data[j,1]=="ND") {
        data[j,1]=log10(runif(1, min = lc_lowval, max = lc_upval))
      }
    }
  } else {
    
    for (j in 1:nrow(data)){
      if (data[j,1]=="TNTC") {
        data[j,1]=(runif(1, min = rc_lowval, max = rc_upval))
      }
      
      if (data[j,1]=="ND") {
        data[j,1]=(runif(1, min = lc_lowval, max = lc_upval))
      }
    }
  }
  
  # RANDOMIZE DATA
  if (randomize==TRUE) {
    random_index = sample(1:nrow(data), nrow(data))
    data = data[random_index, ]
  }
  
  # Variable Selection Routine based on variable importance in training data model
  
  if (xgb_booster == "dart") {
    
    params = list(
      booster = xgb_booster,
      rate_drop = drop_rate,
      skip_drop = skip_drop,
      dart_sample_type = dart_sample_type,
      dart_normalize_type = dart_normalize_type,
      tree_method = xgb_tree_method,
      eta = eta,
      gamma = gamma,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsamp,
      colsample_bytree = colsamp
    )
  } else {
    params = list(
      booster = xgb_booster,
      tree_method = xgb_tree_method,
      eta = eta,
      gamma = gamma,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsamp,
      colsample_bytree = colsamp
    )
  }
  
  remaining = ncol(data)-2
  Iteration_results = matrix(0, nrow=remaining, ncol=8)
  colnames(Iteration_results) = c("Iteration","Worst Gain","Gain","Worst SHAP","SHAP","RMSE_Train","RMSE_Test","RMSE_Weighted")
  smp_size = floor(train_prop * nrow(data))
  temp_data = data
  
  withProgress(
    message = 'Calculation in progress',
    detail = paste0("Covariates remaining:",i=remaining,"; Current iteration:",j=1,"/",MC_runs),
    value = 0,
    {
      
      for(i in 1:remaining) {
        
        RMSE_temp_train = matrix(0,nrow=MC_runs, ncol=1)
        RMSE_temp_test = matrix(0,nrow=MC_runs, ncol=1)
        
        gain_train = matrix(0,nrow=ncol(temp_data)-1,ncol=MC_runs+1)
        shap_train = matrix(0,nrow=ncol(temp_data)-1,ncol=MC_runs+1)
        
        for (j in 1:MC_runs) {
          train_ind = sample(seq_len(nrow(temp_data)), size = smp_size)
          train = temp_data[train_ind, ]
          test = temp_data[-train_ind, ]
          
          temp_model = xgboost(data = as.matrix(train[,-1]), label=as.matrix(train[,1]), params=params, early_stopping_rounds=early_stop, nrounds=nrounds, verbose=0)
          
          fit_values = predict(temp_model,as.matrix(train[,-1]))
          RMSE_temp_train[j,1]=rmse(fit_values, train[,1])
          
          predictions = predict(temp_model, as.matrix(test[,-1]))
          RMSE_temp_test[j,1]=rmse(predictions, test[,1])
          
          # Recording Gain
          
          xgb_covariates = as.matrix(xgb.importance(model = temp_model)[,1])
          xgb_gain = as.matrix(xgb.importance(model = temp_model)[,2])
          
          gain_temp=data.frame(cbind(xgb_covariates,xgb_gain))
          gain_train[,1] = colnames(train[,-1])
          
          for (f in 1:nrow(gain_train)) {
            
            cove_name = gain_train[f,1]
            
            if (cove_name %in% gain_temp$Feature) {
              gain_train[f,j+1] = as.numeric(unlist(subset(gain_temp, gain_temp$Feature == cove_name, select = "Gain")))
            } else {
              gain_train[f,j+1] = 0.0000
            }
          }
          
          # Recording SHAP
          
          shap_values = shap.values(xgb_model = temp_model, X_train = as.matrix(train[,-1]))
          mean_shaps = shap_values$mean_shap_score
          shap_names = names(mean_shaps)
          shap_temp = cbind(shap_names,mean_shaps)
          
          shap_train[,1] = colnames(train[,-1])
          
          for (c in 1:nrow(shap_train)) {
            current_cove = shap_train[c,1]
            shap_train[c,j+1] = shap_temp[shap_temp[,1] == current_cove,2]
          }
          
          # cat("vars remaining=", ncol(train_coves), "iteration=", j,"\n")
          
          incProgress(1/(remaining*MC_runs),detail = paste0("Covariates remaining:",ncol(train)-1,"; Current iteration:",j,"/",MC_runs))
          
        } #END the MC/Iterations Loop
        
        RMSE_mean_train = mean(RMSE_temp_train)
        RMSE_mean_test = mean(RMSE_temp_test)
        
        # Compute average Gain across iterations and find the covariate with the lowest Gain
        
        gain_train = as.data.frame(gain_train)
        gain_result = matrix(0,nrow=nrow(gain_train),ncol=ncol(gain_train)-1)
        
        for (h in 1:nrow(gain_result)) {
          
          gain_result[h,] = as.numeric(unlist(gain_train[h,-1]))
          
        }
        
        lowest_gain = min(rowMeans(gain_result)) 
        loser_gain = which.min(rowMeans(gain_result))
        loser_gain_name = gain_train[loser_gain,1]
        
        # Compute average Shap value and find the covariate with the lowest Shap
        
        shap_train = as.data.frame(shap_train)
        shap_result = matrix(0,nrow=nrow(shap_train),ncol=ncol(shap_train)-1)
        
        for (h in 1:nrow(shap_result)) {
          shap_result[h,] = as.numeric(shap_train[h,-1])
        }
        
        lowest_shap = min(rowMeans(shap_result)) 
        loser_shap = which.min(rowMeans(shap_result))
        loser_shap_name = shap_train[loser_shap,1]
        
        # fill out sub-iteration results
        
        Iteration_results[i,1] = i
        Iteration_results[i,2] = loser_gain_name
        Iteration_results[i,3] = lowest_gain
        Iteration_results[i,4] = loser_shap_name
        Iteration_results[i,5] = lowest_shap
        Iteration_results[i,6] = RMSE_mean_train
        Iteration_results[i,7] = RMSE_mean_test
        Iteration_results[i,8] =test_weight*RMSE_mean_test+(1-test_weight)*RMSE_mean_train
        
        # Drop lowest SHAP or Gain
        
        if (selector=="Gain") {
          
          temp_data = temp_data[ , -which(colnames(temp_data) %in% loser_gain_name)]
          
        } else {
          
          temp_data = temp_data[ , -which(colnames(temp_data) %in% loser_shap_name)]
        }
      } #END the Iteration Loop
    })
  
  dbWriteTable(temp_db, "xgb_select_results", data.frame(Iteration_results), overwrite = TRUE)

  Iteration_results = Iteration_results[,c(1,4:8)]
  Iteration_results
}