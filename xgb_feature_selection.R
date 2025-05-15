xgb_selection = function(xgb_select_data,seed,rv,feats_to_use,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,
                      xgb_standardize,xgb_tree_method,xgb_booster,normalize_type,sample_type,rate_drop,skip_drop,eta,gamma,
                      max_depth,min_child_weight,subsamp,colsamp,nrounds,early_stop,temp_db) {
  
  set.seed(as.integer(seed))
  
  selector = "SHAP"
  feat_data=xgb_select_data[,feats_to_use]
  
  if(xgb_booster == "-") {
    xgb_booster = "gbtree"
  }

  if (xgb_standardize==TRUE) {
    
    for (i in 1:nrow(feat_data)) {
      for (j in 1:ncol(feat_data)) {
        if (is.numeric(feat_data[i,j])==TRUE) {
          
          range = (max(na.omit(feat_data[,j])) - min(na.omit(feat_data[,j])))
          
          if (range == 0) {
            feat_data[i,j] = 0
          } else {
            feat_data[i,j]=(feat_data[i,j] - min(na.omit(feat_data[,j]))) / range
          }
        }
      }
    }
  }
  
  data = data.frame(cbind(xgb_select_data[,rv],feat_data))
  colnames(data) = c(colnames(xgb_select_data)[rv],feats_to_use)
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data=data[!is.na(data[,1]),]
  
  # RANDOMIZE DATA
  if (randomize==TRUE) {
    random_index = sample(1:nrow(data), nrow(data))
    data = data[random_index, ]
  }
  
  # Variable Selection Routine based on SHAP variable importance
  
  if (xgb_booster == "dart") {
    
    params = list(
      booster = xgb_booster,
      rate_drop = rate_drop,
      skip_drop = skip_drop,
      sample_type = sample_type,
      normalize_type = normalize_type,
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
  Iteration_results = matrix(0, nrow=remaining, ncol=7)
  colnames(Iteration_results) = c("Iteration","Lowest_Gain","Gain","Lowest_SHAP","SHAP","RMSE_Train","RMSE_Test")
  smp_size = floor(train_prop * nrow(data))
  temp_data = as.data.frame(data)
  
  withProgress(
    message = 'Calculation in progress',
    detail = paste0("Features remaining:",i=remaining,"; Current iteration:",j=1,"/",MC_runs),
    value = 0,
    {
      
      for(i in 1:remaining) {
        
        RMSE_temp_train = matrix(0,nrow=MC_runs, ncol=1)
        RMSE_temp_test = matrix(0,nrow=MC_runs, ncol=1)
        
        gain_train = matrix(0,nrow=ncol(temp_data)-1,ncol=MC_runs+1)
        shap_train = matrix(0,nrow=ncol(temp_data)-1,ncol=MC_runs+1)
        
        for (j in 1:MC_runs) {
          
          # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
          if (loggy==TRUE) {
            
            for (z in 1:nrow(temp_data)){
              if (temp_data[z,1]=="TNTC") {
                temp_data[z,1]=log10(runif(1, min = rc_lowval, max = rc_upval))
              }
              
              if (temp_data[z,1]=="ND") {
                temp_data[z,1]=log10(runif(1, min = lc_lowval, max = lc_upval))
              }
            }
          } else {
            
            for (z in 1:nrow(temp_data)){
              if (temp_data[z,1]=="TNTC") {
                temp_data[z,1]=(runif(1, min = rc_lowval, max = rc_upval))
              }
              
              if (temp_data[z,1]=="ND") {
                temp_data[z,1]=(runif(1, min = lc_lowval, max = lc_upval))
              }
            }
          }
          
          train_ind = sample(seq_len(nrow(temp_data)), size = smp_size)
          train = temp_data[train_ind, ]
          test = temp_data[-train_ind, ]
          
          temp_model = xgboost(data = as.matrix(train[,-1]), label=train[,1], params=params, early_stopping_rounds=20, nrounds=nrounds, verbose=0)
          
          fit_values = as.numeric(predict(temp_model,as.matrix(train[,-1])))
          RMSE_temp_train[j,1]= sqrt(mean((train[,1] - fit_values)^2))

          predictions = as.numeric(predict(temp_model, as.matrix(test[,-1])))
          RMSE_temp_test[j,1]= sqrt(mean((test[,1] - predictions)^2))
          
          # Recording Gain
          
          xgb_features = as.matrix(xgb.importance(model = temp_model)[,1])
          xgb_gain = as.matrix(xgb.importance(model = temp_model)[,2])
          
          gain_temp=data.frame(cbind(xgb_features,xgb_gain))
          gain_train[,1] = colnames(train[,-1])
          
          for (f in 1:nrow(gain_train)) {
            
            feat_name = gain_train[f,1]
            
            if (feat_name %in% gain_temp$Feature) {
              gain_train[f,j+1] = as.numeric(unlist(subset(gain_temp, gain_temp$Feature == feat_name, select = "Gain")))
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
            current_feat = shap_train[c,1]
            shap_train[c,j+1] = shap_temp[shap_temp[,1] == current_feat,2]
          }
          
          incProgress(1/(remaining*MC_runs),detail = paste0("Features remaining:",ncol(train)-1,"; Current iteration:",j,"/",MC_runs))
          
        } #END the MC/Iterations Loop
        
        RMSE_mean_train = mean(RMSE_temp_train)
        RMSE_mean_test = mean(RMSE_temp_test)
        
        # Compute average Gain across iterations and find the feature with the lowest mean Gain
        
        gain_train = as.data.frame(gain_train)
        gain_result = matrix(0,nrow=nrow(gain_train),ncol=ncol(gain_train)-1)
        
        for (h in 1:nrow(gain_result)) {
          
          gain_result[h,] = as.numeric(unlist(gain_train[h,-1]))
          
        }
        
        lowest_gain = min(rowMeans(gain_result)) 
        loser_gain = which.min(rowMeans(gain_result))
        loser_gain_name = gain_train[loser_gain,1]
        
        # Compute average SHAP value and find the feature with the lowest mean SHAP
        shap_train = as.data.frame(shap_train)
        shap_result = matrix(0,nrow=nrow(shap_train),ncol=ncol(shap_train)-1)
        
        for (h in 1:nrow(shap_result)) {
          shap_result[h,] = as.numeric(shap_train[h,-1])
        }
        
        lowest_shap = min(rowMeans(shap_result)) 
        loser_shap = which.min(rowMeans(shap_result))
        loser_shap_name = shap_train[loser_shap,1]
        
        # fill out sub-iteration results
        
        Iteration_results[i,1] = as.numeric(i)
        Iteration_results[i,2] = loser_gain_name
        Iteration_results[i,3] = round(lowest_gain,5)
        Iteration_results[i,4] = loser_shap_name
        Iteration_results[i,5] = round(lowest_shap,5)
        Iteration_results[i,6] = round(RMSE_mean_train,4)
        Iteration_results[i,7] = round(RMSE_mean_test,4)
        
        # Drop lowest SHAP or Gain
        
        if (selector=="Gain") {
          
          temp_data = temp_data[ , -which(colnames(temp_data) %in% loser_gain_name)]
          
        } else {
          
          temp_data = temp_data[ , -which(colnames(temp_data) %in% loser_shap_name)]
        }
      } #END the Feature Iteration Loop
    })
  
  dbWriteTable(temp_db, "xgb_selection_results", data.frame(Iteration_results), overwrite = TRUE)

  Iteration_results = Iteration_results[,c(1,4:7)]
  Iteration_results
}