xgbcl_selection = function(data0,seed,rv,feats_to_use,ignored_rows,crit_value,eval_metric,lc_val,rc_val,lc_lowval,lc_upval,rc_lowval,rc_upval,train_prop,MC_runs,loggy,randomize,
                      standardize,binarize,xgb_tree_method,xgb_booster,normalize_type,sample_type,rate_drop,skip_drop,eta,gamma,
                      max_depth,min_child_weight,subsamp,colsamp,nrounds,early_stop,temp_db,MC_subbin,create_data) {

  set.seed(seed)
  
  selector = "SHAP"
  feat_data=data0[,feats_to_use]
  
  if(xgb_booster == "-") {
    xgb_booster = "gbtree"
  }
  
  data1 = create_data(data0,rv,feats_to_use,ignored_rows,randomize,standardize)
  data = data1[,-1]
  
  # Variable Selection Routine based on SHAP variable importance
  
  if (xgb_booster == "dart") {
    
    params = list(
      objective = "binary:logistic",
      eval_metric = eval_metric,
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
      objective = "binary:logistic",
      eval_metric = eval_metric,
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
  colnames(Iteration_results) = c("Iteration","Lowest_Gain","Gain","Lowest_SHAP","SHAP","LnLoss_Train","LnLoss_Test")
  smp_size = floor(train_prop * nrow(data))
  temp_data = as.data.frame(data)
  
  withProgress(
    message = 'Calculation in progress',
    detail = paste0("Features remaining:",i=remaining,"; Current iteration:",j=1,"/",MC_runs),
    value = 0,
    {
      
      for(i in 1:remaining) {
        
        LL_temp_train = matrix(0,nrow=MC_runs, ncol=1)
        LL_temp_test = matrix(0,nrow=MC_runs, ncol=1)
        
        gain_train = matrix(0,nrow=ncol(temp_data)-1,ncol=MC_runs+1)
        shap_train = matrix(0,nrow=ncol(temp_data)-1,ncol=MC_runs+1)
        
        for (m in 1:MC_runs) {
          
          MC_data = MC_subbin(temp_data,loggy,lc_val,lc_lowval,lc_upval,rc_val,rc_lowval)
          
          if (binarize) {
            for (c in 1:nrow(MC_data)) {
              MC_data[c, 1] = ifelse(test = MC_data[c, 1] >= crit_value, yes = 1, no = 0)
            }
          }
          
          train_ind = sample(seq_len(nrow(MC_data)), size = smp_size)
          train = MC_data[train_ind, ]
          test = MC_data[-train_ind, ]
          
          temp_model = xgboost(data = as.matrix(train[,-1]), label=train[,1], params=params, early_stopping_rounds=20, nrounds=nrounds, verbose=0)
          
          fit_values = as.numeric(predict(temp_model,as.matrix(train[,-1])))
          predictions = as.numeric(predict(temp_model, as.matrix(test[,-1])))
          
          sumLLfit = 0
          sumLLpred = 0
          
          for (s in 1:length(fit_values)) {
              sumLLfit = sumLLfit + (train[s,1]*log(fit_values[s]) + (1-train[s,1])*(log(1-fit_values[s])))
          }
          
          for (s in 1:length(predictions)) {
            sumLLpred = sumLLpred + (test[s,1]*log(predictions[s]) + (1-test[s,1])*(log(1-predictions[s])))
          }
          
          LLfit = -sumLLfit
          LLpred = -sumLLpred
          
          LL_temp_train[m,1]= LLfit
          LL_temp_test[m,1]= LLpred

          # Recording Gain
          
          xgb_features = as.matrix(xgb.importance(model = temp_model)[,1])
          xgb_gain = as.matrix(xgb.importance(model = temp_model)[,2])
          
          gain_temp=data.frame(cbind(xgb_features,xgb_gain))
          gain_train[,1] = colnames(train[,-1])
          
          for (f in 1:nrow(gain_train)) {
            
            feat_name = gain_train[f,1]
            
            if (feat_name %in% gain_temp$Feature) {
              gain_train[f,m+1] = as.numeric(unlist(subset(gain_temp, gain_temp$Feature == feat_name, select = "Gain")))
            } else {
              gain_train[f,m+1] = 0.0000
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
            shap_train[c,m+1] = shap_temp[shap_temp[,1] == current_feat,2]
          }
          
          incProgress(1/(remaining*MC_runs),detail = paste0("Features remaining:",ncol(train)-1,"; Current iteration:",m,"/",MC_runs))
          
        } #END the MC/Iterations Loop
        
        LL_mean_train = mean(LL_temp_train)
        LL_mean_test = mean(LL_temp_test)
        
        # Compute average Gain across iterations and find the feature with the lowest average Gain
        
        gain_train = as.data.frame(gain_train)
        gain_result = matrix(0,nrow=nrow(gain_train),ncol=ncol(gain_train)-1)
        
        for (h in 1:nrow(gain_result)) {
          
          gain_result[h,] = as.numeric(unlist(gain_train[h,-1]))
          
        }
        
        lowest_gain = min(rowMeans(gain_result)) 
        loser_gain = which.min(rowMeans(gain_result))
        loser_gain_name = gain_train[loser_gain,1]
        
        # Compute average SHAP value and find the feature with the lowest average SHAP
        
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
        Iteration_results[i,6] = round(LL_mean_train,4)
        Iteration_results[i,7] = round(LL_mean_test,4)
        
        # Drop lowest SHAP or Gain
        
        if (selector=="Gain") {
          
          temp_data = temp_data[ , -which(colnames(temp_data) %in% loser_gain_name)]
          
        } else {
          
          temp_data = temp_data[ , -which(colnames(temp_data) %in% loser_shap_name)]
        }
      } #END the Feature Iteration Loop
    })
  
  dbWriteTable(temp_db, "xgbcl_selection_results", data.frame(Iteration_results), overwrite = TRUE)

  Iteration_results = Iteration_results[,c(1,4:7)]
  Iteration_results
}