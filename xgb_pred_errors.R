xgb_pred_fold_errors = function(train_data,
                                test_data,
                                rv,
                                feats_to_use,
                                lc_val,
                                lc_lowval,
                                lc_upval,
                                rc_val,
                                rc_lowval,
                                rc_upval,
                                train_prop,
                                MC_runs,
                                loggy,
                                tot_folds,
                                fold_num,
                                eta,
                                gamma,
                                max_depth,
                                min_child_weight,
                                subsamp,
                                colsamp,
                                nrounds) {
  
  withProgress(
    message = 'HP Prediction Progress',
    detail = paste("MC runs:", x = 1,"/",MC_runs,"; Fold: ",y=1,"/",tot_folds),
    value = (fold_num/tot_folds-1/tot_folds),
    {
      
      temp_preds = matrix(0, nrow = nrow(test_data), ncol = 2*MC_runs)
      temp_preds = data.frame(temp_preds)
      
      temp_shapes = matrix(0, nrow = length(feats_to_use), ncol = MC_runs+1)
      temp_shapes = data.frame(temp_shapes)
      temp_shapes[,1] = feats_to_use
      
      
      for (i in 1:MC_runs) {
        
        trainData = MC_subbin(train_data,loggy,lc_val,lc_lowval,lc_upval,rc_val,rc_lowval,rc_upval)
        testData = MC_subbin(test_data,loggy,lc_val,lc_lowval,lc_upval,rc_val,rc_lowval,rc_upval)
        
        temp_preds[,2*i-1] = testData[,1]
        
        # Prepare data for XGBoost
        X_train0 = trainData[,-1]
        X_train = as.matrix(X_train0[,-1])
        y_train = trainData[,1]
        
        X_test0 = test_data[,-1]
        X_test = as.matrix(X_test0[,-1])
        y_test = test_data[,1]
        
        params = list(
          eta = eta,
          gamma = gamma,
          max_depth = max_depth,
          min_child_weight = min_child_weight,
          subsample = subsamp,
          colsample_bytree = colsamp
        )
        
        #train a model
        model = xgboost(data = X_train,label=y_train, params=params, early_stopping_rounds=10, nrounds=nrounds, verbose=0)
        
        preds = predict(model, X_test)
        temp_preds[,2*i] = round(preds,3)
        
        shap_values = shap.values(xgb_model = model, X_train = X_train)
        mean_shaps = shap_values$mean_shap_score
        shap_names = names(mean_shaps)
        shap_temp = data.frame(cbind(shap_names,mean_shaps))
        
        for (c in 1:nrow(temp_shapes)) {
          current_cove = temp_shapes[c,1]
          temp_shapes[c,i+1] = as.numeric(shap_temp[shap_temp[,1] == current_cove,2])
        }
        
        incProgress(1/(MC_runs*tot_folds), detail = paste("MC run: ",i,"/",MC_runs,"; Fold: ",fold_num,"/",tot_folds))
        
      } #End the MC runs
      
    })
  
  even_columns = temp_preds[,seq(2, ncol(temp_preds), by = 2)]
  odd_columns = temp_preds[,seq(1, ncol(temp_preds), by = 2)]
  
  obs_mean_values = rowMeans(odd_columns)
  pred_mean_values = rowMeans(even_columns)
  fold_preds = cbind(obs_mean_values,pred_mean_values)
  
  temp_shapes = data.frame(temp_shapes[,-1])
  
  MC_shapes = rowMeans(temp_shapes)
  
  return(list(fold_preds,MC_shapes))
}