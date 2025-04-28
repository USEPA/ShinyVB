xgb_pred_fold_errors = function(train_data,
                                test_data,
                                resvar,
                                coves_to_use,
                                lc_lowval,
                                lc_upval,
                                rc_lowval,
                                rc_upval,
                                train_prop,
                                MC_runs,
                                loggy,
                                xgb_hyper_metric,
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
    detail = paste("MC runs:", x = MC_runs,"; Fold: ",fold_num),
    value = (fold_num/tot_folds)-0.2,
    {
      
      temp_preds = matrix(0, nrow = nrow(test_data), ncol = 2*MC_runs)
      temp_preds = data.frame(temp_preds)
      
      
      for (i in 1:MC_runs) {
        
        incProgress(1/(MC_runs*tot_folds), detail = paste("MC run: ",i,"/",MC_runs,"; Fold: ",fold_num,"/",tot_folds))
        
        # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
        if (loggy == TRUE) {
          for (j in 1:nrow(train_data)) {
            if (train_data[j, 2] == "TNTC") {
              train_data[j, 2] = log10(runif(1, min = rc_lowval, max = rc_upval))
            }
            
            if (train_data[j, 2] == "ND") {
              train_data[j, 2] = log10(runif(1, min = lc_lowval, max = lc_upval))
            }
          }
          
          for (j in 1:nrow(test_data)) {
            if (test_data[j, 2] == "TNTC") {
              test_data[j, 2] = log10(runif(1, min = rc_lowval, max = rc_upval))
            }
            
            if (test_data[j, 2] == "ND") {
              test_data[j, 2] = log10(runif(1, min = lc_lowval, max = lc_upval))
            }
          }
        } else {
          for (j in 1:nrow(train_data)) {
            if (train_data[j, 2] == "TNTC") {
              train_data[j, 2] = (runif(1, min = rc_lowval, max = rc_upval))
            }
            
            if (train_data[j, 2] == "ND") {
              train_data[j, 2] = (runif(1, min = lc_lowval, max = lc_upval))
            }
          }
          for (j in 1:nrow(test_data)) {
            if (test_data[j, 2] == "TNTC") {
              test_data[j, 2] = (runif(1, min = rc_lowval, max = rc_upval))
            }
            
            if (test_data[j, 2] == "ND") {
              test_data[j, 2] = (runif(1, min = lc_lowval, max = lc_upval))
            }
          }
        }
        
        temp_preds[,2*i-1] = test_data[,2]
        
        # Prepare data for XGBoost
        X_train0 = train_data[,-1]
        X_train = as.matrix(X_train0[,-1])
        y_train = train_data[,2]
        
        X_test0 = test_data[,-1]
        X_test = as.matrix(X_test0[,-1])
        y_test = test_data[,2]
        
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
        
      } #End the MC runs
      
    })
  
  even_columns = temp_preds[,seq(2, ncol(temp_preds), by = 2)]
  odd_columns = temp_preds[,seq(1, ncol(temp_preds), by = 2)]
  
  obs_mean_values = rowMeans(odd_columns)
  pred_mean_values = rowMeans(even_columns)
  fold_preds = cbind(obs_mean_values,pred_mean_values)
  
  return(fold_preds)
}