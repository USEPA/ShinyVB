xgbcl_pred_fold_errors = function(train_data,
                                test_data,
                                rv,
                                feats_to_use,
                                eval_metric,
                                lc_val,
                                rc_val,
                                lc_lowval,
                                lc_upval,
                                rc_lowval,
                                rc_upval,
                                train_prop,
                                MC_runs,
                                loggy,
                                tot_folds,
                                fold_num,
                                xgbcl_tree_method,
                                xgbcl_booster,
                                dartcl_normalize_type,
                                dartcl_sample_type,
                                ratecl_drop,
                                skipcl_drop,
                                eta,
                                gamma,
                                max_depth,
                                min_child_weight,
                                subsamp,
                                colsamp,
                                nrounds,
                                binarize,
                                crit_value) {
  
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

        # SUBSTITUTE random value FOR RESPONSE VARIABLE non-values and then binarize
        if (loggy == TRUE) {
          for (j in 1:nrow(train_data)) {
            if (train_data[j, 2] == rc_val) {
              train_data[j, 2] = log10(runif(1, min = rc_lowval, max = rc_upval))
              ifelse(test = train_data[j, 2] >= crit_value, yes = 1, no = 0)
            }
            
            if (train_data[j, 2] == lc_val) {
              train_data[j, 2] = log10(runif(1, min = lc_lowval, max = lc_upval))
              ifelse(test = train_data[j, 2] >= crit_value, yes = 1, no = 0)
            }
          }
          
          for (j in 1:nrow(test_data)) {
            if (test_data[j, 2] == rc_val) {
              test_data[j, 2] = log10(runif(1, min = rc_lowval, max = rc_upval))
              ifelse(test = test_data[j, 2] >= crit_value, yes = 1, no = 0)
            }
            
            if (test_data[j, 2] == lc_val) {
              test_data[j, 2] = log10(runif(1, min = lc_lowval, max = lc_upval))
              ifelse(test = test_data[j, 2] >= crit_value, yes = 1, no = 0)
            }
          }
        } else {
          for (j in 1:nrow(train_data)) {
            if (train_data[j, 2] == rc_val) {
              train_data[j, 2] = (runif(1, min = rc_lowval, max = rc_upval))
              ifelse(test = train_data[j, 2] >= crit_value, yes = 1, no = 0)
            }
            
            if (train_data[j, 2] == lc_val) {
              train_data[j, 2] = (runif(1, min = lc_lowval, max = lc_upval))
              ifelse(test = train_data[j, 2] >= crit_value, yes = 1, no = 0)
            }
          }
          for (j in 1:nrow(test_data)) {
            if (test_data[j, 2] == rc_val) {
              test_data[j, 2] = (runif(1, min = rc_lowval, max = rc_upval))
              ifelse(test = test_data[j, 2] >= crit_value, yes = 1, no = 0)
            }
            
            if (test_data[j, 2] == lc_val) {
              test_data[j, 2] = (runif(1, min = lc_lowval, max = lc_upval))
              ifelse(test = test_data[j, 2] >= crit_value, yes = 1, no = 0)
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
        
        # Set up XGBoost parameters
        
        if (xgbcl_booster == "dart") {
          
          params = list(
            objective = "binary:logistic",
            eval_metric = eval_metric,
            booster = xgbcl_booster,
            rate_drop = ratecl_drop,
            skip_drop = skipcl_drop,
            sample_type = dartcl_sample_type,
            normalize_type = dartcl_normalize_type,
            tree_method = xgbcl_tree_method,
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
            booster = xgbcl_booster,
            tree_method = xgbcl_tree_method,
            eta = eta,
            gamma = gamma,
            max_depth = max_depth,
            min_child_weight = min_child_weight,
            subsample = subsamp,
            colsample_bytree = colsamp
          )
        }
        
        #train a model
        model = xgboost(data = X_train,label=y_train, params=params, early_stopping_rounds=15, nrounds=nrounds, verbose=0)
        
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
  
  obs_mean_values = ifelse(test = rowMeans(odd_columns) >= 0.5, yes = 1, no = 0)
  pred_mean_values = rowMeans(even_columns)
  fold_preds = cbind(obs_mean_values,pred_mean_values)
  
  temp_shapes = data.frame(temp_shapes[,-1])
  
  MC_shapes = rowMeans(temp_shapes)
  
  return(list(fold_preds,MC_shapes))
}