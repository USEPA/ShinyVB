xgb_final = function(data,seed,
                     lc_lowval,
                     lc_upval,
                     rc_lowval,
                     rc_upval,
                     loggy,
                     randomize,
                     xgb_standardize,
                     xgb_tree_method,
                     xgb_booster,
                     normalize_type,
                     sample_type,rate_drop,
                     skip_drop,
                     eta,
                     gamma,
                     max_depth,
                     min_child_weight,
                     subsamp,
                     colsamp,
                     nrounds) {
  
  set.seed(seed)
  
  if (xgb_standardize==TRUE) {
    
    for (i in 1:nrow(data)) {
      for (j in 2:ncol(data)) {
        if (is.numeric(data[i,j])==TRUE) {
          
          range = (max(na.omit(data[,j])) - min(na.omit(data[,j])))
          
          if (range == 0) {
            data[i,j] = 0
          } else {
            data[i,j]=(data[i,j] - min(na.omit(data[,j]))) / range
          }
        }
      }
    }
  }
  
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
  
  xgb_final_model = xgboost(data = as.matrix(data[,-1]),label=data[,1], params=params, early_stopping_rounds=20, nrounds=nrounds, verbose=0)
  xgb_fits = data.frame(predict(xgb_final_model, newdata=as.matrix(data[,-1])))
  colnames(xgb_fits) = "Fitted_Values"
  
  if (ncol(data) > 2) {
    shap_values = shap.values(xgb_model = xgb_final_model, X_train = as.matrix(data[,-1]))
    mean_shaps = round(shap_values$mean_shap_score,4)
    shap_names = names(mean_shaps)
    shap = cbind(shap_names,mean_shaps)
  } else {
    shap = data.frame("Feature" = colnames(data)[[2]], "Mean_SHAP" = 0)
  }
  
  return(list(xgb_final_model,xgb_fits,shap))
}