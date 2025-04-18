xgb_select = function(xgb_data,ignored_rows,seed,resvar,coves_to_use,lc_lowval,lc_upval,rc_lowval,rc_upval,loggy,randomize,xgb_standardize,xgb_tree_method,xgb_booster,dart_normalize_type,
                      dart_sample_type,rate_drop,skip_drop,eta,gamma,max_depth,min_child_weight,subsamp,colsamp,nrounds) {
  
  set.seed(seed)
  
  if (is.null(ignored_rows)) {
    data = xgb_data
  } else {
    data = xgb_data[-ignored_rows,]
  }
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data = data[!is.na(data[,resvar]), ]
  
  data = data[,c(resvar,coves_to_use)]
  
  if(xgb_booster == "-") {
    xgb_booster = "gbtree"
  }
  
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
  
  xgb_final_model = xgboost(data = as.matrix(data[,-1]),label=data[,1], params=params, early_stopping_rounds=20, nrounds=nrounds, verbose=0)
  
}