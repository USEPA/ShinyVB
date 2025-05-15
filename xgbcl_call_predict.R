xgbcl_call_predict = function(current_data,
                       rv,
                       id_var,
                       seed,
                       ignored_rows,
                       feats_to_use,
                       eval_metric,
                       lc_lowval,
                       lc_upval,
                       rc_lowval,
                       rc_upval,
                       train_prop,
                       MC_runs,
                       nfolds,
                       loggy,
                       randomize,
                       standardize,
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
  
  set.seed(seed)
  
  data = current_data
  
  if (is.null(ignored_rows)) {
    data = data
  } else {
    data = data[-ignored_rows,]
  }
  
  if (binarize) {
    new_Y = ifelse(test = data[,rv] >= crit_value, yes = 1, no = 0)
    data[,rv] = new_Y
  }

  # REMOVE NA'S FROM RESPONSE VARIABLE
  data = data[!is.na(data[,rv]), ]
  
  #Randomly shuffle the data
  if (randomize == TRUE) {
    data = data[sample(nrow(data)), ]
  }
  
  feat_data = data[,feats_to_use]
  
  std_feat_data = feat_data
  
  # Min/Max Standardize the features
  if (standardize == TRUE) {
    for (i in 1:nrow(std_feat_data)) {
      for (j in 1:ncol(std_feat_data)) {
        if (is.numeric(std_feat_data[i, j]) == TRUE) {
          if (max(na.omit(std_feat_data[, j])) - min(na.omit(std_feat_data[, j])) == 0) {
            std_feat_data[i, j] = 0
          } else {
            std_feat_data[i, j] = (std_feat_data[i, j] - min(na.omit(std_feat_data[, j]))) / (max(na.omit(std_feat_data[, j])) - min(na.omit(std_feat_data[, j])))
          }
        }
      }
    }
  }
  
  # Add the standardized features back into the data frame for analysis
  dataset = cbind(data[,id_var],data[,rv],std_feat_data)
  colnames(dataset)[1] = colnames(current_data)[id_var]
  colnames(dataset)[2] = colnames(current_data)[rv]
  
  #Create n folds
  tot_folds = nfolds
  folds = cut(seq(1, nrow(dataset)), breaks = tot_folds, labels = FALSE)
  
  prediction_results = matrix(0, nrow = 0, ncol = length(feats_to_use) + 3)
  prediction_results = data.frame(prediction_results)
  
  pred_shapes = matrix(0, nrow = length(feats_to_use), ncol = tot_folds+1)
  pred_shapes[,1] = feats_to_use
  pred_shapes = data.frame(pred_shapes)
  
  #Perform cross validation
  for (i in 1:tot_folds) {
    fold_num = i
    
    testIndices = which(folds == i, arr.ind = TRUE)
    testData = dataset[testIndices, ]
    trainData = dataset[-testIndices, ]
    
    prediction_fold_result = xgbcl_pred_fold_errors(
      trainData,
      testData,
      rv,
      feats_to_use,
      eval_metric,
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
      crit_value
    )
    
    prediction_results1 = cbind(testData[,1],prediction_fold_result[[1]],testData[,3:ncol(testData)])
    prediction_results = rbind(prediction_results, prediction_results1)
    
    pred_shapes[,i+1] = prediction_fold_result[[2]]
  }
  
  prediction_results[,3:ncol(prediction_results)] = round(prediction_results[,3:ncol(prediction_results)],4)
  prediction_results = prediction_results[order(prediction_results[,1]),]
  colnames(prediction_results) = c(colnames(current_data)[1],colnames(current_data)[rv], "Predictive_Prob", feats_to_use)
  xgb_predictions = data.frame(prediction_results)
  
  pred_shapes = data.frame(cbind(pred_shapes[,1],format(round(rowMeans(pred_shapes[,-1]),4),scientific=F)))
  pred_shapes = pred_shapes[order(pred_shapes[,2],decreasing=T),]
  
  return(list(xgb_predictions,pred_shapes))
}