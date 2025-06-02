xgbcl_call_predict = function(data0,
                       rv,
                       id_var,
                       seed,
                       ignored_rows,
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
  
  data = create_data(data0,rv,feats_to_use,ignored_rows,randomize,standardize)
  
  #Create n folds
  tot_folds = nfolds
  folds = cut(seq(1, nrow(data)), breaks = tot_folds, labels = FALSE)
  
  prediction_results = matrix(0, nrow = 0, ncol = length(feats_to_use) + 3)
  prediction_results = data.frame(prediction_results)
  
  pred_shapes = matrix(0, nrow = length(feats_to_use), ncol = tot_folds+1)
  pred_shapes[,1] = feats_to_use
  pred_shapes = data.frame(pred_shapes)
  
  #Perform cross validation
  for (i in 1:tot_folds) {
    fold_num = i
    
    testIndices = which(folds == i, arr.ind = TRUE)
    testData0 = dataset[testIndices, ]
    trainData0 = dataset[-testIndices, ]
    
    testData = testData0[,-1]
    trainData = testData0[,-1]
    
    prediction_fold_result = xgbcl_pred_fold_errors(
      trainData,
      testData,
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
      crit_value
    )
    
    prediction_results1 = cbind(testData0[,1],prediction_fold_result[[1]],testData[,2:ncol(testData)])
    prediction_results = rbind(prediction_results, prediction_results1)
    
    pred_shapes[,i+1] = prediction_fold_result[[2]]
  }
  
  prediction_results[,3:ncol(prediction_results)] = round(prediction_results[,3:ncol(prediction_results)],4)
  prediction_results = prediction_results[order(prediction_results[,1]),]
  colnames(prediction_results) = c(colnames(data0)[1],colnames(data0)[rv], "Predictive_Prob", feats_to_use)
  xgb_predictions = data.frame(prediction_results)
  
  pred_shapes = data.frame(cbind(pred_shapes[,1],format(round(rowMeans(pred_shapes[,-1]),4),scientific=F)))
  pred_shapes = pred_shapes[order(pred_shapes[,2],decreasing=T),]
  
  return(list(xgb_predictions,pred_shapes))
}