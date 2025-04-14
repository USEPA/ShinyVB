xgb_HP_and_errors = function(pso_train_data,
                   pso_test_data,
                   resvar,
                   coves_to_use,
                   lc_lowval,
                   lc_upval,
                   rc_lowval,
                   rc_upval,
                   train_prop,
                   MC_runs,
                   loggy,
                   randomize,
                   xgb_standardize,
                   xgb_hyper_metric,
                   max_iter,
                   swarm_size,
                   member_exp,
                   ss_exp,
                   fold_num) {
  
  cove_train_data = pso_train_data[, coves_to_use]
  cove_test_data = pso_test_data[, coves_to_use]
  
  num_rows = nrow(cove_train_data)
  num_cols = ncol(cove_train_data)
  
  ncoves = ncol(cove_train_data)
  cove_names = colnames(cove_train_data)
  pso_data = cbind(pso_train_data[, resvar], cove_train_data)
  
  test_data = cbind(pso_test_data[, resvar], cove_test_data)
  
  pso_result = xgb_pso(
    pso_data,
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
    max_iter,
    swarm_size,
    member_exp,
    ss_exp,
    fold_num)
  
  params = list(
    eta = pso_result[[2]][1],
    gamma = pso_result[[6]][1],
    max_depth = round(pso_result[[1]][1],0),
    min_child_weight = round(pso_result[[5]][1],0),
    subsample = pso_result[[3]][1],
    colsample_bytree = pso_result[[4]][1]
  )
  
  #use the best parameters from pso to make predictions on test data
  model = xgboost(data = as.matrix(pso_data[,-1]),label=pso_data[,1], params=params, early_stopping_rounds=10, nrounds=round(pso_result[[7]][1],0), verbose=0)
  
  predictions = predict(model, as.matrix(test_data[,-1]))
  pred_results = cbind(test_data[,1],predictions)
  
  HP_values = c(
    round(pso_result[[1]][1],0),
    pso_result[[2]][1],
    pso_result[[3]][1],
    pso_result[[4]][1],
    round(pso_result[[5]][1],0),
    pso_result[[6]][1],
    round(pso_result[[7]][1],0)
  )
  
  return(list(pred_results,HP_values))
}