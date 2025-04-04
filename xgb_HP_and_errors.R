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
                   swarm_size) {
  
  cove_train_data = pso_train_data[, coves_to_use]
  cove_test_data = pso_test_data[, coves_to_use]
  
  num_rows = nrow(cove_train_data)
  num_cols = ncol(cove_train_data)
  
  ncoves = ncol(cove_train_data)
  cove_names = colnames(cove_train_data)
  pso_data = cbind(pso_train_data[, resvar], cove_train_data)
  
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
    swarm_size)
  
  print(pso_result)
  
  #use the best parameters from pso to make predictions on test data
  #save the errors for final predictive model
  
}