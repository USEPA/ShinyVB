xgb_HP_and_errors = function(train_data,
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
                             max_iter,
                             swarm_size,
                             member_exp,
                             ss_exp,
                             tot_folds,
                             fold_num,
                             HP_choice,eta,gamma,max_depth,min_child_weight,subsamp,colsamp,nrounds) {
  
  cove_train_data = train_data[,coves_to_use]
  cove_test_data = test_data[,coves_to_use]
  
  cove_names = colnames(cove_train_data)
  pso_data = as.data.frame(cbind(train_data[,resvar],cove_train_data))
  
  test_data = as.data.frame(cbind(test_data[,resvar],cove_test_data))
  
  if (HP_choice) {
    
    pso_result = xgb_pso(
      pso_data,
      resvar,
      coves_to_use,
      lc_lowval,
      lc_upval,
      rc_lowval,
      rc_upval,
      MC_runs,
      loggy,
      xgb_hyper_metric,
      max_iter,
      swarm_size,
      member_exp,
      ss_exp,
      tot_folds,
      fold_num)
    
    params = list(
      eta = pso_result[[2]][1],
      gamma = pso_result[[6]][1],
      max_depth = round(pso_result[[1]][1],0),
      min_child_weight = round(pso_result[[5]][1],0),
      subsample = pso_result[[3]][1],
      colsample_bytree = pso_result[[4]][1]
    )
    
    nrnds = round(pso_result[[7]][1],0)
    
  } else {
    
    params = list(
      eta = eta,
      gamma = gamma,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsamp,
      colsample_bytree = colsamp
    )
    
    nrnds = nrounds
  }
  
  #use the passed parameters to make predictions on test data
  model = xgboost(data = as.matrix(pso_data[,-1]),label=pso_data[,1], params=params, early_stopping_rounds=10, nrounds=nrnds, verbose=0)
  
  predictions = predict(model, as.matrix(test_data[,-1]))
  pred_results = cbind(test_data[,1],predictions)
  
  if (HP_choice) {
    HP_values = c(
      round(pso_result[[1]][1],0),
      pso_result[[2]][1],
      pso_result[[3]][1],
      pso_result[[4]][1],
      round(pso_result[[5]][1],0),
      pso_result[[6]][1],
      round(pso_result[[7]][1],0))
  } else {
    HP_values = c(
      max_depth,
      eta,
      subsamp,
      colsamp,
      min_child_weight,
      gamma,
      nrounds
    )
  }
  
  return(list(pred_results,HP_values))
}