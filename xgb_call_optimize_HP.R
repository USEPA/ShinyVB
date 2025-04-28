xgb_call_optimize_HP = function(current_data,
                       resvar,
                       id_var,
                       seed,
                       ignored_rows,
                       coves_to_use,
                       lc_lowval,
                       lc_upval,
                       rc_lowval,
                       rc_upval,
                       MC_runs,
                       loggy,
                       randomize,
                       standardize,
                       xgb_hyper_metric,
                       max_iter,
                       swarm_size,
                       member_exp,
                       ss_exp) {
  
  set.seed(seed)
  
  if (is.null(ignored_rows)) {
    data = current_data
  } else {
    data = current_data[-ignored_rows,]
  }
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data = data[!is.na(data[, resvar]), ]
  
  #Randomly shuffle the data
  if (randomize == TRUE) {
    data = data[sample(nrow(data)), ]
  }
  
  covar_data = data[,coves_to_use]
  
  std_covar_data = covar_data
  
  # Min/Max Standardize the features
  if (standardize == TRUE) {
    for (i in 1:nrow(std_covar_data)) {
      for (j in 1:ncol(std_covar_data)) {
        if (is.numeric(std_covar_data[i, j]) == TRUE) {
          if (max(na.omit(std_covar_data[, j])) - min(na.omit(std_covar_data[, j])) == 0) {
            std_covar_data[i, j] = 0
          } else {
            std_covar_data[i, j] = (std_covar_data[i, j] - min(na.omit(std_covar_data[, j]))) / (max(na.omit(std_covar_data[, j])) - min(na.omit(std_covar_data[, j])))
          }
        }
      }
    }
  }
  
  # Add the standardized features back into the data frame for analysis
  dataset = cbind(data[,id_var],data[,resvar],std_covar_data)
  colnames(dataset)[1] = colnames(current_data)[id_var]
  colnames(dataset)[2] = colnames(current_data)[resvar]
  
  pso_result = xgb_pso(
    dataset[,-1],
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
    ss_exp)
  
  return(pso_result)
}