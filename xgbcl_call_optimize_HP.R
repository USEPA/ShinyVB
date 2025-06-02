xgbcl_call_optimize_HP = function(current_data,
                       rv,
                       iv,
                       seed,
                       ignored_rows,
                       feats_to_use,
                       lc_val,
                       rc_val,
                       lc_lowval,
                       lc_upval,
                       rc_lowval,
                       rc_upval,
                       MC_runs,
                       nfolds,
                       loggy,
                       randomize,
                       standardize,
                       xgb_hyper_metric,
                       max_iter,
                       swarm_size,
                       member_exp,
                       ss_exp,
                       binarize,
                       crit_value) {
  
  set.seed(seed)
  
  data = current_data
  
  if (is.null(ignored_rows)) {
    data = data
  } else {
    data = data[-ignored_rows,]
  }
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data = data[!is.na(data[,rv]), ]
  
  #Randomly shuffle the data
  if (randomize) {
    data = data[sample(nrow(data)), ]
  }
  
  feat_data = data[,feats_to_use]
  
  std_feat_data = feat_data
  
  # Min/Max Standardize the features
  if (standardize) {
    for (i in 1:nrow(std_feat_data)) {
      for (j in 1:ncol(std_feat_data)) {
        if (is.numeric(std_feat_data[i, j])) {
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
  dataset = cbind(data[,iv],data[,rv],std_feat_data)
  colnames(dataset)[1] = colnames(current_data)[iv]
  colnames(dataset)[2] = colnames(current_data)[rv]
  
  pso_result = xgbcl_pso(
    dataset[,-1],
    rv,
    feats_to_use,
    lc_val,
    rc_val,
    lc_lowval,
    lc_upval,
    rc_lowval,
    rc_upval,
    MC_runs,
    nfolds,
    loggy,
    xgb_hyper_metric,
    max_iter,
    swarm_size,
    member_exp,
    ss_exp,
    binarize,
    crit_value)
  
  return(pso_result)
}