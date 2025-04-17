xgb_call_HP = function(current_data,
                        resvar,
                        id_var,
                        seed,
                        ignored_rows,
                        coves_to_use,
                        lc_lowval,
                        lc_upval,
                        rc_lowval,
                        rc_upval,
                        train_prop,
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
  data = cbind(data[,id_var],data[,resvar],std_covar_data)
  colnames(data)[1] = colnames(current_data)[id_var]
  colnames(data)[2] = colnames(current_data)[resvar]
  
  #Create n folds
  tot_folds = 5
  folds = cut(seq(1, nrow(data)), breaks = tot_folds, labels = FALSE)
  
  prediction_results = matrix(0, nrow = 0, ncol = length(coves_to_use) + 3)
  prediction_results = as.data.frame(prediction_results)
  
  
  hp_matrix = matrix(0, nrow = 0, ncol = 7)
  colnames(hp_matrix) = c(
    "max_depth",
    "eta",
    "subsample",
    "colsample_bytree",
    "min_child_weight",
    "gamma",
    "nrounds"
  )
  
  #Perform cross validation
  for (i in 1:tot_folds) {
    fold_num = i
    
    testIndexes = which(folds == i, arr.ind = TRUE)
    testData = data[testIndexes, ]
    trainData = data[-testIndexes, ]
    
    pso_result = xgb_HP_and_errors(
      trainData,
      testData,
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
      fold_num
    )
    
    prediction_results1 = cbind(data[testIndexes,],pso_result[[1]],covar_data[testIndexes,])
    prediction_results = rbind(prediction_results, prediction_results1)
    hp_matrix = rbind(hp_matrix, pso_result[[2]])
  }
  
  prediction_results[,3] = round(prediction_results[,3],3)
  prediction_results = prediction_results[order(prediction_results[,1]),]
  colnames(prediction_results) = c(colnames(current_data)[1],"Observation", "Prediction", coves_to_use)
  xgb_predictions = data.frame(prediction_results)
  
  print(xgb_predictions)
  
  # Cluster the HP matrix to find a single HP solution
  scaled_hp_matrix = as.data.frame(hp_matrix)
  
  # Only scale columns in HP matrix with more than 1 unique value
  for (h in 1:ncol(scaled_hp_matrix)) {
    if (length(unique(scaled_hp_matrix[,h])) > 1) {
      scaled_hp_matrix[,h] = scale(scaled_hp_matrix[,h])
    } else {
      scaled_hp_matrix[,h] = 0
    }
  }
  
  clust_data = scaled_hp_matrix
  clust_results = matrix(NA, nrow = (tot_folds-2), ncol = 4)
  
  km_models = list()
  
  for (j in 1:(tot_folds-2)) {
    
    km_model = kmeans(clust_data, centers = j, nstart = 10)
    
    clust_dens = rep(0,j)
    
    for (z in 1:j) {
      
      if (km_model$withinss[z] > 0) {
        Dens = (km_model$size[z]^member_exp)/(km_model$withinss[z]^ss_exp)
        clust_dens[z] = Dens
      } else {
        clust_dens[z] = 0
      }
    }
    
    Dm = which.max(clust_dens)
    
    if (j>1) {
      silhouette_scores = silhouette(km_model$cluster, dist(clust_data))
      mean_silhouette_score = mean(silhouette_scores[,3])
    } else {
      mean_silhouette_score = 0
    }
    
    clust_results[j,1] = j
    clust_results[j,2] = Dm
    clust_results[j,3] = clust_dens[Dm]
    clust_results[j,4] = mean_silhouette_score
    km_models[[j]]=km_model
  }
  
  best_sil = which.max(clust_results[,4])
  best_density = clust_results[best_sil,3]
  best_model = km_models[[best_sil]]
  best_list = which(best_model$cluster == clust_results[best_sil,2])
  best_centroid_members = hp_matrix[best_list,]
  best_centroid = colMeans(best_centroid_members)
  
  print(best_centroid)
  
  return(list(xgb_predictions,best_centroid))
}