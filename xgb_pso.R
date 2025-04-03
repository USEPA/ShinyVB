library(xgboost)
library(pso)
library(caret)
library(dplyr)
library(ggplot2)
library(stats)

xgb_pso = function(pso_data,
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
                   swarm_size) {
  
  permutes = MC_runs - 1
  
  pso_results = matrix(NA, nrow = MC_runs, ncol = 7)
  clust_results = rep(NA,permutes)
  
  for (i in 1:MC_runs) {

    # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
    if (loggy == TRUE) {
      for (j in 1:nrow(pso_data)) {
        if (pso_data[j, 1] == "TNTC") {
          pso_data[j, 1] = log10(runif(1, min = rc_lowval, max = rc_upval))
        }
        
        if (pso_data[j, 1] == "ND") {
          pso_data[j, 1] = log10(runif(1, min = lc_lowval, max = lc_upval))
        }
      }
    } else {
      for (j in 1:nrow(pso_data)) {
        if (pso_data[j, 1] == "TNTC") {
          pso_data[j, 1] = (runif(1, min = rc_lowval, max = rc_upval))
        }
        
        if (pso_data[j, 1] == "ND") {
          pso_data[j, 1] = (runif(1, min = lc_lowval, max = lc_upval))
        }
      }
    }
    
    # Split data
    train_idx = createDataPartition(pso_data[, 1], p = train_prop, list = FALSE)
    train_data = pso_data[train_idx, ]
    test_data = pso_data[-train_idx, ]
    
    # Prepare data matrices for XGBoost
    X_train = as.matrix(train_data[, -1])
    y_train = train_data[, 1]
    X_test = as.matrix(test_data[, -1])
    y_test = test_data[, 1]
    
    dtrain = xgb.DMatrix(data = X_train, label = y_train)
    dtest = xgb.DMatrix(data = X_test, label = y_test)
    
    # Cross-validation function to evaluate XGBoost performance with given parameters
    xgb_cv_score = function(params) {
      
      # Extract parameters
      max_depth = round(params[1])
      eta = params[2]
      subsample = params[3]
      colsample_bytree = params[4]
      min_child_weight = round(params[5])
      gamma = params[6]
      nrounds = params[7]
      
      # Set up XGBoost parameters
      xgb_params = list(
        objective = "reg:linear",
        eval_metric = xgb_hyper_metric,
        max_depth = max_depth,
        eta = eta,
        subsample = subsample,
        colsample_bytree = colsample_bytree,
        min_child_weight = min_child_weight,
        gamma = gamma,
        nrounds = nrounds
      )
      
      # Run 5-fold cross-validation
      cv_results = xgb.cv(
        params = xgb_params,
        data = dtrain,
        nfold = 5,
        early_stopping_rounds = 10,
        verbose = 0
      )
      
      # Return RMSE (for minimization)
      best_score = min(cv_results$evaluation_log$test_rmse_mean)
      return(best_score)
    }
      
      # Define parameter bounds for PSO
      param_bounds = matrix(
        c(2, #lower max_depth
          floor(0.33*num_cols), #upper max_depth
          0.01, #lower eta
          0.3, #upper eta
          0.4, #lower subsample proportion
          0.9, #upper subsample proportion
          0.4, #lower colsamp_bytree
          0.9, #upper colsamp_bytree
          1, #lower min_child_weight
          floor(0.33*num_rows), #upper min_child_weight
          0.3, #Lower gamma
          0.8, #upper hamma
          200, #lower nrounds
          1000, #upper nrounds)
          ncol = 2,
          byrow = TRUE
        ))
        
        # Run PSO to find optimal parameters
        pso_result = psoptim(
          rep(NA, nrow(param_bounds)),# Initial values (NA for random)
          xgb_cv_score,# Function to minimize
          lower = param_bounds[,1],
          upper = param_bounds[,2],
          
          control = list(
            maxit = max_iter,
            # Maximum iterations
            s = swarm_size,
            # Swarm size
            trace = 1,
            # Trace progress
            trace.stats = TRUE
          )
        )
        
        # Extract best parameters
        best_params = pso_result$par
        param_names = c(
          "max_depth",
          "eta",
          "subsample",
          "colsample_bytree",
          "min_child_weight",
          "gamma",
          "nrounds"
        )
        colnames(pso_results) = param_names
        pso_results[i,] = best_params
    
    #pso_result$value
  }

  clust_data = data.frame(scale(pso_results))
  
  km_models = vector(mode = "list", length = permutes)

  for (j in 1:permutes) {

    km_model = kmeans(clust_data, centers = j, nstart = 10)
    
    clust_dens = rep(0,j)
    
    for (z in 1:j) {
      Dens = km_model$withinss[z]/km_model$size[z]
      clust_dens[z] = Dens
    }
    
    Dm = which(max(clust_dens))
    
    clust_results[z,1] = k
    clust_results[z,2] = Dm
    clust_results[z,3] = km_model$silinfo$avg.width
    km_models[j]=km_model
  }
  
  best = which.max(clust_results[,3])
  best_centroid_members = pso_results[which(km_models[best]$cluster==clust_results[best,2]),]
  best_centroid = data.frame(col_means(best_centroid_members))
  colnames(best_centroid) = c("max_depth", "eta", "subsample", "colsample_bytree", "min_child_weight", "gamma", "nrounds")
  
  return(best_centroid)
}