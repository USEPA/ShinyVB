library(xgboost)
library(pso)
library(caret)
library(dplyr)
library(ggplot2)

xgb_pso = function(xgb_pso_data,
                   resvar,
                   coves_to_use,
                   seed,
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
                   eta_list,
                   gamma_list,
                   max_depth_list,
                   min_child_weight_list,
                   subsamp_list,
                   colsamp_list,
                   nround,
                   nfolds,
                   early_stop,
                   max_iter,
                   swarm_size) {
  # Set seed for reproducibility
  set.seed(seed)
  
  cove_data = xgb_pso_data[, coves_to_use]
  
  if (xgb_standardize == TRUE) {
    for (i in 1:nrow(cove_data)) {
      for (j in 1:ncol(cove_data)) {
        if (is.numeric(cove_data[i, j]) == TRUE) {
          cove_data[i, j] = (cove_data[i, j] - min(na.omit(cove_data[, j]))) / (max(na.omit(cove_data[, j])) - min(na.omit(cove_data[, j])))
        }
      }
    }
  }
  
  ncoves = ncol(cove_data)
  cove_names = colnames(cove_data)
  data = cbind(xgb_pso_data[, resvar], cove_data)
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data = data[!is.na(data[, 1]), ]
  
  # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
  if (loggy == TRUE) {
    for (j in 1:nrow(data)) {
      if (data[j, 1] == "TNTC") {
        data[j, 1] = log10(runif(1, min = rc_lowval, max = rc_upval))
      }
      
      if (data[j, 1] == "ND") {
        data[j, 1] = log10(runif(1, min = lc_lowval, max = lc_upval))
      }
    }
  } else {
    for (j in 1:nrow(data)) {
      if (data[j, 1] == "TNTC") {
        data[j, 1] = (runif(1, min = rc_lowval, max = rc_upval))
      }
      
      if (data[j, 1] == "ND") {
        data[j, 1] = (runif(1, min = lc_lowval, max = lc_upval))
      }
    }
  }
  
  # RANDOMIZE DATA
  if (randomize == TRUE) {
    random_index = sample(1:nrow(data), nrow(data))
    data = data[random_index, ]
  }
  
  # Split data
  train_idx = createDataPartition(data[, 1], p = train_prop, list = FALSE)
  train_data = data[train_idx, ]
  test_data = data[-train_idx, ]
  
  # Prepare data matrices for XGBoost
  X_train = as.matrix(train_data[, -1])
  y_train = train_data[, 1]
  X_test = as.matrix(test_data[, -1])
  y_test = test_data[, 1]
  
  dtrain = xgb.DMatrix(data = X_train, label = y_train)
  dtest = xgb.DMatrix(data = X_test, label = y_test)
  
  # Cross-validation function to evaluate XGBoost performance with given parameters
  xgb_cv_score = function(params) {
    cv_folds = nfolds
    
    # Extract parameters
    max_depth = round(params[1])
    eta = params[2]
    subsample = params[3]
    colsample_bytree = params[4]
    min_child_weight = round(params[5])
    gamma = params[6]
    
    # Set up XGBoost parameters
    xgb_params <- list(
      objective = "reg:linear",
      eval_metric = xgb_hyper_metric,
      max_depth = max_depth,
      eta = eta,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      gamma = gamma
    )
    
    # Run cross-validation
    cv_results = xgb.cv(
      params = xgb_params,
      data = dtrain,
      nrounds = nround,
      nfold = cv_folds,
      early_stopping_rounds = early_stop,
      verbose = 0
    )
    
    # Return RMSE (for minimization)
    best_score = min(cv_results$evaluation_log$test_rmse_mean)
    return(best_score)  # Negative because PSO minimizes
  }
  
  # Define parameter bounds for PSO
  param_bounds = matrix(
    c(
      max_depth_list[1],
      max_depth_list[2],
      eta_list[1],
      eta_list[2],
      subsamp_list[1],
      subsamp_list[2],
      colsamp_list[1],
      colsamp_list[2],
      min_child_weight_list[1],
      min_child_weight_list[2],
      gamma_list[1],
      gamma_list[2]
    ),
    ncol = 2,
    byrow = TRUE
  )
  
  # Run PSO to find optimal parameters
  pso_result = psoptim(
    rep(NA, nrow(param_bounds)),
    # Initial values (NA for random)
    xgb_cv_score,
    # Function to minimize
    lower = param_bounds[, 1],
    # Lower bounds
    upper = param_bounds[, 2],
    # Upper bounds
    control = list(
      maxit = max_iter,
      # Maximum iterations
      s = swarm_size,
      # Swarm size
      trace = 1,
      # Trace progress
      trace.stats = TRUE             # Keep track of stats
    )
  )
  
  # Extract best parameters
  best_params = pso_result$par
  param_names = c("max_depth",
                  "eta",
                  "subsample",
                  "colsample_bytree",
                  "min_child_weight",
                  "gamma")
  names(best_params) = param_names
  
  # Print best parameters and score
  print("Best parameters found:")
  print(
    data.frame(
      max_depth = round(best_params["max_depth"]),
      eta = best_params["eta"],
      subsample = best_params["subsample"],
      colsample_bytree = best_params["colsample_bytree"],
      min_child_weight = round(best_params["min_child_weight"]),
      gamma = best_params["gamma"]
    )
  )
  print(paste("Best CV RMSE:", -pso_result$value))
  
  
}