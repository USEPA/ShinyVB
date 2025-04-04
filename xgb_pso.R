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
  
  pso_results = matrix(NA, nrow = MC_runs, ncol = 7)
  
  withProgress(
    message = 'Calculation in progress',
    detail = paste0("Runs remaining:", i = MC_runs),
    value = 0,
    {
      
      for (i in 1:MC_runs) {
        
        incProgress(1/MC_runs, detail = paste("Runs remaining:", i = (MC_runs - i)))
        
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
        
        num_cols = ncol(train_data)
        num_rows = nrow(train_data)
        
        # Cross-validation function to evaluate XGBoost performance with given parameters
        xgb_cv_score = function(params) {
          
          # Extract parameters
          max_depth = as.integer(round(params[1],0))
          eta = params[2]
          subsample = params[3]
          colsample_bytree = params[4]
          min_child_weight = as.integer(round(params[5],0))
          gamma = params[6]
          nrounds = as.integer(round(params[7],0))
          
          # Set up XGBoost parameters
          xgb_params = list(
            objective = "reg:squarederror",
            eval_metric = xgb_hyper_metric,
            max_depth = max_depth,
            eta = eta,
            subsample = subsample,
            colsample_bytree = colsample_bytree,
            min_child_weight = min_child_weight,
            gamma = gamma
          )
          
          # Run 5-fold cross-validation
          cv_results = xgb.cv(
            params = xgb_params,
            data = dtrain,
            nfold = 5,
            nrounds = nrounds,
            early_stopping_rounds = 5,
            verbose = 0
          )
          
          # Return RMSE (for minimization)
          best_score = min(cv_results$evaluation_log$test_rmse_mean)
          return(best_score)
        }
        
        # Define parameter bounds for PSO
        param_bounds = matrix(
          c(as.integer(2), #lower max_depth
            as.integer(floor(0.33*num_cols)), #upper max_depth
            0.01, #lower eta
            0.3, #upper eta
            0.4, #lower subsample proportion
            0.9, #upper subsample proportion
            0.4, #lower colsamp_bytree
            0.9, #upper colsamp_bytree
            as.integer(2), #lower min_child_weight
            as.integer(floor(0.33*num_rows)), #upper min_child_weight
            0.05, #Lower gamma
            0.5, #upper hamma
            as.integer(25), #lower nrounds
            as.integer(50)), #upper nrounds
          ncol = 2,
          byrow = TRUE
        )
        
        # Run PSO to find optimal parameters
        pso_result = psoptim(
          rep(NA, nrow(param_bounds)),# Initial values (NA for random)
          xgb_cv_score,# Function to minimize
          lower = param_bounds[,1],
          upper = param_bounds[,2],
          control = list(
            maxit = max_iter,
            s = swarm_size,
            trace = 1,
            vectorize=T,
            maxit.stagnate = 7,
            trace.stats = TRUE
          )
        )
        
        # Extract best parameters
        best_params = pso_result$par
        print(best_params)
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
        pso_results[i,] = c(round(best_params[1],0),
                            best_params[2],
                            best_params[3],
                            best_params[4],
                            round(best_params[5],0),
                            best_params[6],
                            round(best_params[7],0))
        
        print(pso_results)
        
        #pso_result$value
      }
    })
  
  clust_data = data.frame(scale(pso_results))
  clust_results = matrix(NA, nrow = MC_runs - 2, ncol = 3)
  
  km_models = vector(mode = "list", length = MC_runs-2)
  
  for (j in 1:(MC_runs - 2)) {
    
    km_model = kmeans(clust_data, centers = j+1, nstart = 10)
    
    clust_dens = rep(0,j+1)
    
    for (z in 1:j+1) {
      Dens = km_model$withinss[z]/km_model$size[z]
      clust_dens[z] = Dens
    }
    
    Dm = which.max(clust_dens)
    
    silhouette_scores = silhouette(km_model$cluster, dist(clust_data))
    
    mean_silhouette_score = mean(silhouette_scores[,3])
    
    clust_results[j,1] = j+1
    clust_results[j,2] = Dm
    clust_results[j,3] = mean_silhouette_score
    km_models[j]=km_model
  }
  
  best = which.max(clust_results[,3])
  best_model = km_models[[best]]
  
  best_centroid_members = pso_results[which(best_model$clusters == clust_results[best,2]),]
  print(best_centroid_members)
  
  best_centroid = data.frame(colMeans(best_centroid_members))
  print(best_centroid)
  
  colnames(best_centroid) = c("max_depth", "eta", "subsample", "colsample_bytree", "min_child_weight", "gamma", "nrounds")
  
  return(best_centroid)
}