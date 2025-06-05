xgbcl_pso = function(pso_data,
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
                   crit_value) {
  
  MC_subbin = function(data,loggy,lc_val,lc_lowval,lc_upval,rc_val,rc_lowval,rc_upval) {
    
    if (loggy) {
      for (j in 1:nrow(data)) {
        if (data[j, 1] == lc_val) {
          data[j, 1] = log10(runif(1, min = lc_lowval, max = lc_upval))
        }
        if (data[j, 1] == rc_val) {
          data[j, 1] = log10(runif(1, min = rc_lowval, max = rc_upval))
        }
      }
    } else {
      for (j in 1:nrow(data)) {
        if (data[j, 1] == lc_val) {
          data[j, 1] = (runif(1, min = lc_lowval, max = lc_upval))
        }
        if (data[j, 1] == rc_val) {
          data[j, 1] = (runif(1, min = rc_lowval, max = rc_upval))
        }
      }
    }
    data
  }
  
  pso_results = matrix(NA, nrow = MC_runs, ncol = 7)

  withProgress(
    message = 'HP Tuning Progress',
    detail = paste("MC runs:", x = 1,"/",MC_runs),
    value = 0,
    {

      for (i in 1:MC_runs) {
        
        MC_data = MC_subbin(pso_data,loggy,lc_val,lc_lowval,lc_upval,rc_val,rc_lowval,rc_upval)
        
        
        if (binarize) {
          for (j in 1:nrow(MC_data)) {
            MC_data[j, 1] = ifelse(test = MC_data[j, 1] >= crit_value, yes = 1, no = 0)
          }
        }

        # Prepare data matrices for XGBoost
        X_train = as.matrix(MC_data[,-1])
        y_train = MC_data[,1]

        data = xgb.DMatrix(data = X_train, label = y_train)

        num_cols = ncol(MC_data)-1
        num_rows = nrow(MC_data)

        # Cross-validation function to evaluate XGBoost performance with given parameters
        xgb_cv_score = function(params) {
          
          # Extract parameters
          max_depth = round(params[1],0)
          eta = params[2]
          subsample = params[3]
          colsample_bytree = params[4]
          min_child_weight = round(params[5],0)
          gamma = params[6]
          nrounds = round(params[7],0)

          # Set up XGBoost parameters
          xgb_params = list(
            objective = "binary:logistic",
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
            data = data,
            nfold = nfolds,
            nrounds = nrounds,
            early_stopping_rounds = 10,
            verbose = 0
          )
          
          if (xgb_hyper_metric == "logloss") {
            best_score = min(cv_results$evaluation_log$test_logloss_mean)
          } else if (xgb_hyper_metric == "auc") {
            best_score = max(cv_results$evaluation_log$test_auc_mean)
          }
        }

        # Define parameter bounds for PSO
        param_bounds = matrix(
          c(2, #lower max_depth
            5, #upper max_depth
            0.01, #lower eta
            0.3, #upper eta
            0.5, #lower subsample proportion
            0.9, #upper subsample proportion
            0.5, #lower colsamp_bytree
            0.9, #upper colsamp_bytree
            2, #lower min_child_weight
            10, #upper min_child_weight
            0.25, #Lower gamma
            5, #upper gamma
            floor(0.33*num_rows), #lower nrounds
            3*num_rows), #upper nrounds
          ncol = 2,
          byrow = TRUE
        )

        # Run PSO to find optimal parameters
        pso_result = psoptim(
          rep(NA, 7),# Initial parameter values (NA for random)
          xgb_cv_score,# Function to minimize
          lower = c(param_bounds[1,1],param_bounds[2,1],param_bounds[3,1],param_bounds[4,1],param_bounds[5,1],param_bounds[6,1],param_bounds[7,1]),
          upper = c(param_bounds[1,2],param_bounds[2,2],param_bounds[3,2],param_bounds[4,2],param_bounds[5,2],param_bounds[6,2],param_bounds[7,2]),
          control = list(
            maxit = max_iter,
            s = swarm_size,
            trace = 1,
            vectorize=T,
            maxit.stagnate = 10,
            trace.stats = TRUE)
        )

        # Extract best parameters
        best_params = pso_result$par
        
        column_names =  c(
          "max_depth",
          "eta",
          "subsample",
          "colsample_bytree",
          "min_child_weight",
          "gamma",
          "nrounds"
        )
        
        colnames(pso_results) = column_names
        
        pso_results[i,] = c(round(best_params[1],0),
                            best_params[2],
                            best_params[3],
                            best_params[4],
                            round(best_params[5],0),
                            best_params[6],
                            round(best_params[7],0))
        
        incProgress(1/MC_runs, detail = paste("MC run:",i,"/",MC_runs))
      }
    })
  
  if (MC_runs < 3) {
    
    best_centroid = colMeans(as.data.frame(pso_results))
    
  } else {
    
    pso_results_scale = as.data.frame(pso_results)
    
    # Only scale columns with more than 1 unique value
    for (h in 1:ncol(pso_results_scale)) {
      if (length(unique(pso_results_scale[,h])) > 1) {
        pso_results_scale[,h] = scale(pso_results_scale[,h])
      } else {
        pso_results_scale[,h] = 0
      }
    }
    
    clust_data = as.matrix(pso_results_scale)
    clust_results = matrix(NA, nrow = MC_runs - 2, ncol = 4)
    
    km_models = list()
    
    for (j in 1:(MC_runs - 2)) {
      
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
    best_centroid_members = pso_results[best_list,]
    best_centroid = colMeans(best_centroid_members)
  }
  
  return(best_centroid)
}