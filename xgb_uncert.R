library(rsample)
library(xgboost)
library(dplyr)
library(permimp)

xgb_uncert = function(current_data,response_var,id_var,coves_to_use,nd_val,tntc_val,tntc_multy,MC_runs,
                      loggy,randomize,xgb_tech,drop_rate,skip_rate,eta,gamma,max_depth,min_child_weight,subsamp,
                      colsamp,samp_prop,nrounds,normalize_type,sample_type) {
  
  cove_data=current_data[,coves_to_use]
  ncoves = ncol(cove_data)
  cove_names = colnames(cove_data)
  data = cbind(current_data[,id_var],current_data[,response_var],cove_data)
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data=data[!is.na(data[,2]),]
  
  models=c()
  
  smp_size = floor(samp_prop * nrow(data))
  
  fits_data = matrix(0, nrow = smp_size, ncol = 3*MC_runs)
  predicts_data = matrix(0, nrow = (nrow(data)-smp_size), ncol = 3*MC_runs)
  
  if (xgb_tech == "dart") {
    params =
      list(
        booster = xgb_tech,
        rate_drop = drop_rate,
        skip_drop = skip_drop,
        normalize_type = normalize_type,
        sample_type = sample_type,
        
        tree_method = tree_method,
        eta = eta,
        gamma = gamma,
        max_depth = max_depth,
        min_child_weight = min_child_weight,
        subsample = subsamp,
        colsample_bytree = colsamp)
  } else {
    params =
      list(
        booster = xgb_tech,
        
        tree_method = tree_method,
        eta = eta,
        gamma = gamma,
        max_depth = max_depth,
        min_child_weight = min_child_weight,
        subsample = subsamp,
        colsample_bytree = colsamp)
  }
  
  for (a in 1:MC_runs) {
    
    # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
    if (loggy==TRUE) {
      
      for (j in 1:nrow(data)){
        if (data[j,2]=="TNTC") {
          data[j,2]=log10(runif(1, min = tntc_val, max = tntc_mult*tntc_val))
        }
        
        if (data[j,2]=="ND") {
          data[j,2]=log10(runif(1, min = 0, max = nd_val))
        }
      }
    } else {
      
      for (j in 1:nrow(data)){
        if (data[j,2]=="TNTC") {
          data[j,2]=(runif(1, min = tntc_val, max = tntc_mult*tntc_val))
        }
        
        if (data[j,2]=="ND") {
          data[j,2]=(runif(1, min = 0, max = nd_val))
        }
      }
    }
    
    # RANDOMIZE DATA
    if (randomize==TRUE) {
      random_index = sample(1:nrow(data), nrow(data))
      data = data[random_index, ]
    }
    
    # Split Dataset into Training and Testing
    train_ind = sample(seq_len(nrow(data)), size = smp_size)
    train_data = data[train_ind, ]
    test-data = data[-train_ind, ]
    
    model = xgboost(data = as.matrix(train_data[,-c(1:2)]), label=train_data[,2], params=params, nrounds = nrounds, verbose=0)
    
    models[[a]]=model
    
    fit1_values = rep(NA,1)
    fits1 = predict(model,as.matrix(train[,-c(1:2)]))
    fit1_values = append(fit1_values,fits1)
    
    fits_data[,3*a-2]=train[,1]
    fits_data[,3*a-1]=train[,2]
    fits_data[,3*a]=fit1_values[-1]
    
    fit2_values = rep(NA,1)
    
    if (xgb_tech == "dart") {
      fits2 = predict(model,as.matrix(test[,-c(1:2)]))
    } else {
      fits2 = predict(model,as.matrix(test[,-c(1:2)]))
    }
    
    fits2 = predict(model,as.matrix(test[,-c(1:2)]))
    fit2_values = append(fit2_values,fits2)
    
    predicts_data[,3*a-2]=test[,1]
    predicts_data[,3*a-1]=test[,2]
    predicts_data[,3*a]=fit2_values[-1]
  }
  
  c(models,fits_data,predicts_data)
}