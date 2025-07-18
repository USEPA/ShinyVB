get_model_params = function(fit) {
  alpha = fit$alpha
  lambdaMin = sapply(fit$modlist, `[[`, "lambda.min")
  lambdaSE = sapply(fit$modlist, `[[`, "lambda.1se")
  error = sapply(fit$modlist, function(mod) {min(mod$cvm)})
  best = which.min(error)
  data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
             lambdaSE = lambdaSE[best], eror = error[best])
}

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
  MC_data = data
}

MC_final_subbin = function(data,loggy,lc_val,rc_val,lowmult,highmult) {
  
  exclude_values = c(lc_val,rc_val)
  real_responses = na.omit(data[!data[,1] %in% exclude_values,1])
  
  if (loggy) {
    
    for (j in 1:nrow(data)){
      if (data[j,1]==rc_val) {
        data[j,1]=log10(lowmult*min(real_responses))
      }
      
      if (data[j,1]==lc_val) {
        data[j,1]=log10(highmult*max(real_responses))
      }
    }
  } else {
    
    for (j in 1:nrow(data)){
      if (data[j,1]==rc_val) {
        data[j,1]=lowmult*min(real_responses)
      }
      
      if (data[j,1]==lc_val) {
        data[j,1]=highmult*max(real_responses)
      }
    }
  }
  
  MC_data = data
}

create_data = function(data,rv,feats_to_use,ignored_rows,randomize,standardize) {
  
  if (is.null(ignored_rows)) {
    data = data
  } else {
    data = data[-ignored_rows,]
  }
  
  data = data[!is.na(data[,rv]),]
  
  var_list = c(1,rv,which(colnames(data) %in% feats_to_use))
  data1 = data[,var_list]
  colnames(data1) = c(colnames(data)[1],"Response",feats_to_use)
  
  if (randomize) {
    random_index = sample(1:nrow(data1), nrow(data1))
    data1 = data1[random_index, ]
  }
  
  if (standardize) {
    
    for (i in 1:nrow(data1)) {
      for (j in 3:ncol(data1)) {
        if (is.numeric(data1[i,j])) {
          
          range = (max(na.omit(data1[,j])) - min(na.omit(data1[,j])))
          
          if (range == 0) {
            data1[i,j] = 0
          } else if (range < 1) {
            data1[i,j]= round((1 + (data1[i,j] - min(na.omit(data1[,j])))) / (range+1),4)
          } else {
            data1[i,j]=round((data1[i,j] - min(na.omit(data1[,j]))) / range,4)
          }
        }
      }
    }
  }
  created_data = data1
}

custom_xgb_loss = function(preds, dtrain, rw) {
  labels = getinfo(dtrain, "label")
  
  # Calculate residuals
  residuals = preds - labels
  
  # Calculate RSS
  rss = sum(residuals^2)
  
  # Calculate range penalty
  observed_range = max(labels) - min(labels)
  predicted_range = max(preds) - min(preds)
  range_penalty = (observed_range - predicted_range)^2
  
  # Weighting factor for range penalty
  range_weight = rw
  
  # Combine RSS and range penalty
  loss = rss + range_weight * range_penalty
  
  # Gradient and Hessian (derivatives)
  grad = 2 * residuals + 2 * range_weight * (predicted_range - observed_range)
  hess = rep(2, length(preds))
  
  return(list(grad = grad, hess = hess))
}

magnitude_round = function(x) {
  ifelse(abs(x) < 0.001, round(x, 6),
         ifelse(abs(x) < 0.01, round(x, 5),
                ifelse(abs(x) < 0.1, round(x, 4),
                       ifelse(abs(x) < 1, round(x, 3),
                              ifelse(abs(x) < 10, round(x, 2),
                                     round(x, 1)
                              )
                       )
                )
         )
  )
}