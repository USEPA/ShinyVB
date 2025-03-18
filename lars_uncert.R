library(lars)

lars_uncert = function(current_data,response_var,id_var,samp_prop,coves_to_use,nd_val,tntc_val,tntc_multy,MC_runs,loggy,randomize,lars_tech,standardize,max_steps) {
  
  cove_data=current_data[,coves_to_use]
  ncoves = ncol(cove_data)
  data = cbind(current_data[,id_var],current_data[,response_var],cove_data)
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data=data[!is.na(data[,2]),]
  
  models=c()
  
  smp_size = floor(samp_prop * nrow(data))
  
  fits_data = matrix(0, nrow = smp_size, ncol = 3*MC_runs)
  predicts_data = matrix(0, nrow = (nrow(data)-smp_size), ncol = 3*MC_runs)
  
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
    
    all = data
    
    # Split Dataset into Training and Testing
    train_ind = sample(seq_len(nrow(all)), size = smp_size)
    train_data = all[train_ind, ]
    test-data = all[-train_ind, ]
    
    # USE lars MODEL TO OBTAIN COVARIATE COEFFICIENTS for model that minimizes Mallows Cp
    model = lars(as.matrix(train[,-c(1:2)]), train[,2], type = lars_tech, normalize=standardize, max.steps=max_steps)
    model_summary=summary(model)
    opt_step=which.min(model_summary$Cp)
    
    models[[a]]=model

    fit1_values = rep(NA,1)
    fits1 = predict(model, as.matrix(train[,-c(1:2)]), s=opt_step)
    fits1 = as.data.frame(fits1[4])
    fit1_values= unlist(append(fit1_values,fits1))
    
    fits_data[,3*a-2]=train[,1]
    fits_data[,3*a-1]=train[,2]
    fits_data[,3*a]=fit_values[-1]
    
    fit2_values = rep(NA,1)
    fits2 = predict(model, as.matrix(test[,-c(1:2)]), s=opt_step)
    fits2 = as.data.frame(fits2[4])
    fit2_values= unlist(append(fit2_values,fits2))
    
    predicts_data[,3*a-2]=test_data[,1]
    predicts_data[,3*a-1]=test_data[,2]
    predicts_data[,3*a]=fit2_values[-1]
  }
  
  c(fits_data,predicts_data)
}