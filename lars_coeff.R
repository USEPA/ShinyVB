library(lars)

lars_coeff = function(current_data,response_var,coves_to_use,nd_val,tntc_val,tntc_multy,MC_runs,loggy,randomize,lars_tech,standardize,max_steps) {
  
  cove_data=current_data[,coves_to_use]
  ncoves = ncol(cove_data)
  cove_names = colnames(cove_data)
  data = cbind(current_data[,response_var],cove_data)
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data=data[!is.na(data[,1]),]
  
  coeff_data=matrix(0, nrow = ncoves, ncol=MC_runs+1)
  coeff_data[,1] = cove_names

  for (a in 1:MC_runs) {
    
    # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
    if (loggy==TRUE) {
      
      for (j in 1:nrow(data)){
        if (data[j,1]=="TNTC") {
          data[j,1]=log10(runif(1, min = tntc_val, max = tntc_mult*tntc_val))
        }
        
        if (data[j,1]=="ND") {
          data[j,1]=log10(runif(1, min = 0, max = nd_val))
        }
      }
    } else {
    
      for (j in 1:nrow(data)){
        if (data[j,1]=="TNTC") {
          data[j,1]=(runif(1, min = tntc_val, max = tntc_mult*tntc_val))
        }
        
        if (data[j,1]=="ND") {
          data[j,1]=(runif(1, min = 0, max = nd_val))
        }
      }
    }
    
    # RANDOMIZE DATA
    if (randomize==TRUE) {
      random_index = sample(1:nrow(data), nrow(data))
      data = data[random_index, ]
    }

    # USE lars MODEL TO OBTAIN COVARIATE COEFFICIENTS for model that minimizes Mallows Cp
    model = lars(as.matrix(data[,-1]), data[,1], type = lars_tech, normalize=standardize, max.steps=max_steps)
    
    coeff=summary(model)
    
    opt_step=which.min(coeff$Cp+(coeff$Df)^0.00001)
    
    coeff_vector = as.data.frame(coef(model, s=opt_step))
    
    coeff_data[,a+1] = coeff_vector[,1]
  }
  
  coeff_data
}