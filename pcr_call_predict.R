pcr_call_predict = function(data,
                       resvar,
                       id_var,
                       seed,
                       ignored_rows,
                       coves_to_use,
                       lc_lowval,
                       lc_upval,
                       rc_lowval,
                       rc_upval,
                       MC_runs,
                       nfolds,
                       loggy,
                       randomize,
                       pcr_prop) {
  
  set.seed(seed)
  
  if (is.null(ignored_rows)) {
    data = data
  } else {
    data = data[-ignored_rows,]
  }
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data = data[!is.na(data[,resvar]), ]
  
  #Randomly shuffle the data
  if (randomize == TRUE) {
    data = data[sample(nrow(data)), ]
  }
  
  # Isolate the features
  feat_data = data[,coves_to_use]
  
  # Run PCA on feature data
  pca_result = prcomp(feat_data, scale. = TRUE)
  pca_summary = summary(pca_result)
  
  cumulative_proportions = pca_summary$importance[3,]
  
  explained=0
  dataset = cbind(data[,1],data[,resvar])
  
  for (i in 1:length(cumulative_proportions)) {
    if (explained <= pcr_prop) {
      dataset = cbind(dataset, pca_result$x[,i])
      explained = cumulative_proportions[i]
    }
  }
  
  # Create n folds
  tot_folds = nfolds
  folds = cut(seq(1, nrow(dataset)), breaks = tot_folds, labels = FALSE)
  
  prediction_results = matrix(0, nrow = 0, ncol = ncol(dataset) + 1)
  prediction_results = data.frame(prediction_results)
  
  # Perform cross validation
  for (k in 1:tot_folds) {
    fold_num = k
    testIndices = which(folds == k, arr.ind = TRUE)
    test_data = dataset[testIndices, ]
    train_data = dataset[-testIndices, ]
    
    withProgress(
    message = 'PCR Prediction Progress',
    detail = paste("MC runs:", x = MC_runs,"; Fold: ",fold_num),
    value = (fold_num/tot_folds)-0.2,
    {
      
      temp_preds = matrix(0, nrow = nrow(test_data), ncol = 2*MC_runs)
      temp_preds = data.frame(temp_preds)
      
      for (i in 1:MC_runs) {

        # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
        if (loggy == TRUE) {
          for (j in 1:nrow(train_data)) {
            if (train_data[j, 2] == "TNTC") {
              train_data[j, 2] = log10(runif(1, min = rc_lowval, max = rc_upval))
            }
            
            if (train_data[j, 2] == "ND") {
              train_data[j, 2] = log10(runif(1, min = lc_lowval, max = lc_upval))
            }
          }
          for (j in 1:nrow(test_data)) {
            if (test_data[j, 2] == "TNTC") {
              test_data[j, 2] = log10(runif(1, min = rc_lowval, max = rc_upval))
            }
            
            if (test_data[j, 2] == "ND") {
              test_data[j, 2] = log10(runif(1, min = lc_lowval, max = lc_upval))
            }
          }
        } else {
          for (j in 1:nrow(train_data)) {
            if (train_data[j, 2] == "TNTC") {
              train_data[j, 2] = (runif(1, min = rc_lowval, max = rc_upval))
            }
            
            if (train_data[j, 2] == "ND") {
              train_data[j, 2] = (runif(1, min = lc_lowval, max = lc_upval))
            }
          }
          for (j in 1:nrow(test_data)) {
            if (test_data[j, 2] == "TNTC") {
              test_data[j, 2] = (runif(1, min = rc_lowval, max = rc_upval))
            }
            
            if (test_data[j, 2] == "ND") {
              test_data[j, 2] = (runif(1, min = lc_lowval, max = lc_upval))
            }
          }
        }
        
        temp_preds[,2*i-1] = test_data[,2]
        
        training= data.frame(train_data[,-1])
        colnames(training)[1] = "Response"
        
        testing=data.frame(test_data[,-1])

        #fit training data
        model = lm(Response~.,data=training,na.action=na.exclude)
        
        preds = predict(model, newdata = testing)
        temp_preds[,2*i] = round(preds,3)
        
        incProgress(1/(MC_runs*tot_folds), detail = paste("MC run: ",i,"/",MC_runs,"; Fold: ",fold_num,"/",tot_folds))
        
      } #End the MC runs
      
    })
  
  even_columns = temp_preds[,seq(2, ncol(temp_preds), by = 2)]
  odd_columns = temp_preds[,seq(1, ncol(temp_preds), by = 2)]
  
  obs_mean_values = rowMeans(odd_columns)
  pred_mean_values = rowMeans(even_columns)
  fold_preds = cbind(round(obs_mean_values,3),round(pred_mean_values,3))
    
  prediction_results_fold = cbind(test_data[,1],fold_preds,round(test_data[,3:ncol(test_data)],4))
  prediction_results = rbind(prediction_results, prediction_results_fold)
  
  }#End the folds

  prediction_results = prediction_results[order(prediction_results[,1]),]
  colnames(prediction_results)[1:3] = c(colnames(data)[1],colnames(data)[resvar], "Prediction")
  pcr_predictions = data.frame(prediction_results)
  
  for (t in 4:ncol(pcr_predictions)) {
    colnames(pcr_predictions)[t] = paste("PCA",t-3)
  }
  
  return(list(pcr_predictions,pca_result))
}