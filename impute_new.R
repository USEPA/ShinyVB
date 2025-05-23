imputing = function(trainData,testData,resvar) {
  
  rv = resvar - 1
  
  train_X = trainData[,-rv]
  test_X = testData[,-rv]
  
  imp_train_X=missForest(train_X)$ximp
    
  train_test_X = rbind(test_X, imp_train_X)
  imp_test_X = missForest(train_test_X)$ximp[1:nrow(test_X), ]
  
  # Add the new column to the end of the data frame
  imp_train_X$new_col = trainData[,rv]
  imp_test_X$new_col = trainData[,rv]
  
  train_data = imp_train_X[, c(1:(rv - 1), ncol(imp_train_X), rv:(ncol(imp_train_X) - 1))]
  test_data = imp_test_X[, c(1:(rv - 1), ncol(imp_test_X), rv:(ncol(imp_test_X) - 1))]

  colnames(train_data) = colnames(trainData)
  colnames(test_data) = colnames(testData)
  return(list(train_data, test_data))
}