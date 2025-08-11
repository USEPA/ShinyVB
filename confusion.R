confuse = function(data,reg_stand,dec_crit) {
  
  if (!is.numeric(dec_crit) || !is.numeric(reg_stand)) {
    
    showModal(modalDialog("The decision criterion and regulatory standard must be numeric",easyClose = TRUE,
      footer = tagList(modalButton("Close"))
    ))
    
    return(list(
      TP = 0,
      TN = 0,
      FP = 0,
      FN = 0,
      Sensitivity = 0,
      Specificity = 0,
      Accuracy = 0
    ))
    
  } else {
    
    FP = 0
    FN = 0
    TP = 0
    TN = 0
    
    for (i in 1:nrow(data)) {
      if (data[i,1] >= reg_stand && data[i,2] >= dec_crit) {
        TP = TP + 1
      } else if (data[i,1] >= reg_stand && data[i,2] < dec_crit) {
        FN = FN + 1
      } else if (data[i,1] < reg_stand && data[i,2] >= dec_crit) {
        FP = FP + 1
      } else if (data[i,1] < reg_stand && data[i,2] < dec_crit) {
        TN = TN + 1
      }
    }
    
    Sens = TP / (TP + FN)
    Spec = TN / (TN + FP)
    Acc = (TP + TN) / (TP + TN + FP + FN)
    
    return(list(
      TP = TP,
      TN = TN,
      FP = FP,
      FN = FN,
      Sensitivity = Sens,
      Specificity = Spec,
      Accuracy = Acc
    ))
  }
}