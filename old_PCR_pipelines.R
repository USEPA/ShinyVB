# PCR predictions on test data - NOT USING ANYMORE

observeEvent(input$PCR_pred_stand, {
  
  if (nrow(PCR_pred_scat_dat) != 0) {
    
    output$PCR_pred_scatplot = renderPlotly(scatter_confuse(PCR_pred_scat_dat,input$PCR_pred_stand,input$PCR_pred_dc))
    
    confuse_results = confuse(PCR_pred_scat_dat[,2:3],input$PCR_pred_stand,input$PCR_pred_dc)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$PCR_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                                                                                options = list(autoWidth = F,dom='tB', paging = F,buttons = c('copy', 'csv', 'excel'),scrollX = F,scrollY = F,
                                                                                               columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$PCR_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
  }
})

observeEvent(input$PCR_pred_dc, {
  
  if (nrow(PCR_pred_scat_dat) != 0) {
    
    output$PCR_pred_scatplot = renderPlotly(scatter_confuse(PCR_pred_scat_dat,input$PCR_pred_stand,input$PCR_pred_dc))
    
    confuse_results = confuse(PCR_pred_scat_dat[,2:3],input$PCR_pred_stand,input$PCR_pred_dc)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$PCR_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                                                                                options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                                                    orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$PCR_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
  }
})

observeEvent(input$run_PCR_predict, {
  
  req(iv$is_valid())
  
  data = current_data()
  
  if (any(is.na(data[,3:ncol(data)]))) {
    
    showModal(modalDialog(paste("PCA does not tolerate missing feature values. You can either Impute these
                (on the Data tab) or Disable rows/columns with missing values."),footer = modalButton("Close")))
    
  } else {
    
    pcr_pred_results = pcr_call_predict(current_data(),response_var(),id_var,input$model_seed,ignored_rows,input$coves_to_use,input$lc_lowval,
                                        input$lc_upval,input$rc_lowval,input$rc_upval,input$MC_runs,input$num_folds,input$loggy,input$randomize,input$pcr_prop)
    
    
    PCA_coefficients = data.frame(round(pcr_pred_results[[2]]$rotation,4))
    PCA_coefficients = cbind(Feature = rownames(PCA_coefficients), PCA_coefficients)
    
    PCA_summary = summary(pcr_pred_results[[2]])
    PCA_summary_df = data.frame(rbind(round(PCA_summary$importance[1,],3),PCA_summary$importance[2,],PCA_summary$importance[3,]))
    summ_rownames= c("Std. Dev.","Var Explained","Cum Var Explained")
    PCA_summary_df = cbind(summ_rownames,PCA_summary_df)
    colnames(PCA_summary_df)[1] = "Metric"
    
    output$PCA_coeffs = DT::renderDataTable(server = T, {data = datatable(PCA_coefficients,rownames = F,selection =
                                                                            list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                                                                            list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                                                                              className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                         "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$PCA_summary = DT::renderDataTable(server = T, {data = datatable(PCA_summary_df,rownames = F,selection =
                                                                             list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                                                                             list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                                                                               className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                          "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    PCR_saved_predictions <<- pcr_pred_results[[1]]
    
    pcr_pred_stepr = round((max(PCR_saved_predictions[,2])-min(PCR_saved_predictions[,2]))/40,2)
    
    updateNumericInput(session, "PCR_pred_stand",
                       value = round(mean(PCR_saved_predictions[,2]),2),
                       max = round(max(PCR_saved_predictions[,2]),2),
                       min = round(min(PCR_saved_predictions[,2]),2),
                       step = pcr_pred_stepr
    )
    
    pcr_pred_stepdc = round((max(PCR_saved_predictions[,3])-min(PCR_saved_predictions[,3]))/40,2)
    
    updateNumericInput(session, "PCR_pred_dc",
                       value = round(mean(PCR_saved_predictions[,3]),2),
                       max = round(max(PCR_saved_predictions[,3]),2),
                       min = round(min(PCR_saved_predictions[,3]),2),
                       step = pcr_pred_stepdc
    )
    
    output$PCR_predictions = DT::renderDataTable(server = T, {data = datatable(PCR_saved_predictions,rownames = F,selection =
                                                                                 list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions='Buttons',options = list(autoWidth = F,
                                                                                                                                                                                                                 paging = TRUE,pageLength = 17,dom="ltBp",buttons = c('copy', 'csv', 'excel'),scrollX = TRUE,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                                                                                                                                                                                   orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744',
              'color': '#fff'});","}")))
    })
    
    
    
    PCR_pred_scat_dat <<- PCR_saved_predictions[,1:3]
    output$PCR_pred_scatplot = renderPlotly(scatter_confuse(PCR_pred_scat_dat,input$PCR_pred_stand,input$PCR_pred_dc))
    
    confuse_results = confuse(PCR_pred_scat_dat[,2:3],input$PCR_pred_stand,input$PCR_pred_dc)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$PCR_pred_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                                  list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                                                                                options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                                                                                               columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$PCR_pred_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                      round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    
    resid_data = PCR_pred_scat_dat[,c(1,3)] %>% mutate(Residuals = round(PCR_pred_scat_dat[,2]-PCR_pred_scat_dat[,3],3))
    
    output$PCR_pred_resid_scatplot = renderPlotly(scatter(resid_data))
    
    output$PCR_pred_lineplot = renderPlotly(plot_ly(PCR_pred_scat_dat, x = ~PCR_pred_scat_dat[,1], y = ~PCR_pred_scat_dat[,2], name="Observations",
                                                    type="scatter", mode = "lines",text = ~paste("<b>ID: </b>",PCR_pred_scat_dat[,1],"<br><b>Observed Value:</b> ",
                                                                                                 PCR_pred_scat_dat[,2],sep=""),hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5)) %>%
                                              add_trace(y = ~PCR_pred_scat_dat[,3], name="Predictions", mode = 'lines',
                                                        text = ~paste("<b>ID: </b>",PCR_pred_scat_dat[,1],"<br><b>Predicted Value:</b> ",round(PCR_pred_scat_dat[,3],3),sep=""),
                                                        hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                                              layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Predictions",font=list(size=20)),
                                                                                                                           range=c(min(0.99*min(PCR_pred_scat_dat[,2],PCR_pred_scat_dat[,3]),1.01*min(PCR_pred_scat_dat[,2],PCR_pred_scat_dat[,3])),
                                                                                                                                   max(0.99*max(PCR_pred_scat_dat[,2],PCR_pred_scat_dat[,3]),1.01*max(PCR_pred_scat_dat[,2],PCR_pred_scat_dat[,3]))))))
    
    output$PCR_used_hp_pred = DT::renderDataTable(server=T,{data = datatable(Optimal_PCR,rownames=F,selection=list(selected = list(rows = NULL, cols = NULL),
                                                                                                                   target = "row",mode="single"),editable=F,extensions='Buttons', options = list(autoWidth=F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,
                                                                                                                                                                                                 pageLength = 5,scrollX = F,scrollY = F,columnDefs = list(list(className = 'dt-center',orderable=T,targets='_all')),
                                                                                                                                                                                                 initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    removeModal()
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'PCR: Predict')
    
  }
})

# PCR Fitting of entire data set - NOT USING ANYMORE

observeEvent(input$PCR_stand, {
  
  if (nrow(PCR_scat_dat) != 0) {
    
    output$PCR_scatplot = renderPlotly(scatter_confuse(PCR_scat_dat,input$PCR_stand,input$PCR_dec_crit))
    
    confuse_results = confuse(PCR_scat_dat[,2:3],input$PCR_stand,input$PCR_dec_crit)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$PCR_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                             list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                                                                           options = list(autoWidth = F,dom='tB', paging = F,buttons = c('copy', 'csv', 'excel'),scrollX = F,scrollY = F,
                                                                                          columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$PCR_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                 round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
  }
})

observeEvent(input$PCR_dec_crit, {
  
  if (nrow(PCR_scat_dat) != 0) {
    
    output$PCR_scatplot = renderPlotly(scatter_confuse(PCR_scat_dat,input$PCR_stand,input$PCR_dec_crit))
    
    confuse_results = confuse(PCR_scat_dat[,2:3],input$PCR_stand,input$PCR_dec_crit)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$PCR_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                             list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                                                                           options = list(autoWidth = F,dom='tB', paging = F,buttons = c('copy', 'csv', 'excel'),scrollX = F,scrollY = F,
                                                                                          columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$PCR_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                 round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
  }
})

observeEvent(input$PCR_final_fitting, {
  
  req(iv$is_valid())
  
  set.seed(input$model_seed)
  
  if (is.null(ignored_rows)) {
    data = current_data()
  } else {
    data = current_data()[-ignored_rows,]
  }
  
  # REMOVE NA'S FROM RESPONSE VARIABLE
  data = data[!is.na(data[,response_var()]), ]
  
  if (any(is.na(data[,3:ncol(data)]))) {
    
    showModal(modalDialog(paste("PCA does not tolerate missing feature values. You can either Impute these
                (on the Data tab) or Disable rows/columns with missing values."),footer = modalButton("Close")))
    
  } else {
    
    #Randomly shuffle the data
    if (input$randomize == TRUE) {
      data = data[sample(nrow(data)), ]
    }
    
    # Isolate the features
    feat_data = data[,input$coves_to_use]
    
    # Run PCA on feature data
    pca_result = prcomp(feat_data, scale. = TRUE)
    pca_summary = summary(pca_result)
    
    cumulative_proportions = pca_summary$importance[3,]
    
    explained=0
    dataset = cbind(data[,1],data[,response_var()])
    
    for (i in 1:length(cumulative_proportions)) {
      if (explained <= input$pcr_prop) {
        dataset = cbind(dataset, pca_result$x[,i])
        explained = cumulative_proportions[i]
      }
    }
    
    MC_runs = input$MC_runs
    
    withProgress(
      message = 'PCR Fitting Progress',
      detail = paste("MC runs:", x = MC_runs),
      value = 0,
      {
        
        temp_fits = matrix(0, nrow = nrow(dataset), ncol = 2*MC_runs)
        temp_fits = data.frame(temp_fits)
        
        for (i in 1:MC_runs) {
          
          # SUBSTITUTE random value FOR RESPONSE VARIABLE NON-DETECTS
          if (input$loggy == TRUE) {
            for (j in 1:nrow(dataset)) {
              if (dataset[j, 2] == "TNTC") {
                dataset[j, 2] = log10(runif(1, min = rc_lowval, max = rc_upval))
              }
              
              if (dataset[j, 2] == "ND") {
                dataset[j, 2] = log10(runif(1, min = lc_lowval, max = lc_upval))
              }
            }
          } else {
            for (j in 1:nrow(dataset)) {
              if (dataset[j, 2] == "TNTC") {
                dataset[j, 2] = (runif(1, min = rc_lowval, max = rc_upval))
              }
              
              if (dataset[j, 2] == "ND") {
                dataset[j, 2] = (runif(1, min = lc_lowval, max = lc_upval))
              }
            }
          }
          
          temp_fits[,2*i-1] = dataset[,2]
          
          training = data.frame(dataset[,-1])
          colnames(training)[1] = "Response"
          
          #fit training data
          model = lm(Response~.,data=training,na.action=na.exclude)
          
          fitvals = predict(model, newdata = training)
          temp_fits[,2*i] = round(fitvals,3)
          
          incProgress(1/MC_runs, detail = paste("MC run: ",i,"/",MC_runs))
          
        } #End the MC runs
        
      })
    
    even_columns = temp_fits[,seq(2, ncol(temp_fits), by = 2)]
    odd_columns = temp_fits[,seq(1, ncol(temp_fits), by = 2)]
    
    obs_mean_values = rowMeans(odd_columns)
    fit_mean_values = rowMeans(even_columns)
    fits = cbind(round(obs_mean_values,3),round(fit_mean_values,3))
    
    fitted_results = data.frame(cbind(dataset[,1],fits,round(dataset[,3:ncol(dataset)],4)))
    fitted_results = fitted_results[order(fitted_results[,1]),]
    
    pcr_fits = data.frame(fitted_results)
    
    colnames(pcr_fits)[1:3] = c(colnames(data)[1],colnames(data)[response_var()],"Fitted_Value")
    
    for (t in 4:ncol(pcr_fits)) {
      colnames(pcr_fits)[t] = paste("PCA",t-3)
    }
    
    PCA_coefficients = data.frame(round(pca_result$rotation,4))
    PCA_coefficients = cbind(Feature = rownames(PCA_coefficients), PCA_coefficients)
    
    PCA_summary_df = data.frame(rbind(round(pca_summary$importance[1,],3),pca_summary$importance[2,],pca_summary$importance[3,]))
    summary_rownames= c("Std. Dev.","Var Explained","Cum Var Explained")
    PCA_summary_df = cbind(summary_rownames,PCA_summary_df)
    colnames(PCA_summary_df)[1] = "Metric"
    
    output$PCA_fcoeffs = DT::renderDataTable(server = T, {data = datatable(PCA_coefficients,rownames = F,selection =
                                                                             list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                                                                             list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                                                                               className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                          "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$PCA_fsummary = DT::renderDataTable(server = T, {data = datatable(PCA_summary_df,rownames = F,selection =
                                                                              list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions="Buttons", options =
                                                                              list(autoWidth = F, dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = T,columnDefs = list(list(
                                                                                className = 'dt-center',orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {",
                                                                                                                                                           "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    pcr_stepr = round((max(pcr_fits[,2])-min(pcr_fits[,2]))/40,2)
    
    updateNumericInput(session, "PCR_stand",
                       value = round(mean(pcr_fits[,2]),2),
                       max = round(max(pcr_fits[,2]),2),
                       min = round(min(pcr_fits[,2]),2),
                       step = pcr_stepr
    )
    
    pcr_stepdc = round((max(pcr_fits[,3])-min(pcr_fits[,3]))/40,2)
    
    updateNumericInput(session, "PCR_dec_crit",
                       value = round(mean(pcr_fits[,3]),2),
                       max = round(max(pcr_fits[,3]),2),
                       min = round(min(pcr_fits[,3]),2),
                       step = pcr_stepdc
    )
    
    output$PCR_fits = DT::renderDataTable(server = T, {data = datatable(pcr_fits,rownames = F,selection =
                                                                          list(selected = list(rows = NULL, cols = NULL),target = "row",mode = "single"),editable = F,extensions='Buttons',options = list(autoWidth = F,
                                                                                                                                                                                                          paging = TRUE,pageLength = 17,dom="ltBp",buttons = c('copy', 'csv', 'excel'),scrollX = TRUE,scrollY = TRUE,columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                                                                                                                                                                                            orderable = T,targets = '_all')),initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#073744',
              'color': '#fff'});","}")))
    })
    
    PCR_scat_dat <<- pcr_fits[,1:3]
    output$PCR_scatplot = renderPlotly(scatter_confuse(PCR_scat_dat,input$PCR_stand,input$PCR_dec_crit))
    
    confuse_results = confuse(PCR_scat_dat[,2:3],input$PCR_stand,input$PCR_dec_crit)
    confuse_table = matrix(0,nrow=1,ncol=4)
    
    confuse_table[1,1] = confuse_results$TP
    confuse_table[1,2] = confuse_results$TN
    confuse_table[1,3] = confuse_results$FP
    confuse_table[1,4] = confuse_results$FN
    
    colnames(confuse_table) = c("True Positives","True Negatives", "False Positives","False Negatives")
    
    output$PCR_confuse = DT::renderDataTable(server = T, {data = datatable(confuse_table,rownames = F,selection = 
                                                                             list(selected = list(rows = NULL, cols = NULL), target = "row",mode = "single"),editable = F,extensions='Buttons',
                                                                           options = list(autoWidth = F,dom='tB',buttons = c('copy', 'csv', 'excel'),paging = F,scrollX = F,scrollY = F,
                                                                                          columnDefs = list(list(className = 'dt-center',orderable = F,targets = '_all')),initComplete = JS("function(settings, json) 
                      {","$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});","}")))})
    
    output$PCR_confuse_text = renderText({paste0("Sensitivity = ",round(confuse_results$Sensitivity,3),"; Specificity = ",
                                                 round(confuse_results$Specificity,3),"; Accuracy = ",round(confuse_results$Accuracy,3))})
    
    resid_data = PCR_scat_dat[,c(1,3)] %>% mutate(Residuals = round(PCR_scat_dat[,2]-PCR_scat_dat[,3],3))
    
    output$PCR_resid_scatplot = renderPlotly(scatter(resid_data))
    
    output$PCR_lineplot = renderPlotly(plot_ly(PCR_scat_dat, x = ~PCR_scat_dat[,1], y = ~PCR_scat_dat[,2], name="Observations",
                                               type="scatter", mode = "lines",text = ~paste("<b>ID: </b>",PCR_scat_dat[,1],"<br><b>Observed Value:</b> ",
                                                                                            PCR_scat_dat[,2],sep=""),hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5)) %>%
                                         add_trace(y = ~PCR_scat_dat[,3], name="Fitted_Values", mode = 'lines',
                                                   text = ~paste("<b>ID: </b>",PCR_scat_dat[,1],"<br><b>Fitted Value:</b> ",round(PCR_scat_dat[,3],3),sep=""),
                                                   hoveron = 'points',hoverinfo='text', line = list(color = "#2c3e50", width = 1.5, dash='dash')) %>%
                                         layout(xaxis = list(title = list(text='ID',font=list(size=20))),yaxis = list(title = list(text="Observations/Fits",font=list(size=20)),
                                                                                                                      range=c(min(0.99*min(PCR_scat_dat[,2],PCR_scat_dat[,3]),1.01*min(PCR_scat_dat[,2],PCR_scat_dat[,3])),
                                                                                                                              max(0.99*max(PCR_scat_dat[,2],PCR_scat_dat[,3]),1.01*max(PCR_scat_dat[,2],PCR_scat_dat[,3]))))))
    
    removeModal()
    
    updateTabsetPanel(session, inputId = 'shinyVB', selected = 'Modeling')
    updateTabsetPanel(session, inputId = 'modeling_tabs', selected = 'PCR: Fitting')
  }
})