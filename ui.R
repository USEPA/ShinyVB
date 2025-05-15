source("bs_multi.R")
source("ui_prediction.R")
source("ui_modeling.R")
source("ui_data.R")
source("ui_map.R")

# Define UI
ui = fluidPage(
  
  useShinyjs(),
  
  tags$head(tags$style(HTML('
    .selectize-input {white-space: nowrap}
    #send_plot+ div>.selectize-dropdown{width: 100px !important;}
    #send_plot+ div>.selectize-input{width: 100px !important;padding: 3px;}
    #scatterx+ div>.selectize-dropdown{width: 175px !important;}
    #scatterx+ div>.selectize-input{width: 130px !important;padding: 3px;}
    #xgb_hyper_metric+ div>.select-dropdown{width: 100px !important;}
    #xgb_hyper_metric+ div>.select-dropdown-content{height: 200px !important;}
    #scattery+ div>.selectize-dropdown{width: 175px !important;}
    #scattery+ div>.selectize-input{width: 130px !important;padding: 3px;}'))),
  
  tags$style(HTML("
  
    .tabbable > .nav > li > a                  {background-color: lightgray;  color:black}
    
    .tabbable > .nav > li[class=active]    > a {background-color: #2c3e50; color:white}

    .btn-default {
       margin: 2px 2px;
       font-size: 14px;
       color: #000000;
       border-color: #2c3e50;
       text-align: center;
    }
    .align-center {
      display: flex;
      align-items: center;
    }
    .progress-message, .progress-detail {
      float: left; 
      clear: left
    }
    #MC_runs {
      width: 100px;
      height: 35px;
    }
    #num_folds {
      width: 80px;
      height: 35px;
    }
    #pcr_prop {
      width: 100px;
      height: 35px;
    }
    #train_pct {
      width: 80px;
      height: 35px;
    }
    #num_axes_using {
      width: 80px;
      height: 35px;
      vertical-align: -30px;
    }
    #XGBCL_binarize_crit_value {
      width: 90px;
      height: 40px;
      vertical-align: -30px;
    }
    #data_seed {
      width: 100px;
      height: 35px;
    }
    #model_seed {
      width: 100px;
      height: 35px;
    }
    #EN_stand {
      width: 120px;
      height: 35px;
        }
    #EN_dec_crit {
      width: 120px;
      height: 35px;
    }
    #LG_binarize_crit_value {
      width: 90px;
      height: 40px;
    }

    #LG_pred_dc {
      width: 120px;
      height: 35px;
    }
    #LG_fit_dc {
      width: 120px;
      height: 35px;
    }
    #XGB_stand {
      width: 120px;
      height: 35px;
        }
    #XGB_dec_crit {
      width: 120px;
      height: 35px;
    }
    #XGBCL_pred_dc {
      width: 120px;
      height: 35px;
    }
    #XGBCL_dec_crit{
      width: 120px;
      height: 35px;
    }
    .tab-pane {
      height: calc(100vh - 120px) !important;
    }
    #iso_ndim {
      width: 70px;
    }
    #lc_lowval {
      height: 35px;
    }
    #lc_upval {
      height: 35px;
    }
    #rc_lowval {
      height: 35px;
    }
    #rc_upval {
      height: 35px;
    }
    #lc_lowval {
      height: 35px;
    }
    #test_weight {
      width: 100px;
      height: 35px;
    }
    #logist_train_pct {
      width: 100px;
      height: 35px;
    }
    .bslib-card .card-body {
       overflow:hidden;
    }
    .shiny-notification {
       position: fixed;
       top: 45%;
       left: 45%;
    }
    .btn-default:hover {
       background-color: #2c3e50;
       color: #18bc9c;
    }
    .panel-heading:hover {
      background-color: #cccccc;
      color: #18bc9c;
    }
    .panel-body {
      background-color: #ecf0f1;
    }
    
    .shiny-input-panel {
        margin: 0px 0px;
        padding: 0px 0px;
        border: 0px solid #e3e3e3;
        background-color: #ecf0f1;
    }
    .selectize-input {
      width: 110px;
    }
    #LG_eval {
      width: 125px;
      height: 40px;
    }
    #XGBCL_eval {
      width: 125px;
      height: 40px;
    }
    .modal-footer {
      text-align: center;
    }
                  
  ")),
  
  navbarPage(
    title = "Virtual Beach",
    id="shinyVB",
    theme = shinytheme("flatly"),
    
    tabPanel(title = "Landing",style = "background-color: #acbdb0;",
             p( 
               h6(".", align = "center"),
               h2("", align = "center"),
               h2("", align = "center"),
               h2("", align = "center"),
               h2("Welcome to ShinyVB!", align = "center"),
               h3("The latest form in the evolutionary line of the Virtual Beach software toolkit.", align = "center"),
               h3("Email cyterski.mike@epa.gov with comments and questions.", align = "center")
             )
             ,div(align="center",img(src="logo.png",width=450))),

    tabPanel(title = "Map",MapPanel),
    
    tabPanel(title = "Data",DataPanel),
    
    tabPanel(title = "Modeling",ModelingPanel),
    
    tabPanel(title = "Prediction",PredictionPanel)
  )
)