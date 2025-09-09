source("bs_multi.R")
source("ui_map.R")
source("ui_data.R")
source("ui_modeling.R")
source("ui_prediction.R")

ui = fluidPage(

  useShinyjs(),

  tags$head(
    
  tags$style(HTML("
    .selectize-input {white-space: nowrap}
    
    #send_plot+ div>.selectize-dropdown{width: 100px !important;}
    
    #send_plot+ div>.selectize-input{width: 100px !important;padding: 3px;}
    
    #scatterx+ div>.selectize-dropdown{width: 175px !important;}
    
    #scatterx+ div>.selectize-input{width: 130px !important;padding: 3px;}
    
    #IDasDate+ div>.selectize-input{width: 130px !important;}
    
    #set_column_props+ div>.selectize-input{width: 140px !important;}
    
    #xgb_hyper_metric+ div>.select-dropdown{width: 100px !important;}
    
    #xgb_hyper_metric+ div>.select-dropdown-content{height: 200px !important;}
    
    #scattery+ div>.selectize-dropdown{width: 175px !important;}
    
    #scattery+ div>.selectize-input{width: 130px !important;padding: 3px;}
  
    #pd_feat_ranges th, #pd_feat_ranges td {
      min-width: 60px !important;
    }
    #pd_data th, #pd_data td {
      min-width: 60px !important;
    }
    
    .highlighted-cell {
      background-color: #BA0C2F !important;
      color: #000000 !important;
    }
      
    .dt-green {
      background-color: #aaeeaa !important;
      color: #000000 !important;
    }
  
    .tabbable > .nav > li > a {
      background-color: lightgray;
      color:black;
    }
    
    .tabbable > .nav > li[class=active] > a {
      background-color: #2c3e50 !important;
      color:white !important;
    }
    
    .bslib-card .card-body {
       overflow:hidden;
    }
    
    .shiny-notification {
       position: fixed;
       top: 45%;
       left: 45%;
    }
    
    #XGBCL_optim_hp .dataTables_wrapper .dataTables_scroll {
      width: 70% !important;
    }
    
    #XGB_optim_hp .dataTables_wrapper .dataTables_scroll {
      width: 70% !important;
    }
    
    .tab-pane {
      height: calc(100vh - 120px) !important;
    }
    
    .navbar-nav li.disabled {
        pointer-events: none;
        opacity: 0.5;
    }
    
    .btn-default {
       color: black !important;
       background-color: #A9A9A9 !important;
       border-color: #D3D3D3 !important;
       text-align: center !important;
    }
    
    .btn-default:hover {
       background-color: #2c3e50 !important;
       color: #18bc9c !important;
    }
    
    .btn.checkbtn.btn-custom.active {
      background-color: #2c3e50;
      color: white;
      border-color: green;
    }
    
    .btn.checkbtn.btn-custom {
      background-color: #cccccc;
      color: #111111;
      border-color: #2c3e50;
    }
    
    .custom-btn {
       margin: 2px 2px !important;
       font-size: 14px !important;
       color: white !important;
       background-color: #5A6E5A !important;
       border-color: #D3D3D3 !important;
       text-align: center !important;
    }
    
    .custom-btn:disabled {
      color: #8A3D45 !important;
    }
    
    .custom-btn:hover {
       background-color: #2c3e50 !important;
       color: #18bc9c !important;
    }
    
    .custom2-btn {
       margin: 2px 2px !important;
       font-size: 14px !important;
       color: black !important;
       background-color: #E0F7FA !important;
       border-color: #D3D3D3 !important;
       text-align: center !important;
    }
    
    .custom2-btn:disabled {
      color: #CD8295 !important;
    }
    
    .custom2-btn:hover {
       background-color: #2c3e50 !important;
       color: #18bc9c !important;
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
    
    .shiny-input-container {
        margin-top: 2px;
        margin-bottom: 2px;
    }
    
    .selectize-input {
      width: 110px;
    }
    
    .btn-default.btn-file {
      height: 42px;
      width: 80px !important;
      padding: 6px;
    }
    
    .shiny-download-link {
      display: flex;
      justify-content: center; /* Centers horizontally */
      align-items: center; /* Centers vertically */
      height: 40px;
      text-align: center;
      padding: 0;
    }
    
    .modal-footer {
      text-align: center;
    }
    
    .align-center {
      display: flex;
      align-items: center;
    }
    
    .progress-message, .progress-detail {
      float: left; 
      clear: left;
    }
    
    #sig_digies {
      width: 75px;
    }
    
    #MC_runs {
      width: 100px;
      height: 35px;
    }
    
    #num_folds {
      width: 80px;
      height: 35px;
    }
    
    #num_preds {
      width: 100px;
      height: 40px;
    }
    
    #conf_bound {
      width: 100px;
      height: 40px;
    }
    
    #lc_val {
      width: 100px;
      height: 35px;
    }
    
    #rc_val {
      width: 100px;
      height: 35px;
    }
    
    #lc_replace {
      width: 90px;
      height: 35px;
    }
    
    #rc_replace {
      width: 90px;
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
    
    #XGBCL_binarize_crit_value, #beach_angle {
      width: 90px;
      height: 35px;
    }
    
    #model_seed {
      width: 100px;
      height: 35px;
    }
    
    #EN_pred_stand, #EN_pred_dc, #EN_stand, #EN_dec_crit, #LG_pred_dc, #LG_fit_dc, #XGBCL_pred_dc, #XGBCL_dec_crit, #XGB_pred_stand, #XGB_stand, #XGB_pred_dc, #XGB_dec_crit {
      width: 120px;
      height: 35px;
    }
    
    #LG_binarize_crit_value {
      width: 90px;
      height: 35px;
    }
    
    #iso_ndim {
      width: 70px;
    }
    
    #lc_lowval, #lc_upval, #rc_lowval, #rc_upval {
      width: 110px;
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
    
    #LG_eval {
      width: 125px;
      height: 40px;
    }
    
    #XGBCL_eval {
      width: 125px;
    }
  ")),
  
  tags$script(HTML(
    
    "$(document).ready(function() {
        $('#pd_data').on('focus', 'td', function(e) {
          var cell = $(this);
          var colIndex = cell.index();
          var totalCols = cell.closest('tr').children().length;
          if (colIndex >= 2 && colIndex < totalCols - 3) {
            cell.addClass('highlighted-cell');
          }
        }).on('blur', 'td', function(e) {
          var cell = $(this);
          var colIndex = cell.index();
          var totalCols = cell.closest('tr').children().length;
          if (colIndex >= 2 && colIndex < totalCols - 3) {
            if (cell.text() == '-999') {
              cell.addClass('highlighted-cell');
            } else {
              cell.removeClass('highlighted-cell').addClass('dt-green');
            }
          }
        });
      });
    ",
    
    "Shiny.addCustomMessageHandler('disableTabs', function(message) {
        if (message.action === 'disable') {
          $('#shinyVB li a:contains(\"Map\")').parent().addClass('disabled');
          $('#shinyVB li a:contains(\"Data\")').parent().addClass('disabled');
          $('#shinyVB li a:contains(\"Modeling\")').parent().addClass('disabled');}});",
    
    "Shiny.addCustomMessageHandler('enableTabs', function(message) {
        if (message.action === 'enable') {
          $('#shinyVB li a:contains(\"Map\")').parent().removeClass('disabled');
          $('#shinyVB li a:contains(\"Data\")').parent().removeClass('disabled');
          $('#shinyVB li a:contains(\"Modeling\")').parent().removeClass('disabled');}});",
    
    "$(document).ready(function() {
      $('#data').on('focus', 'input[type=number]', function() {
        $(this).attr('type', 'text');
      });});",
    
    "$(document).ready(function() {
      $('#pd_data').on('focus', 'input[type=number]', function() {
        $(this).attr('type', 'text');
      });});"
    ))),
  
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