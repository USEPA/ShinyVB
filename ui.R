source("bs_multi.R")
source("ui_map.R")
source("ui_data.R")
source("ui_modeling.R")
source("ui_prediction.R")

ui = fluidPage(

  useShinyjs(),

  tags$head(
    
  tags$style(type = "text/css", HTML("
  #trans_table_wrap .dataTables_scrollHead th,
  #trans_table_wrap table.dataTable thead th,
  #inter_table_wrap .dataTables_scrollHead th,
  #inter_table_wrap table.dataTable thead th {
    background-color: #073744 !important;
    color: #fff !important;
    cursor: default;
    text-align: center;
  }")),
    
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
  
    #pd_feat_ranges th, #pd_feat_ranges td, #pd_data th, #pd_data td {
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

    .custom-btn {
       padding: 5px !important;
       height: 35px !important;
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
       margin: 5px 5px !important;
       height: 35px !important;
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

    #feats_to_corr .btn,
    #feats_to_use .btn,
    #pcax_to_use .btn {
      background-color: lightgray !important;
      color: #495057 !important;
      border-color: black !important;
      box-shadow: none !important;
    }

    #feats_to_corr .btn.active,
    #feats_to_use .btn.active,
    #pcax_to_use .btn.active,
    #feats_to_corr .btn[aria-pressed='true'],
    #feats_to_use .btn[aria-pressed='true'],
    #pcax_to_use .btn[aria-pressed='true'] {
      background-color: #2c3e50 !important;
      color: #ffffff !important;
      border-color: green !important;
      box-shadow: none !important;
    }

    #feats_to_corr input.btn-check:checked + label.btn,
    #feats_to_use input.btn-check:checked + label.btn,
    #pcax_to_use input.btn-check:checked + label.btn {
      background-color: #2c3e50 !important;
      color: #ffffff !important;
      border-color: green !important;
      box-shadow: none !important;
    }

    #feats_to_corr .btn:hover,
    #feats_to_use .btn:hover,
    #pcax_to_use .btn:hover,
    #feats_to_corr .btn:focus,
    #feats_to_use .btn:focus,
    #pcax_to_use .btn:focus {
      filter: brightness(0.95);
      box-shadow: none !important;
    }
              
    #feats_select {
      vertical-align: middle;
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
      height: 40px;
      width: 80px !important;
      padding: 5px;
    }
    
    .shiny-download-link {
      display: flex;
      justify-content: center;
      align-items: center;
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
    
    #MC_runs, #num_preds, #conf_bound, #lc_val, #rc_val, #pcr_prop, #model_seed, #test_weight, #testcl_weight, #logist_train_pct {
      width: 100px;
      height: 35px;
    }
    
    #num_folds, #train_pct, #num_axes, #iso_ndim, #sig_digies, #num_axes_using, #XGBCL_binarize_crit_value, #beach_angle, #LG_binarize_crit_value {
      width: 85px;
      height: 35px;
    }
    
    #lc_replace, #rc_replace {
      width: 95px;
      height: 35px;
    }
    
    #num_axes_using {
      vertical-align: -30px;
    }
    
    #EN_pred_stand, #lc_lowval, #lc_upval, #LG_eval, #rc_lowval, #rc_upval, #XGBCL_eval, #EN_pred_dc, #EN_stand, #EN_dec_crit, #LG_pred_dc, #LG_fit_dc, #XGBCL_pred_dc, #XGBCL_dec_crit, #XGB_pred_stand, #XGB_stand, #XGB_pred_dc, #XGB_dec_crit {
      width: 115px;
      height: 35px;
    }
    
    #EN_pred_stand, #EN_stand, #XGB_pred_stand, #XGB_stand {
      border: 1px solid #0d6efd !important;
    }

    #EN_pred_stand:hover, #EN_stand:hover, #XGB_pred_stand:hover, #XGB_stand:hover, #EN_pred_stand:focus, #EN_stand:focus, #XGB_pred_stand:focus, #XGB_stand:focus {
      border-color: #0d6efd !important;
      box-shadow: 0 0 0 0.2rem rgba(13,110,253,0.25);
      outline: 0;
    }

    #EN_pred_dc,#EN_dec_crit,#XGB_pred_dc,#XGB_dec_crit {
      border: 1px solid #198754 !important;
    }

    #EN_pred_dc:hover,#EN_dec_crit:hover,#XGB_pred_dc:hover,#XGB_dec_crit:hover,#EN_pred_dc:focus,#EN_dec_crit:focus,#XGB_pred_dc:focus,#XGB_dec_crit:focus {
      border-color: #198754 !important;
      box-shadow: 0 0 0 0.2rem rgba(25,135,84,0.25);
      outline: 0;
    }
    
    label[for='EN_pred_stand'],label[for='EN_stand'],label[for='XGB_pred_stand'],label[for='XGB_stand'] {
      color: #0d6efd !important;
    }

    label[for='EN_pred_dc'],label[for='EN_dec_crit'],label[for='XGB_pred_dc'],label[for='XGB_dec_crit'] {
      color: #198754 !important;
    }
    
    #LG_eval {
      padding: 0px 0px;
    }
    
  ")),
  
  tags$script(HTML(
    
    "Shiny.addCustomMessageHandler('download', function(message) {
      var link = document.createElement('a');
      link.href = message.url;
      link.download = message.filename;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    });",
    
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
      });",
    
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