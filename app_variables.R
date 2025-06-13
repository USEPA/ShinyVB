Version = "1.0.0"

# Leaflet reactive variables
bo = reactiveVal(0)
map_clicks = reactiveValues(points = data.frame())

# Read in WQX station and shoreline data
station_data = data.frame(read.csv("stations.csv"))
shoreline_data = data.frame(read.csv("shorelines.csv"))

# General reactive variables
refresh_trigger = reactiveVal(FALSE)
prediction_trigger = reactiveVal(FALSE)
clear_modeling = reactiveVal(FALSE)
current_data = reactiveVal()
pred_data = reactiveVal(NULL)
pred_residuals = reactiveVal()
pred_model_features = reactiveVal()
response_var = reactiveVal(2)
col_names = reactiveVal()
changed_model = reactiveVal(FALSE)
models_created = reactiveVal()
model_to_use = reactiveVal()
feat_names = reactiveVal()
feats_being_used = reactiveVal()
fs_feats_used = reactiveVal()
PCA_scaling_mean = reactiveVal()
PCA_scaling_sd = reactiveVal()
PCA_dataset = reactiveVal()
PCA_summary_df = reactiveVal()
PCA_coefficients = reactiveVal()
pca_axes_max = reactiveVal(20)
pca_axes = reactiveVal()
pcax_being_used = reactiveVal()
fs_pcax_used = reactiveVal()
last_plot = reactiveVal(NULL)
redraw_rainplot = reactiveVal(FALSE)
redraw_scatplot = reactiveVal(FALSE)
redraw_lineplot = reactiveVal(FALSE)
current_data_page = reactiveVal(1)
current_pred_page = reactiveVal(1)

# General non-reactive variables
init_data = data.frame()
init_column_props = hash()
column_props = hash()
id_var = 1
ignored_rows = NULL
num_rows_per_page = 20
date_format_string = "MDY"

# XGB hyperparameters
xgb_tree_method_set = reactiveVal("hist")
xgb_booster_set = reactiveVal("gbtree")
dart_normalize_type_set = reactiveVal("tree")
dart_sample_type_set = reactiveVal("uniform")
rate_drop_set = reactiveVal(0.1)
skip_drop_set = reactiveVal(0.5)
eta_set = reactiveVal(0.05)
gamma_set = reactiveVal(1)
max_depth_set = reactiveVal(2)
min_child_weight_set = reactiveVal(3)
nrounds_set = reactiveVal(100)
subsamp_set = reactiveVal(0.8)
colsamp_set = reactiveVal(0.8)
xgb_select_result = reactiveVal()
xgb_select_calculation = NULL
running = reactiveVal(FALSE)
# xgb_hyper_result = reactiveVal()
# xgb_hyper_calculation = NULL

# XGBCL hyperparameters
xgbcl_tree_method_set = reactiveVal("hist")
xgbcl_booster_set = reactiveVal("gbtree")
dartcl_normalize_type_set = reactiveVal("tree")
dartcl_sample_type_set = reactiveVal("uniform")
ratecl_drop_set = reactiveVal(0.1)
skipcl_drop_set = reactiveVal(0.5)
etacl_set = reactiveVal(0.05)
gammacl_set = reactiveVal(1)
max_depthcl_set = reactiveVal(2)
min_child_weightcl_set = reactiveVal(3)
nroundscl_set = reactiveVal(100)
subsampcl_set = reactiveVal(0.8)
colsampcl_set = reactiveVal(0.8)
xgbcl_select_result = reactiveVal()
xgbcl_select_calculation = NULL

#General modeling hyperparameters
early_stop_set = reactiveVal(20)
nfold_set = reactiveVal(5)

# Logistic Regression Prediction Results
LG_pred_results = reactiveVal()
LG_pred_coeffs = reactiveVal()
LG_pred_confuse_results = reactiveVal()
LG_pred_scat_dat = reactiveVal()
LG_pred_standardize = reactiveVal(TRUE)
LG_pred_thresh = reactiveVal()

# Logistic Regression Fitting Results
LG_results = reactiveVal()
LG_coeffs = reactiveVal()
LG_confuse_results = reactiveVal()
LG_scat_dat = reactiveVal()
LG_model = NULL
LG_model_PCA = reactiveVal(FALSE)
LG_standardize = reactiveVal(TRUE)
LG_thresh = reactiveVal()

# XGBoost Classifier Prediction Results
XGBCL_pred_results = reactiveVal()
XGBCL_pred_coeffs = reactiveVal()
XGBCL_pred_confuse_results = reactiveVal()
XGBCL_pred_scat_dat = reactiveVal()
XGBCL_pred_standardize = reactiveVal(FALSE)
XGBCL_pred_thresh = reactiveVal()

# XGBoost Classifier Other Results
refresh_XGBCL_Optim_HP = reactiveVal(FALSE)
XGBCL_selection_results = reactiveVal()
Optimal_CLHP = data.frame(max_depth = 2,eta = 0.05,subsample = 0.8,colsample_bytree = 0.8,min_child_weight = 3,gamma = 1,nrounds = 100)

# XGBoost Classifier Fitting Results
XGBCL_results = reactiveVal()
XGBCL_coeffs = reactiveVal()
XGBCL_confuse_results = reactiveVal()
XGBCL_scat_dat = reactiveVal()
XGBCL_model = NULL
XGBCL_model_PCA = reactiveVal(FALSE)
XGBCL_standardize = reactiveVal(FALSE)
XGBCL_final_data = reactiveVal()
XGBCL_thresh = reactiveVal()

# XGBoost Prediction Results
XGB_pred_results = reactiveVal()
XGB_pred_coeffs = reactiveVal()
XGB_pred_confuse_results = reactiveVal()
XGB_pred_scat_dat = reactiveVal()
XGB_pred_standardize = reactiveVal(FALSE)

# XGBoost Other Results
refresh_XGB_Optim_HP = reactiveVal(FALSE)
XGB_selection_results = reactiveVal()
Optimal_HP = data.frame(max_depth = 2,eta = 0.05,subsample = 0.8,colsample_bytree = 0.8,min_child_weight = 3,gamma = 1,nrounds = 100)

# XGBoost Fitting Results
XGB_results = reactiveVal()
XGB_coeffs = reactiveVal()
XGB_confuse_results = reactiveVal()
XGB_scat_dat = reactiveVal()
XGB_model = NULL
XGB_model_PCA = reactiveVal(FALSE)
XGB_standardize = reactiveVal(FALSE)
XGB_final_data = reactiveVal()

# Elastic Net Prediction Results
EN_pred_results = reactiveVal()
EN_pred_coeffs = reactiveVal()
EN_pred_confuse_results = reactiveVal()
EN_pred_scat_dat = reactiveVal()
EN_pred_standardize = reactiveVal(TRUE)

# Elastic Net Fitting Results
EN_results = reactiveVal()
EN_coeffs = reactiveVal()
EN_confuse_results = reactiveVal()
EN_scat_dat = reactiveVal()
EN_model = NULL
EN_model_PCA = reactiveVal(FALSE)
EN_standardize = reactiveVal(TRUE)

# Parameters needed for Prediction Tab
final_model_PCA = reactiveVal(FALSE)
feature_mismatch = reactiveVal(FALSE)
standard_mismatch = reactiveVal(FALSE)
thresh_mismatch = reactiveVal(FALSE)
no_resids = reactiveVal(FALSE)
model_PCA_axes = reactiveVal()