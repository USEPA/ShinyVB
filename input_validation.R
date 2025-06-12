add_validation_rules <- function(iv) {
  
  # iv$add_rule("lc_val", sv_between(-1000000,0))
  # iv$add_rule("rc_val", sv_between(1000,1000000))
  # iv$add_rule("lc_lowval", sv_between(0,1))
  # iv$add_rule("lc_upval", sv_between(1,10))
  # iv$add_rule("rc_lowval", sv_between(0,100))
  # iv$add_rule("rc_upval", sv_between(1000,10000))
  
  iv$add_rule("train_pct", sv_between(1,100))
  iv$add_rule("MC_runs", sv_between(2,10000))
  iv$add_rule("num_folds", sv_between(2,20))
  
  iv$add_rule("model_seed", sv_gte(1))
  iv$add_rule("iso_ndim", sv_between(1,10))
  iv$add_rule("sig_digies", sv_between(0,12))
  
  iv$add_rule("psocl_max_iter", sv_between(5,1000))
  iv$add_rule("psocl_swarm_size", sv_between(3,200))
  iv$add_rule("membercl_exp", sv_between(0.25,3))
  iv$add_rule("sscl_exp", sv_between(0.25,3))
  
  iv$add_rule("etacl", sv_between(0,1))
  iv$add_rule("gammacl", sv_between(0,20))
  iv$add_rule("nroundscl", sv_between(100,3000))
  iv$add_rule("max_depthcl", sv_between(1,10))
  iv$add_rule("min_child_weightcl", sv_between(1,20))
  iv$add_rule("subsampcl", sv_between(0,1))
  iv$add_rule("colsampcl", sv_between(0,1))
  iv$add_rule("ratecl_drop", sv_between(0,1))
  iv$add_rule("skipcl_drop", sv_between(0,1))
  
  iv$add_rule("LG_pred_dc", sv_between(0, 1))
  iv$add_rule("LG_fit_dc", sv_between(0, 1))
  iv$add_rule("XGBCL_pred_dc", sv_between(0,1))
  iv$add_rule("XGBCL_dec_crit", sv_between(0,1))
  
  iv$add_rule("pso_max_iter", sv_between(5, 1000))
  iv$add_rule("pso_swarm_size", sv_between(3, 200))
  iv$add_rule("member_exp", sv_between(0.25, 3))
  iv$add_rule("ss_exp", sv_between(0.25,3))
  
  iv$add_rule("eta", sv_between(0,1))
  iv$add_rule("gamma", sv_between(0,20))
  iv$add_rule("nrounds", sv_between(100,3000))
  iv$add_rule("max_depth", sv_between(1,10))
  iv$add_rule("min_child_weight", sv_between(1,20))
  iv$add_rule("subsamp", sv_between(0,1))
  iv$add_rule("colsamp", sv_between(0,1))
  iv$add_rule("rate_drop", sv_between(0,1))
  iv$add_rule("skip_drop", sv_between(0,1))
  
  iv$add_rule("num_axes", sv_between(2,20))
  iv$add_rule("num_preds", sv_between(2,1000))
  iv$add_rule("conf_bound", sv_between(0.1,0.99))
}