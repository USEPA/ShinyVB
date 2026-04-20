get_model_params = function(fit) {
  alpha = fit$alpha
  lambdaMin = sapply(fit$modlist, `[[`, "lambda.min")
  lambdaSE = sapply(fit$modlist, `[[`, "lambda.1se")
  error = sapply(fit$modlist, function(mod) {min(mod$cvm)})
  best = which.min(error)
  data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
             lambdaSE = lambdaSE[best], eror = error[best])
}

MC_subbin = function(data,loggy,lc_val,lc_lowval,lc_upval,rc_val,rc_lowval,rc_upval) {
  
  if (loggy) {
    for (j in 1:nrow(data)) {
      if (data[j, 1] == lc_val) {
        data[j, 1] = log10(runif(1, min = lc_lowval, max = lc_upval))
      }
      if (data[j, 1] == rc_val) {
        data[j, 1] = log10(runif(1, min = rc_lowval, max = rc_upval))
      }
    }
  } else {
    for (j in 1:nrow(data)) {
      if (data[j, 1] == lc_val) {
        data[j, 1] = (runif(1, min = lc_lowval, max = lc_upval))
      }
      if (data[j, 1] == rc_val) {
        data[j, 1] = (runif(1, min = rc_lowval, max = rc_upval))
      }
    }
  }
  MC_data = data
}

MC_final_subbin = function(data,loggy,lc_val,rc_val,lowmult,highmult) {
  
  exclude_values = c(lc_val,rc_val)
  real_responses = na.omit(data[!data[,1] %in% exclude_values,1])
  
  if (loggy) {
    
    for (j in 1:nrow(data)){
      if (data[j,1]==rc_val) {
        data[j,1]=log10(lowmult*min(real_responses))
      }
      
      if (data[j,1]==lc_val) {
        data[j,1]=log10(highmult*max(real_responses))
      }
    }
  } else {
    
    for (j in 1:nrow(data)){
      if (data[j,1]==rc_val) {
        data[j,1]=lowmult*min(real_responses)
      }
      
      if (data[j,1]==lc_val) {
        data[j,1]=highmult*max(real_responses)
      }
    }
  }
  
  MC_data = data
}

create_data = function(data,rv,feats_to_use,ignored_rows,randomize,standardize) {
  
  if (is.null(ignored_rows)) {
    data = data
  } else {
    data = data[-ignored_rows,]
  }
  
  data = data[!is.na(data[,rv]),]
  
  var_list = c(1,rv,which(colnames(data) %in% feats_to_use))
  data1 = data[,var_list]
  colnames(data1) = c(colnames(data)[1],"Response",feats_to_use)
  
  if (randomize) {
    random_index = sample(1:nrow(data1), nrow(data1))
    data1 = data1[random_index, ]
  }
  
  if (standardize) {
    
    for (i in 1:nrow(data1)) {
      for (j in 3:ncol(data1)) {
        if (is.numeric(data1[i,j])) {
          
          range = (max(na.omit(data1[,j])) - min(na.omit(data1[,j])))
          
          if (range == 0) {
            data1[i,j] = 0
          } else if (range < 1) {
            data1[i,j]= round((1 + (data1[i,j] - min(na.omit(data1[,j])))) / (range+1),4)
          } else {
            data1[i,j]=round((data1[i,j] - min(na.omit(data1[,j]))) / range,4)
          }
        }
      }
    }
  }
  created_data = data1
}

custom_xgb_loss = function(preds, dtrain, rw) {
  labels = getinfo(dtrain, "label")
  
  # Calculate residuals
  residuals = preds - labels
  
  # Calculate RSS
  rss = sum(residuals^2)
  
  # Calculate range penalty
  observed_range = max(labels) - min(labels)
  predicted_range = max(preds) - min(preds)
  range_penalty = (observed_range - predicted_range)^2
  
  # Weighting factor for range penalty
  range_weight = rw
  
  # Combine RSS and range penalty
  loss = rss + range_weight * range_penalty
  
  # Gradient and Hessian (derivatives)
  grad = 2 * residuals + 2 * range_weight * (predicted_range - observed_range)
  hess = rep(2, length(preds))
  
  return(list(grad = grad, hess = hess))
}

magnitude_round = function(x) {
  ifelse(abs(x) < 0.001, round(x, 6),
         ifelse(abs(x) < 0.01, round(x, 5),
                ifelse(abs(x) < 0.1, round(x, 4),
                       ifelse(abs(x) < 1, round(x, 3),
                              ifelse(abs(x) < 10, round(x, 2),
                                     round(x, 1)
                              )
                       )
                )
         )
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

get_current_response_name <- function() {
  df <- try(current_data(), silent = TRUE)
  if (inherits(df, "try-error") || is.null(df) || !is.data.frame(df) || !ncol(df)) return(NULL)
  idx <- try(response_var(), silent = TRUE)
  if (inherits(idx, "try-error") || !is.numeric(idx) || length(idx) != 1 || is.na(idx)) return(NULL)
  if (idx < 1 || idx > ncol(df)) return(NULL)
  colnames(df)[idx]
}

set_prev_response_name <- function(name) {
  options(ShinyVB.prev_resp_name = if (is.null(name)) NULL else as.character(name))
}

# get_derived_pattern <- function() {
#   prefixes <- NULL
#   if (exists("DERIVED_PREFIXES", envir = .GlobalEnv, inherits = FALSE)) {
#     prefixes <- get("DERIVED_PREFIXES", envir = .GlobalEnv, inherits = FALSE)
#   } else {
#     tp <- get0("TRANS_PREFIXES", envir = .GlobalEnv, ifnotfound = character(0))
#     ip <- get0("INTER_PREFIX", envir = .GlobalEnv, ifnotfound = character(0))
#     prefixes <- c(tp, ip)
#   }
#   if (!length(prefixes)) return(NULL)
#   sprintf("^(%s)", paste(escape_regex(prefixes), collapse = "|"))
# }
# 
# find_interaction_cols <- function(df) {
#   if (is.null(df) || !is.data.frame(df)) return(character(0))
#   nm <- names(df); if (!length(nm)) return(character(0))
#   pat <- if (exists("INTER_PATTERN", inherits = FALSE)) {
#     get("INTER_PATTERN", inherits = FALSE)
#   } else if (exists("INTER_PREFIX", inherits = FALSE)) {
#     sprintf("^%s", escape_regex(get("INTER_PREFIX", inherits = FALSE)))
#   } else {
#     return(character(0))
#   }
#   nm[grepl(pat, nm, perl = TRUE)]
# }

clear_trans_table <- function(drop_transforms   = TRUE,
                              drop_interactions = TRUE,
                              drop_AO           = FALSE,
                              column_props      = NULL) {
  if (is.null(column_props)) {
    column_props <- get0("column_props", envir = .GlobalEnv, ifnotfound = NULL)
  }
  
  df <- try(current_data(), silent = TRUE)
  if (inherits(df, "try-error") || is.null(df) || !is.data.frame(df) || ncol(df) == 0L) return(invisible(NULL))
  cols <- colnames(df)
  
  rv_idx  <- try(response_var(), silent = TRUE)
  rv_name <- if (!inherits(rv_idx, "try-error") && is.numeric(rv_idx) && length(rv_idx) == 1L &&
                 rv_idx >= 1L && rv_idx <= ncol(df)) colnames(df)[rv_idx] else NULL
  prev_resp <- getOption("ShinyVB.prev_resp_name", NULL)
  options(ShinyVB.prev_resp_name = NULL)
  protect <- unique(stats::na.omit(as.character(c(rv_name, prev_resp))))
  
  to_remove <- character(0)
  
  if (isTRUE(drop_transforms)) {
    TRANS_PREFIXES <- get0("TRANS_PREFIXES", envir = .GlobalEnv, ifnotfound = character(0))
    if (length(TRANS_PREFIXES)) {
      trans_pat <- sprintf("^(%s)", paste(escape_regex(TRANS_PREFIXES), collapse = "|"))
      to_remove <- c(to_remove, cols[grepl(trans_pat, cols, perl = TRUE)])
    }
  }
  
  if (isTRUE(drop_interactions)) {
    inter_pat <- get0("INTER_PATTERN", envir = .GlobalEnv, ifnotfound = NULL)
    if (is.null(inter_pat)) {
      INTER_PREFIX <- get0("INTER_PREFIX", envir = .GlobalEnv, ifnotfound = character(0))
      if (length(INTER_PREFIX)) inter_pat <- sprintf("^(%s)", paste(escape_regex(INTER_PREFIX), collapse = "|"))
    }
    if (!is.null(inter_pat) && nzchar(inter_pat)) {
      to_remove <- c(to_remove, cols[grepl(inter_pat, cols, perl = TRUE)])
    }
  }
  
  if (isTRUE(drop_AO)) {
    ao_names <- get0("AO_COMP_NAMES", envir = .GlobalEnv,
                     ifnotfound = c("WindA","WindO","CurrentA","CurrentO","WaveA","WaveO"))
    to_remove <- unique(c(to_remove, cols[cols %in% ao_names]))
  }
  
  to_remove <- setdiff(unique(to_remove), protect)
  if (!length(to_remove)) return(invisible(NULL))
  
  # If dropping Polynomial transforms, prune persisted coeffs by base
  poly_prefix <- get0("prefix_map", envir = .GlobalEnv, ifnotfound = NULL)[["Polynomial"]]
  if (!is.null(poly_prefix)) {
    poly_drop <- grep(sprintf("^%s", escape_regex(poly_prefix)), to_remove, perl = TRUE, value = TRUE)
    if (length(poly_drop)) for (b in base_name(poly_drop)) del_poly_coeffs(b)
  }
  
  df[to_remove] <- NULL
  if (!is.null(column_props) && exists(".del", mode = "function")) {
    for (nm in to_remove) try(.del(column_props, keys = nm), silent = TRUE)
  }
  try(current_data(df), silent = TRUE)
  invisible(NULL)
}

# Trim get_interaction_candidates: remove star fallback per your note
get_interaction_candidates <- function(df, include_AO = TRUE) {
  nm <- names(df)
  id_name <- nm[1L]
  rv_name <- nm[response_var()]
  
  cand <- nm[vapply(df, is.numeric, logical(1))]
  cand <- setdiff(cand, c(id_name, rv_name))
  
  inter_pat <- get0("INTER_PATTERN", envir = .GlobalEnv, ifnotfound = NULL)
  if (!is.null(inter_pat) && nzchar(inter_pat)) {
    cand <- cand[!grepl(inter_pat, cand, perl = TRUE)]
  }
  
  if (!isTRUE(include_AO)) {
    ao_names <- get0("AO_COMP_NAMES", envir = .GlobalEnv,
                     ifnotfound = c("WindA","WindO","CurrentA","CurrentO","WaveA","WaveO"))
    cand <- setdiff(cand, ao_names)
  }
  
  cand
}

build_transform_candidates <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) < 2L) return(character(0))
  nm <- names(df)
  id_name <- nm[1L]
  rv_name <- nm[response_var()]
  
  # Only numeric columns, excluding ID and response
  num_names <- nm[vapply(df, is.numeric, logical(1))]
  candidates <- setdiff(num_names, c(id_name, rv_name))
  
  # Exclude transformed columns, but KEEP interactions
  is_transformed <- function(nm_vec) {
    tp <- get0("TRANS_PREFIXES", envir = .GlobalEnv, ifnotfound = character(0))
    if (!length(tp)) return(rep(FALSE, length(nm_vec)))
    pat <- sprintf("^(%s)", paste(escape_regex(tp), collapse = "|"))
    grepl(pat, nm_vec, perl = TRUE)
  }
  candidates <- candidates[!is_transformed(candidates)]
  candidates
}

get_interaction_candidates <- function(df, include_AO = TRUE) {
  nm <- names(df)
  id_name <- nm[1L]
  rv_name <- nm[response_var()]
  
  # Start with numeric columns, excluding ID/response
  cand <- nm[vapply(df, is.numeric, logical(1))]
  cand <- setdiff(cand, c(id_name, rv_name))
  
  # Exclude existing interactions (prefix-based)
  inter_pat <- get0("INTER_PATTERN", envir = .GlobalEnv, ifnotfound = NULL)
  if (!is.null(inter_pat) && nzchar(inter_pat)) {
    cand <- cand[!grepl(inter_pat, cand, perl = TRUE)]
  }
  
  # Exclude legacy star-named interactions (e.g., "FeatA*FeatB")
  cand <- cand[!grepl("\\*", cand, perl = TRUE)]
  
  # Optionally exclude A/O components (default: keep them)
  if (!isTRUE(include_AO)) {
    ao_names <- get0("AO_COMP_NAMES", envir = .GlobalEnv,
                     ifnotfound = c("WindA","WindO","CurrentA","CurrentO","WaveA","WaveO"))
    cand <- setdiff(cand, ao_names)
  }
  
  cand
}

is_transformed <- function(nm) grepl(TRANS_PATTERN, nm, perl = TRUE)

base_name      <- function(nm) sub(TRANS_PATTERN, "", nm, perl = TRUE)

get_prefix     <- function(nm) regmatches(nm, regexpr(TRANS_PATTERN, nm, perl = TRUE))

make_name      <- function(kind, feat) paste0(prefix_map[[kind]], feat)

compute_transform <- function(v, kind, base = NULL) {
  out <- rep(NA_real_, length(v))
  if (!is.numeric(v)) return(out)
  
  # Normalize kind aliases
  k <- kind
  if (identical(k, "Square Root")) k <- "Sqrt"
  if (identical(k, "Quad Root"))   k <- "Qdrt"
  if (identical(k, "Polynomial"))  k <- "Poly"
  
  if (k == "Log10") {
    idx_pos  <- which(is.finite(v) & v > 0)
    idx_neg  <- which(is.finite(v) & v < 0)
    idx_zero <- which(is.finite(v) & v == 0)
    if (length(idx_pos))  out[idx_pos]  <- log10(v[idx_pos])
    if (length(idx_neg))  out[idx_neg]  <- -log10(abs(v[idx_neg]))
    if (length(idx_zero)) out[idx_zero] <- 0
    
  } else if (k == "Inverse") {
    v2 <- v
    idx_fin <- is.finite(v2)
    idx_nz  <- which(idx_fin & v2 != 0)
    if (length(idx_nz)) {
      min_nz <- min(abs(v2[idx_nz]), na.rm = TRUE)
      idx0   <- which(idx_fin & v2 == 0)
      if (length(idx0)) v2[idx0] <- 0.5 * min_nz
      idx_inv <- which(is.finite(v2) & v2 != 0)
      if (length(idx_inv)) out[idx_inv] <- 1 / v2[idx_inv]
    }
    
  } else if (k == "Square") {
    out <- v^2
    
  } else if (k == "Sqrt") {
    idx_fin <- is.finite(v)
    out[idx_fin] <- sign(v[idx_fin]) * sqrt(abs(v[idx_fin]))
    
  } else if (k == "Qdrt") {
    idx_fin <- is.finite(v)
    out[idx_fin] <- sign(v[idx_fin]) * (abs(v[idx_fin])^(1/4))
    
  } else if (k == "Poly") {
    coeff <- if (!is.null(base)) get_poly_coeffs(base) else NULL
    if (!is.null(coeff)) {
      A <- coeff[["A"]]; B <- coeff[["B"]]; C <- coeff[["C"]]
      idx_fin <- is.finite(v)
      out[idx_fin] <- A + B * v[idx_fin] + C * (v[idx_fin]^2)
    } else {
      out <- v^2
    }
  }
  out
}

materialize_model_features <- function(df_ui, model_features, rv_ao_map, bo_deg) {
  if (model_expects_AO(model_features)) {
    ao <- compute_AO(df_ui, rv_ao_map, bo_deg)
    for (nm in names(ao)) df_ui[[nm]] <- ao[[nm]]
  }
  
  for (tf in model_features) {
    if (is_transformed(tf)) {
      base <- base_name(tf)
      kind <- PREFIX_KIND[[get_prefix(tf)]]
      if (!base %in% names(df_ui)) {
        warning(sprintf("Base feature '%s' not found for transformed feature '%s'", base, tf))
        df_ui[[tf]] <- NA_real_
      } else {
        df_ui[[tf]] <- compute_transform(df_ui[[base]], kind, base = base)
      }
    }
  }
  df_ui
}

apply_transform_selections <- function(df, selection, column_props, ignored_rows = NULL) {
  nm <- names(df)
  
  sel_map <- split(selection$kind, selection$feat)
  sel_map <- lapply(sel_map, function(kinds) {
    kind <- unique(kinds)
    if (length(kind) > 1) kind <- kind[1]
    if (identical(kind, "None")) NULL else kind
  })
  
  find_existing_trans <- function(base) {
    idx <- which(is_transformed(nm))
    if (!length(idx)) return(NULL)
    tf_cols <- nm[idx]
    tf_cols <- tf_cols[base_name(tf_cols) == base]
    if (length(tf_cols)) tf_cols[1] else NULL
  }
  
  for (base in names(sel_map)) {
    kind <- sel_map[[base]]
    existing <- find_existing_trans(base)
    has_existing <- !is.null(existing)
    
    if (is.null(kind)) {
      # Removing any transform; also remove persisted poly coeffs for this base
      if (has_existing) {
        df[[existing]] <- NULL
        nm <- names(df)
        if (!is.null(column_props)) .del(column_props, keys = existing)
      }
      del_poly_coeffs(base)
      next
    }
    
    new_name <- make_name(kind, base)
    
    # Compute new column; if Polynomial, fit and persist coefficients
    if (kind == "Polynomial") {
      coeff <- fit_poly_for_feature(base, ignored_rows = ignored_rows)
      if (!is.null(coeff)) set_poly_coeffs(base, coeff[["A"]], coeff[["B"]], coeff[["C"]])
      new_col <- compute_transform(df[[base]], "Poly", base = base)
    } else {
      # Non-polynomial transforms
      new_col <- compute_transform(df[[base]], kind)
      # If replacing Polynomial, delete any stale coeffs
      if (!is.null(get_poly_coeffs(base))) del_poly_coeffs(base)
    }
    
    if (has_existing) {
      at <- match(existing, nm)
      df[[existing]] <- NULL
      nm <- names(df)
      left <- df[seq_len(at - 1)]
      right <- df[seq(at, length(df))]
      df <- c(left, setNames(list(new_col), new_name), right)
      nm <- names(df)
      if (!is.null(column_props)) {
        .del(column_props, keys = existing)
        .set(column_props, keys = new_name, values = 2)
      }
    } else {
      at <- match(base, nm)
      left <- df[seq_len(at)]
      right <- df[seq(at + 1, length(df))]
      df <- c(left, setNames(list(new_col), new_name), right)
      nm <- names(df)
      if (!is.null(column_props)) .set(column_props, keys = new_name, values = 2)
    }
  }
  
  df
}

save_state <- function(path) {
  state <- list(
    PCA_scaling_mean = PCA_scaling_mean(),
    PCA_scaling_sd   = PCA_scaling_sd(),
    rv_ao_map        = reactiveValuesToList(rv_ao_map),
    rv_pred          = { s <- reactiveValuesToList(rv_pred); s$pending <- NULL; s },
    poly_coeffs      = as.list(POLY_COEFFS)  # base -> c(A,B,C)
  )
  saveRDS(state, path)
}

load_state <- function(path) {
  state <- readRDS(path)
  for (nm in names(state$rv_ao_map)) rv_ao_map[[nm]] <- state$rv_ao_map[[nm]]
  for (nm in names(state$rv_pred))   rv_pred[[nm]]   <- state$rv_pred[[nm]]
  # Restore polynomial coefficients
  if (!is.null(state$poly_coeffs)) {
    rm(list = ls(envir = POLY_COEFFS), envir = POLY_COEFFS)
    for (feat in names(state$poly_coeffs)) {
      vals <- state$poly_coeffs[[feat]]
      if (is.numeric(vals) && length(vals) == 3L) set_poly_coeffs(feat, vals[1], vals[2], vals[3])
    }
  }
}

escape_regex = function(x) {
  # 1) Escape backslashes
  x <- gsub("\\\\", "\\\\\\\\", x, perl = TRUE)
  # 2) Escape regex metacharacters: . ^ $ | ( ) [ ] { } * + ?
  x <- gsub("([.\\^$|()\\[\\]{}*+?])", "\\\\\\1", x, perl = TRUE)
  x
}

is_interaction <- function(nm) grepl(INTER_PATTERN, nm, perl = TRUE)

# is_derived  <- function(nm) grepl(DERIVED_PATTERN, nm, perl = TRUE)

make_inter_name <- function(a, b) {
  paste0(INTER_PREFIX, a, INTER_SEP, b)
}

parse_inter_bases <- function(nm) {
  # returns c(a, b) for "Int..A__B"
  core <- sub(INTER_PATTERN, "", nm, perl = TRUE)
  strsplit(core, INTER_SEP, fixed = TRUE)[[1]]
}

format_pval <- function(p) {
  ifelse(
    is.na(p),
    NA_character_,
    ifelse(
      p < 1e-4,
      "<0.0001",
      formatC(p, format = "fg", digits = 4)  # 4 significant digits, non-scientific
    )
  )
}

set_poly_coeffs <- function(feat, A, B, C) {
  POLY_COEFFS[[feat]] <- c(A = as.numeric(A), B = as.numeric(B), C = as.numeric(C))
}

get_poly_coeffs <- function(feat) {
  val <- POLY_COEFFS[[feat]]
  if (is.null(val) || !is.numeric(val) || length(val) != 3L) return(NULL)
  val
}

del_poly_coeffs <- function(feat) {
  if (!is.null(POLY_COEFFS[[feat]])) rm(list = feat, envir = POLY_COEFFS)
}

# Fit A, B, C for a single feature using current_data() and response_var()
fit_poly_for_feature <- function(base_feat, ignored_rows = NULL) {
  df <- current_data()
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  
  rv_name <- colnames(df)[response_var()]
  if (!is.numeric(df[[base_feat]]) || !is.numeric(df[[rv_name]])) return(NULL)
  
  rows <- seq_len(nrow(df))
  if (!is.null(ignored_rows) && length(ignored_rows) > 0) rows <- setdiff(rows, ignored_rows)
  
  x <- df[[base_feat]][rows]
  y <- df[[rv_name]][rows]
  mask <- is.finite(x) & is.finite(y)
  
  if (sum(mask) < 3 || sd(x[mask]) == 0 || sd(y[mask]) == 0) return(NULL)
  
  fit <- try(stats::lm(y[mask] ~ x[mask] + I((x[mask])^2)), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  
  cf <- stats::coef(fit)
  # Ensure coefficients named (Intercept), x[mask], I((x[mask])^2)
  A <- unname(cf[1])
  B <- unname(cf[2])
  C <- unname(cf[3])
  c(A = A, B = B, C = C)
}