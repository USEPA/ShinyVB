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

db_snapshot <- function(conn) {
  ok <- FALSE
  try(ok <- DBI::dbIsValid(conn), silent = TRUE)
  if (!isTRUE(ok)) return(NULL)
  tabs <- DBI::dbListTables(conn)
  setNames(lapply(tabs, function(t) DBI::dbReadTable(conn, t)), tabs)
}

do_save <- function(save_type, fname, temp_db = NULL, session, extras = list()) {
  
  tmp <- tempfile(fileext = ".RData")
  
  save_list <- c(
    list(
      type         = save_type,
      Version      = version,
      temp_db      = NULL,
      temp_db_dump = db_snapshot(temp_db)
    ),
    extras  # everything else you gathered in server
  )
  
  save(save_list, file = tmp)
  
  alias <- paste0("download_", basename(tmp))
  shiny::addResourcePath(alias, dirname(tmp))
  
  port <- session$clientData$url_port
  port <- if (!is.null(port) && nzchar(port)) paste0(":", port) else ""
  
  url <- paste0(
    session$clientData$url_protocol, "//",
    session$clientData$url_hostname, port, "/",
    alias, "/", basename(tmp)
  )
  
  session$sendCustomMessage("download", list(filename = fname, url = url))
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

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

`%||%` <- function(a, b) {
  if (is_string(a)) return(a)
  if (!is.null(a))  return(a)  # preserve non-character values (numeric, logical, list, etc.)
  b
}

read_input_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    df <- openxlsx::read.xlsx(path, check.names = FALSE)
  } else {
    df <- data.table::fread(path, sep = "auto", data.table = FALSE, showProgress = FALSE)
  }
  df[] <- lapply(df, function(col) if (is.factor(col)) as.character(col) else col)
  df
}

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

# Renders a modeling/prediction DT with consistent options and wide, non-wrapping ID
model_dt <- function(df,num_rows_per_page,id_width = "180px") {
  if (is.null(df)) return(NULL)
  
  df[[1]] = as.character(df[[1]])
  
  DT::datatable(
    df,
    rownames   = FALSE,
    selection  = list(
      selected = list(rows = NULL, cols = NULL),
      target   = "row",
      mode     = "single"
    ),
    editable   = FALSE,
    extensions = "Buttons",
    options = list(
      autoWidth   = FALSE,
      dom         = "ltBp",
      buttons     = c("copy", "csv", "excel"),
      paging      = TRUE,
      pageLength  = num_rows_per_page,
      scrollX     = TRUE,
      scrollY     = TRUE,
      columnDefs  = list(
        list(className = "dt-center", orderable = TRUE, targets = "_all"),
        list(width = id_width, targets = 0)
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
        "}"
      )
    )
  ) %>%
    DT::formatStyle(1, `white-space` = "nowrap", `min-width` = id_width)
}

build_interactions <- function(df,response_idx,ignored,threshold,rv_inter,output,session,navigate = FALSE) {
  # Validate data
  if (is.null(df) || !is.data.frame(df) || ncol(df) < 3L) {
    rv_inter$table <- NULL
    output$interactions_table <- DT::renderDataTable(server = FALSE, {
      DT::datatable(
        data.frame(Message = "No data available."),
        rownames = FALSE, selection = "none",
        options = list(dom = "t", paging = FALSE)
      )
    })
    return(invisible(NULL))
  }
  
  # Drop ignored rows (logical mask or integer indices)
  if (!is.null(ignored) && length(ignored) > 0) {
    if (is.logical(ignored) && length(ignored) == nrow(df)) {
      df <- df[!ignored, , drop = FALSE]
    } else {
      drop_idx <- as.integer(ignored)
      keep <- setdiff(seq_len(nrow(df)), drop_idx)
      df <- df[keep, , drop = FALSE]
    }
  }
  
  # Response column
  rv_name <- if (is.numeric(response_idx)) colnames(df)[response_idx] else as.character(response_idx)
  y <- suppressWarnings(as.numeric(df[[rv_name]]))
  
  # Candidate features (excludes ID/response/derived; includes A/O as you specified)
  base_feats <- get_interaction_candidates(df, include_AO = TRUE)
  base_feats <- setdiff(base_feats, rv_name)
  if (length(base_feats) < 2L) {
    rv_inter$table <- NULL
    output$interactions_table <- DT::renderDataTable(server = FALSE, {
      DT::datatable(
        data.frame(Message = "Not enough numeric features to compute interactions."),
        rownames = FALSE, selection = "none",
        options = list(dom = "t", paging = FALSE)
      )
    })
    return(invisible(NULL))
  }
  
  thr <- if (is.null(threshold) || !is.finite(threshold)) 0.7 else threshold
  
  # Compute correlations for pairs i < j
  res <- vector("list", length = 0L)
  k <- 1L
  for (ii in seq_len(length(base_feats) - 1L)) {
    a <- base_feats[ii]
    xa <- suppressWarnings(as.numeric(df[[a]]))
    for (jj in (ii + 1L):length(base_feats)) {
      b <- base_feats[jj]
      xb <- suppressWarnings(as.numeric(df[[b]]))
      prod <- xa * xb
      mask <- is.finite(prod) & is.finite(y)
      if (sum(mask) >= 3 && stats::sd(prod[mask]) > 0 && stats::sd(y[mask]) > 0) {
        ct <- suppressWarnings(stats::cor.test(prod[mask], y[mask], method = "pearson"))
        r  <- unname(ct$estimate)
        p  <- unname(ct$p.value)
        if (is.finite(r) && abs(r) >= thr) {
          res[[k]] <- data.frame(
            Feat1 = a, Feat2 = b,
            Correlation = as.numeric(r),
            p_val = as.numeric(p),
            stringsAsFactors = FALSE
          )
          k <- k + 1L
        }
      }
    }
  }
  
  inter_tbl <- if (length(res)) do.call(rbind, res) else NULL
  rv_inter$table <- inter_tbl
  
  output$interactions_table <- DT::renderDataTable(server = FALSE, {
    if (is.null(inter_tbl) || nrow(inter_tbl) == 0) {
      DT::datatable(
        data.frame(Message = "No interactions exceed the threshold."),
        rownames = FALSE, selection = "none",
        options = list(dom = "t", paging = FALSE)
      )
    } else {
      inter_tbl$Correlation <- round(inter_tbl$Correlation, 4)
      inter_tbl$p_Value <- ifelse(
        is.na(inter_tbl$p_val),
        NA_character_,
        ifelse(inter_tbl$p_val < 1e-4, "<0.0001",
               formatC(inter_tbl$p_val, format = "fg", digits = 4))
      )
      DT::datatable(
        inter_tbl[, c("Feat1", "Feat2", "Correlation", "p_Value")],
        rownames = FALSE,
        selection = "multiple",
        options = list(
          autoWidth  = FALSE,
          dom        = "tip",
          paging     = TRUE,
          pageLength = 50,
          ordering   = FALSE,
          scrollX    = TRUE,
          columnDefs = list(list(targets = 0:3, className = "dt-center"))
        )
      )
    }
  })
  
  if (isTRUE(navigate)) {
    updateTabsetPanel(session, inputId = "shinyVB",  selected = "Data")
    updateTabsetPanel(session, inputId = "data_tabs", selected = "Interactions")
  }
  
  invisible(inter_tbl)
}

clear_trans_table <- function(drop_transforms= TRUE,drop_interactions = TRUE,drop_AO= FALSE,column_props= NULL) {
  
  if (is.null(column_props)) {column_props <- get0("column_props", envir = .GlobalEnv, ifnotfound = NULL)}
  
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

# Collapse whitespace and trim (handles NBSP too)
clean_names <- function(n) {
  n <- gsub("[[:space:]\u00A0]+", " ", n)
  trimws(n)
}

get_inter_components <- function(term) {
  s <- sub(INTER_PATTERN, "", term, perl = TRUE)
  strsplit(s, INTER_SEP, fixed = TRUE)[[1]]
}
get_base <- function(x) {
  if (grepl(TRANS_PATTERN, x, perl = TRUE)) {
    if (exists("base_name", mode = "function")) base_name(x) else sub(TRANS_PATTERN, "", x, perl = TRUE)
  } else x
}

# Compute the RAW columns required to show in the prediction UI
compute_ui_required <- function(model_features, rv_ao_map = NULL) {
  # Ensure a clean character vector of model feature names
  cols <- unique(as.character(model_features))
  cols <- clean_names(cols)
  
  # AO component names (canonical)
  ao_comp <- get0("AO_COMP_NAMES", envir = .GlobalEnv,
                  ifnotfound = c("WindA","WindO","CurrentA","CurrentO","WaveA","WaveO"))
  
  # Canonical raw names from training (fallbacks if not yet defined)
  ao_map <- .get_ao_map(rv_ao_map)
  ao_raw <- list(
    wind    = c(ao_map$wind_speed   %||% "Wind Speed",
                ao_map$wind_dir     %||% "Wind Direction"),
    current = c(ao_map$current_speed%||% "Current Speed",
                ao_map$current_dir  %||% "Current Direction"),
    wave    = c(ao_map$wave_height  %||% "Wave Height",
                ao_map$wave_dir     %||% "Wave Direction")
  )
  
  # Start with non-transformed, non-interaction, non-A/O features
  ui_required <- cols[
    !grepl(get0("TRANS_PATTERN", envir = .GlobalEnv, ifnotfound = ""), cols, perl = TRUE) &
      !grepl(get0("INTER_PATTERN", envir = .GlobalEnv, ifnotfound = ""), cols, perl = TRUE) &
      !(cols %in% ao_comp)
  ]
  
  # Add bases for transformed features
  trans_in_model <- cols[grepl(get0("TRANS_PATTERN", envir = .GlobalEnv, ifnotfound = ""), cols, perl = TRUE)]
  if (length(trans_in_model)) {
    bases <- vapply(trans_in_model, base_name, FUN.VALUE = character(1))
    ui_required <- c(ui_required, bases)
  }
  
  # If the model directly includes AO components, include their raw inputs
  if (any(cols %in% c("WindA","WindO")))    ui_required <- c(ui_required, ao_raw$wind)
  if (any(cols %in% c("CurrentA","CurrentO"))) ui_required <- c(ui_required, ao_raw$current)
  if (any(cols %in% c("WaveA","WaveO")))    ui_required <- c(ui_required, ao_raw$wave)
  
  # Add bases for interaction terms; for AO components add raw inputs, otherwise add the base
  inter_terms <- cols[grepl(get0("INTER_PATTERN", envir = .GlobalEnv, ifnotfound = ""), cols, perl = TRUE)]
  if (length(inter_terms)) {
    for (t in inter_terms) {
      parts <- parse_inter_bases(t)  # returns c(a, b)
      for (p in parts) {
        b <- base_name(p)  # remove any transform prefixes from the interaction component
        if (b %in% c("WindA","WindO")) {
          ui_required <- c(ui_required, ao_raw$wind)
        } else if (b %in% c("CurrentA","CurrentO")) {
          ui_required <- c(ui_required, ao_raw$current)
        } else if (b %in% c("WaveA","WaveO")) {
          ui_required <- c(ui_required, ao_raw$wave)
        } else {
          ui_required <- c(ui_required, b)
        }
      }
    }
  }
  
  # De-duplicate and normalize
  ui_required <- unique(ui_required)
  clean_names(ui_required)
}
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

is_transformed <- function(nm) grepl(TRANS_PATTERN, nm, perl = TRUE)
base_name      <- function(nm) sub(TRANS_PATTERN, "", nm, perl = TRUE)
get_prefix     <- function(nm) regmatches(nm, regexpr(TRANS_PATTERN, nm, perl = TRUE))
make_name      <- function(kind, feat) paste0(prefix_map[[kind]], feat)

# Continue SampleID numbering for newly appended rows
# vec: character ID vector
# from, to: indices (1-based) to fill (inclusive)
assign_sample_ids <- function(vec, from, to) {
  if (from > to) return(vec)
  rng <- from:to
  # Look at existing IDs (before 'from') and find the highest SampleID<number>
  existing <- vec[seq_len(max(from - 1L, 0L))]
  m <- regmatches(existing, regexpr("^SampleID(\\d+)$", existing))
  nums <- suppressWarnings(as.integer(sub("^SampleID(\\d+)$", "\\1", m)))
  start <- if (length(nums) && any(!is.na(nums))) max(nums, na.rm = TRUE) + 1L else from
  vec[rng] <- sprintf("SampleID%d", seq_len(length(rng)) + start - 1L)
  vec
}

# Build a blank prediction table with placeholders and the correct ID seeding per mode
# ui_required: character vector of RAW feature names to show in the UI
build_blank_pred_table <- function(ui_required, rv_name, n, id_mode, base_date = get_base_date()) {
  stopifnot(length(rv_name) == 1L)
  n <- max(1L, as.integer(n))
  temp <- data.frame(matrix(-999, nrow = n, ncol = length(ui_required) + 6), check.names = FALSE)
  colnames(temp) <- c("Sample_ID", rv_name, ui_required,
                      "Prediction", "Lower_Bound", "Upper_Bound", "Outcome")
  
  # Placeholders
  idx_resp       <- 2L
  idx_pred_end   <- ncol(temp)
  idx_pred_start <- idx_pred_end - 3L
  idx_feat_start <- 3L
  idx_feat_end   <- idx_pred_start - 1L
  
  temp[[idx_resp]] <- -999
  if (idx_feat_end >= idx_feat_start) {
    for (j in idx_feat_start:idx_feat_end) temp[[j]] <- -999
  }
  for (j in idx_pred_start:idx_pred_end) temp[[j]] <- -999
  
  # Seed ID per mode
  if (identical(id_mode, "Character")) {
    temp[[1]] <- sprintf("SampleID%d", seq_len(nrow(temp)))
  } else if (identical(id_mode, "Numeric")) {
    temp[[1]] <- as.numeric(seq_len(nrow(temp)))
  } else { # "Date"
    temp[[1]] <- seq.Date(from = base_date, by = "day", length.out = nrow(temp))
  }
  
  # Normalize storage (collapses midnight POSIXct to Date if needed)
  ensure_id_type(temp, idx_id = 1L, id_mode = id_mode)
}

# Make appended rows with class-aware NA and -999 placeholders (ID assigned later)
make_new_rows_pred <- function(df, n_rows) {
  n_rows <- as.integer(n_rows)
  stopifnot(n_rows > 0L)
  idx_pred_end   <- ncol(df)
  idx_pred_start <- idx_pred_end - 3L
  idx_feat_start <- 3L
  idx_feat_end   <- idx_pred_start - 1L
  idx_resp       <- 2L
  
  new_rows <- as.data.frame(lapply(df, function(col) {
    if (inherits(col, c("POSIXct", "POSIXt"))) rep(as.POSIXct(NA), n_rows)
    else if (is.numeric(col)) rep(NA_real_, n_rows)
    else if (is.character(col)) rep(NA_character_, n_rows)
    else rep(NA, n_rows)
  }), check.names = FALSE)
  
  if (idx_feat_end >= idx_feat_start) {
    for (j in idx_feat_start:idx_feat_end) new_rows[[j]] <- rep(-999, n_rows)
  }
  new_rows[[idx_resp]] <- rep(-999, n_rows)
  for (j in idx_pred_start:idx_pred_end) new_rows[[j]] <- rep(-999, n_rows)
  
  new_rows
}

# SampleIDn template detector
is_blank_sampleid_template <- function(df, idx_id = 1L) {
  id <- df[[idx_id]]
  is.character(id) && length(id) > 0 && all(grepl("^SampleID\\d+$", id))
}

# Per-model getters/setters to de-duplicate switch(..)
get_pred_table <- function(model) {
  switch(model,
         "Logistic_Regression" = LG_pred_table_data(),
         "XGB_Classifier"      = XGBCL_pred_table_data(),
         "XGBoost"             = XGB_pred_table_data(),
         "Elastic_Net"         = EN_pred_table_data(),
         NULL)
}
set_pred_table <- function(model, df, sync_display = TRUE) {
  switch(model,
         "Logistic_Regression" = LG_pred_table_data(df),
         "XGB_Classifier"      = XGBCL_pred_table_data(df),
         "XGBoost"             = XGB_pred_table_data(df),
         "Elastic_Net"         = EN_pred_table_data(df))
  if (isTRUE(sync_display)) pred_table_data(df)
}

canonicalize_kind <- function(kind) {
  # Normalize to 1-length string
  if (length(kind) != 1L) kind <- kind[1L]
  k <- as.character(kind)
  if (is.na(k)) return(NA_character_)
  k <- trimws(k)
  
  # If a transform prefix (e.g., "Poly.."), map via PREFIX_KIND when available
  PREFIX_KIND <- get0("PREFIX_KIND", ifnotfound = NULL, envir = .GlobalEnv)
  if (!is.null(PREFIX_KIND) && k %in% names(PREFIX_KIND)) {
    return(PREFIX_KIND[[k]])
  }
  
  # Case-insensitive synonyms
  kl <- tolower(gsub("\\s+", " ", k))
  map <- c(
    "log10" = "Log10", "log" = "Log10", "log 10" = "Log10",
    "inverse" = "Inverse", "inv" = "Inverse", "1/x" = "Inverse",
    "square" = "Square", "x^2" = "Square", "power2" = "Square",
    "square root" = "Square Root", "sqrt" = "Square Root",
    "quad root" = "Quad Root", "qdrt" = "Quad Root", "fourth root" = "Quad Root",
    "polynomial" = "Polynomial", "poly" = "Polynomial", "poly()" = "Polynomial"
  )
  if (kl %in% names(map)) return(map[[kl]])
  
  # If a known prefix set exists but PREFIX_KIND wasn’t available
  TRANS_PREFIXES <- get0("TRANS_PREFIXES",
                         ifnotfound = c("Log..","Inverse..","Square..","Sqrt..","Qdrt..","Poly.."),
                         envir = .GlobalEnv)
  if (k %in% TRANS_PREFIXES) {
    # Fallback mapping for prefixes
    fallback_pk <- c(
      "Log.." = "Log10", "Inverse.." = "Inverse", "Square.." = "Square",
      "Sqrt.." = "Square Root", "Qdrt.." = "Quad Root", "Poly.." = "Polynomial"
    )
    return(fallback_pk[[k]])
  }
  
  # Leave as-is (the caller will error if not recognized)
  k
}

compute_transform <- function(v, kind, base = NULL) {
  v <- as.numeric(v)
  kind <- canonicalize_kind(kind)
  
  if (identical(kind, "Log10")) {
    out <- v
    pos <- v > 0; neg <- v < 0
    out[pos] <- log10(v[pos])
    out[neg] <- -log10(abs(v[neg]))
    out[v == 0] <- 0
    return(out)
  }
  
  if (identical(kind, "Inverse")) {
    vv <- v
    nz <- vv != 0 & is.finite(vv)
    min_nonzero <- suppressWarnings(min(abs(vv[nz]), na.rm = TRUE))
    if (!is.finite(min_nonzero)) min_nonzero <- 1
    vv[!nz] <- 0.5 * min_nonzero
    out <- 1 / vv
    out[!is.finite(out)] <- NA_real_
    return(out)
  }
  
  if (identical(kind, "Square")) {
    return(v^2)
  }
  
  if (identical(kind, "Square Root")) {
    return(sign(v) * sqrt(abs(v)))
  }
  
  if (identical(kind, "Quad Root")) {
    return(sign(v) * (abs(v)^(1/4)))
  }
  
  if (identical(kind, "Polynomial")) {
    # Require a base to look up coefficients; otherwise default to v^2
    if (is.null(base) || !nzchar(base)) return(v^2)
    
    co <- if (exists("get_poly_coeffs", mode = "function")) get_poly_coeffs(base) else NULL
    if (is.null(co) || !is.numeric(co) || length(co) < 3L) {
      # Fallback if coeffs weren’t persisted or malformed
      return(v^2)
    }
    
    # Extract A/B/C robustly for both named numeric vectors and lists
    get_coef <- function(obj, nm, pos) {
      if (is.list(obj)) {
        val <- obj[[nm]]
      } else {
        if (!is.null(names(obj)) && nm %in% names(obj)) {
          val <- obj[nm]
        } else {
          val <- obj[pos]
        }
      }
      as.numeric(val)
    }
    A <- get_coef(co, "A", 1L)
    B <- get_coef(co, "B", 2L)
    C <- get_coef(co, "C", 3L)
    
    if (any(!is.finite(c(A, B, C)))) {
      # As a last resort, fall back to v^2 to avoid hard failure
      return(v^2)
    }
    return(A + B * v + C * (v^2))
  }
  
  stop("Unknown transform kind: ", kind)
}

# Convert rv_ao_map (reactiveValues, reactive, or list) to a plain list
.get_ao_map <- function(rv_ao_map) {
  if (is.function(rv_ao_map)) rv_ao_map <- rv_ao_map()
  if (inherits(rv_ao_map, "reactivevalues")) return(reactiveValuesToList(rv_ao_map))
  as.list(rv_ao_map)
}

# Robust A/O synthesis that tolerates NA/NULL/"" mappings
compute_AO <- function(df, rv_ao_map, bo_deg) {
  map <- .get_ao_map(rv_ao_map)
  
  has_val <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  
  synth_pair <- function(speed_name, dir_name, A_name, O_name) {
    # Skip if mapping not set
    if (!has_val(speed_name) || !has_val(dir_name)) return(invisible(NULL))
    # Skip if mapped columns do not exist in df
    if (!isTRUE(all(c(speed_name, dir_name) %in% names(df)))) return(invisible(NULL))
    
    S <- suppressWarnings(as.numeric(df[[speed_name]]))  # speed or height
    D <- suppressWarnings(as.numeric(df[[dir_name]]))    # direction (deg)
    theta <- (D - as.numeric(bo_deg)) * pi / 180
    df[[A_name]] <<- -S * cos(theta)
    df[[O_name]] <<-  S * sin(theta)
    invisible(NULL)
  }
  
  # Wind
  synth_pair(map$wind_speed,    map$wind_dir,    "WindA",    "WindO")
  # Current
  synth_pair(map$current_speed, map$current_dir, "CurrentA", "CurrentO")
  # Wave (height substitutes speed)
  synth_pair(map$wave_height,   map$wave_dir,    "WaveA",    "WaveO")
  
  df
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

escape_regex <- function(x) gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x, perl = TRUE)

is_interaction <- function(nm) grepl(INTER_PATTERN, nm, perl = TRUE)

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

set_poly_coeffs <- function(feat, A, B, C) assign(feat, c(A, B, C), envir = POLY_COEFFS)
get_poly_coeffs <- function(feat) get0(feat, envir = POLY_COEFFS, inherits = FALSE)
del_poly_coeffs <- function(feat) if (!is.null(get0(feat, envir = POLY_COEFFS, inherits = FALSE))) rm(list = feat, envir = POLY_COEFFS)
fit_poly_coeffs <- function(x, y, ignore = NULL) {
  x <- as.numeric(x); y <- as.numeric(y)
  mask <- is.finite(x) & is.finite(y)
  if (is.logical(ignore) && length(ignore) == length(mask)) mask <- mask & !ignore
  if (sum(mask) < 3 || sd(x[mask]) == 0 || sd(y[mask]) == 0) return(NULL)
  fit <- try(lm(y[mask] ~ x[mask] + I(x[mask]^2)), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  co <- coef(fit)
  if (length(co) < 3 || any(!is.finite(co[1:3]))) return(NULL)
  c(unname(co[[1]]), unname(co[[2]]), unname(co[[3]]))
}

build_rcorr_results <- function(df_num) {
  # df_num must be all-numeric columns
  if (ncol(df_num) < 2L) {
    return(list(
      mat = matrix(numeric(), 0, 0, dimnames = list(NULL, NULL)),
      tab = data.frame(Feat1 = character(),
                       Feat2 = character(),
                       Correlation = numeric(),
                       `p-Value` = numeric(),
                       check.names = FALSE)
    ))
  }
  res <- suppressWarnings(Hmisc::rcorr(as.matrix(df_num), type = "pearson"))
  r  <- res$r
  p  <- res$P
  
  # upper-triangle pairs only
  idx <- which(upper.tri(r), arr.ind = TRUE)
  tab <- data.frame(
    Feat1       = colnames(r)[idx[, 1]],
    Feat2       = colnames(r)[idx[, 2]],
    Correlation = as.numeric(r[idx]),
    `p-Value`   = as.numeric(p[idx]),
    check.names = FALSE
  )
  # sort by descending |Correlation|, NAs last, then by names for stability
  tab <- tab[order(-abs(tab$Correlation), tab$Feat1, tab$Feat2, na.last = TRUE), ]
  rownames(tab) <- NULL
  
  list(mat = r, tab = tab)
}

normalize_pca_artifacts <- function(mu, sdv, rot) {
  # Ensure named numeric vectors for mean/sd
  if (is.data.frame(mu) || is.matrix(mu)) {
    key <- intersect(colnames(mu), c("Feature","feature","Variable","variable","Name","name","feat"))[1]
    val <- setdiff(colnames(mu), key)[1]
    mu <- setNames(as.numeric(mu[[val]]), as.character(mu[[key]]))
  } else {
    mu <- setNames(as.numeric(mu), names(mu))
  }
  
  if (is.data.frame(sdv) || is.matrix(sdv)) {
    key <- intersect(colnames(sdv), c("Feature","feature","Variable","variable","Name","name","feat"))[1]
    val <- setdiff(colnames(sdv), key)[1]
    sdv <- setNames(as.numeric(sdv[[val]]), as.character(sdv[[key]]))
  } else {
    sdv <- setNames(as.numeric(sdv), names(sdv))
  }
  
  # Coefficients: matrix with rownames = features, colnames = PCs
  if (is.null(rot)) stop("PCA coefficients are NULL")
  if (is.data.frame(rot)) {
    key_candidates <- intersect(names(rot), c("Feature","feature","Variable","variable","Name","name","feat"))
    key <- if (length(key_candidates)) key_candidates[1] else NULL
    if (!is.null(key)) {
      rn <- as.character(rot[[key]])
      rot_num <- rot[, setdiff(names(rot), key), drop = FALSE]
      for (nm in names(rot_num)) rot_num[[nm]] <- suppressWarnings(as.numeric(rot_num[[nm]]))
      rot <- as.matrix(rot_num); rownames(rot) <- rn
    } else {
      rot <- as.matrix(rot)
    }
  } else if (!is.matrix(rot)) {
    rot <- as.matrix(rot)
  }
  
  # Auto‑transpose if features are in colnames instead of rownames
  base_vars <- names(mu)
  rn <- rownames(rot); cn <- colnames(rot)
  if ((is.null(rn) || !any(rn %in% base_vars)) && (!is.null(cn) && any(cn %in% base_vars))) {
    rot <- t(rot)
  }
  
  # Warn if any features are missing
  if (!all(base_vars %in% rownames(rot))) {
    warning("PCA loadings missing rows for: ",
            paste(setdiff(base_vars, rownames(rot)), collapse = ", "))
  }
  
  list(mu = mu, sdv = sdv, rot = rot)
}