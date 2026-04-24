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

parse_id_any_datetime_minute <- function(x, tz = Sys.timezone(), excel_ext = NULL, locale = "C") {
  # Already POSIXct -> floor to minute
  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(lubridate::floor_date(x, unit = "minute"))
  }
  # Already Date -> coerce to POSIXct at midnight
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = tz))
  }
  # Excel numeric serials (xlsx) -> convert using Excel origin
  if (is.numeric(x) && (is.null(excel_ext) || identical(excel_ext, "xlsx"))) {
    px <- as.POSIXct(x * 86400, origin = "1899-12-30", tz = tz)
    return(lubridate::floor_date(px, unit = "minute"))
  }
  
  # Character inputs: normalize whitespace
  chr <- gsub("[[:space:]]+", " ", trimws(as.character(x)))
  
  # Broad lubridate orders: MDY, YMD, DMY; 4-digit and 2-digit years; 12/24h; optional seconds
  orders <- c(
    # MDY (month/day/year)
    "mdYIMSp","mdYIMp","mdYHMS","mdYHM","mdY",
    "mdyIMSp","mdyIMp","mdyHMS","mdyHM","mdy",
    # YMD (year-month-day)
    "YmdIMSp","YmdIMp","YmdHMS","YmdHM","Ymd",
    "ymdIMSp","ymdIMp","ymdHMS","ymdHM","ymd",
    # DMY (day-month-year)
    "dmYIMSp","dmYIMp","dmYHMS","dmYHM","dmY",
    "dmyIMSp","dmyIMp","dmyHMS","dmyHM","dmy"
  )
  
  px <- suppressWarnings(
    lubridate::parse_date_time(chr, orders = orders, truncated = 1, exact = TRUE, tz = tz, locale = locale)
  )
  
  # Fallback with explicit base::strptime formats for remaining NAs, including month names
  na_idx <- which(is.na(px))
  if (length(na_idx) > 0L) {
    old_loc <- tryCatch(Sys.getlocale("LC_TIME"), error = function(e) NA_character_)
    if (!is.na(old_loc)) on.exit(try(Sys.setlocale("LC_TIME", old_loc), silent = TRUE), add = TRUE)
    try(Sys.setlocale("LC_TIME", "C"), silent = TRUE)
    
    fmts <- c(
      # MDY with slash/dash, 4-digit year, 12/24h, with/without seconds
      "%m/%d/%Y %I:%M:%S %p","%m/%d/%Y %I:%M %p","%m/%d/%Y %H:%M:%S","%m/%d/%Y %H:%M","%m/%d/%Y",
      "%m-%d-%Y %I:%M:%S %p","%m-%d-%Y %I:%M %p","%m-%d-%Y %H:%M:%S","%m-%d-%Y %H:%M","%m-%d-%Y",
      # MDY 2-digit year
      "%m/%d/%y %I:%M:%S %p","%m/%d/%y %I:%M %p","%m/%d/%y %H:%M:%S","%m/%d/%y %H:%M","%m/%d/%y",
      "%m-%d-%y %I:%M:%S %p","%m-%d-%y %I:%M %p","%m-%d-%y %H:%M:%S","%m-%d-%y %H:%M","%m-%d-%y",
      # YMD with slash/dash
      "%Y/%m/%d %I:%M:%S %p","%Y/%m/%d %I:%M %p","%Y/%m/%d %H:%M:%S","%Y/%m/%d %H:%M","%Y/%m/%d",
      "%Y-%m-%d %I:%M:%S %p","%Y-%m-%d %I:%M %p","%Y-%m-%d %H:%M:%S","%Y-%m-%d %H:%M","%Y-%m-%d",
      # DMY with slash/dash
      "%d/%m/%Y %I:%M:%S %p","%d/%m/%Y %I:%M %p","%d/%m/%Y %H:%M:%S","%d/%m/%Y %H:%M","%d/%m/%Y",
      "%d-%m-%Y %I:%M:%S %p","%d-%m-%Y %I:%M %p","%d-%m-%Y %H:%M:%S","%d-%m-%Y %H:%M","%d-%m-%Y",
      # Month-name variants
      "%b %d, %Y %H:%M:%S","%b %d, %Y %H:%M","%b %d, %Y",
      "%B %d, %Y %H:%M:%S","%B %d, %Y %H:%M","%B %d, %Y"
    )
    
    for (fmt in fmts) {
      remaining <- na_idx[is.na(px[na_idx])]
      if (!length(remaining)) break
      parsed <- as.POSIXct(chr[remaining], format = fmt, tz = tz)
      ok <- !is.na(parsed)
      if (any(ok)) px[remaining][ok] <- parsed[ok]
    }
  }
  
  lubridate::floor_date(px, unit = "minute")
}

# Renders a modeling/prediction DT with consistent options and a wide, non-wrapping ID column.
model_dt <- function(df,
                     date_format_string,
                     num_rows_per_page,
                     id_col = 1,
                     id_width = "180px",
                     tz = Sys.timezone()) {
  if (is.null(df)) return(NULL)
  
  # Format ID as "M/D/YYYY HH:MM" only when converted from numeric Excel serials
  if (ncol(df) >= id_col && identical(date_format_string, "Date_MDY_HM")) {
    df[[id_col]] <- format_id_MDY_HM(df[[id_col]], tz = tz)
  }
  
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
        list(width = id_width, targets = 0)  # widen first column
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
        "}"
      )
    )
  ) %>%
    DT::formatStyle(id_col, `white-space` = "nowrap", `min-width` = id_width)
}

# Format a vector to "M/D/YYYY HH:MM" for display
format_id_MDY_HM <- function(x, tz = Sys.timezone()) {
  dt <- x
  if (inherits(dt, "Date")) {
    dt <- as.POSIXct(dt, tz = tz)
  } else if (inherits(dt, c("POSIXct", "POSIXt"))) {
    # keep
  } else if (is.numeric(dt)) {
    # Treat numeric as Excel serial days (Windows origin)
    dt <- as.POSIXct(dt * 86400, origin = "1899-12-30", tz = tz)
  } else if (is.character(dt)) {
    dt <- suppressWarnings(as.POSIXct(dt, tz = tz))
  } else {
    return(as.character(x))
  }
  dt <- lubridate::floor_date(dt, unit = "minute")
  y  <- format(dt, "%Y")
  m  <- as.integer(format(dt, "%m"))
  d  <- as.integer(format(dt, "%d"))
  hm <- format(dt, "%H:%M")
  sprintf("%d/%d/%s %s", m, d, y, hm)
}

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

# Fit A, B, C for a single feature using current_data() and response_var()
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