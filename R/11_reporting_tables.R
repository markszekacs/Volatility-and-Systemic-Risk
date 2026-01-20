suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(tseries)   # jarque.bera.test, adf.test, kpss.test
  library(moments)   # skewness, kurtosis
})

# ----- helpers -----

# Convert an xts panel to a long data.frame: date, series, value.
xts_to_long <- function(x) {
  if (!xts::is.xts(x)) stop("x must be an xts object.")
  df <- data.frame(date = zoo::index(x), coredata(x), check.names = FALSE)
  long <- stats::reshape(df,
                         varying = colnames(x),
                         v.names = "value",
                         timevar = "series",
                         times = colnames(x),
                         direction = "long")
  rownames(long) <- NULL
  long
}

# Write a data.frame to CSV with directory creation.
write_table_csv <- function(df, file_path) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, file_path, row.names = FALSE)
  invisible(file_path)
}

# ----- summary statistics -----

# Compute summary stats for each column of an xts panel.
summary_stats_xts <- function(x) {
  if (!xts::is.xts(x)) stop("x must be xts.")
  
  out <- lapply(colnames(x), function(nm) {
    v <- as.numeric(x[, nm])
    v <- v[is.finite(v)]
    
    data.frame(
      series = nm,
      n = length(v),
      mean = mean(v),
      sd = stats::sd(v),
      min = min(v),
      p05 = as.numeric(stats::quantile(v, 0.05)),
      median = stats::median(v),
      p95 = as.numeric(stats::quantile(v, 0.95)),
      max = max(v),
      skew = moments::skewness(v),
      kurt = moments::kurtosis(v),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, out)
}

# ----- normality tests -----

# Run Jarque-Bera and Shapiro tests for each series.
# Note: Shapiro-Wilk is not recommended for very large n; we apply it on a subsample if needed.
normality_tests_xts <- function(x, shapiro_max_n = 5000, shapiro_seed = 1) {
  if (!xts::is.xts(x)) stop("x must be xts.")
  
  out <- lapply(colnames(x), function(nm) {
    v <- as.numeric(x[, nm])
    v <- v[is.finite(v)]
    
    jb_p <- NA_real_
    sh_p <- NA_real_
    
    if (length(v) >= 10) {
      jb_p <- tryCatch(tseries::jarque.bera.test(v)$p.value, error = function(e) NA_real_)
    }
    
    if (length(v) >= 3) {
      set.seed(shapiro_seed)
      vv <- if (length(v) > shapiro_max_n) sample(v, shapiro_max_n) else v
      sh_p <- tryCatch(stats::shapiro.test(vv)$p.value, error = function(e) NA_real_)
    }
    
    data.frame(
      series = nm,
      n = length(v),
      jb_pvalue = jb_p,
      shapiro_pvalue = sh_p,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, out)
}

# ----- stationarity tests -----

# Run ADF and KPSS tests for each series.
# - adf.test: null is unit root (non-stationary)
# - kpss.test: null is stationary (level or trend depending on null)
stationarity_tests_xts <- function(x, kpss_null = c("Level", "Trend")) {
  kpss_null <- match.arg(kpss_null)
  if (!xts::is.xts(x)) stop("x must be xts.")
  
  out <- lapply(colnames(x), function(nm) {
    v <- as.numeric(x[, nm])
    v <- v[is.finite(v)]
    
    adf_p <- NA_real_
    kpss_p <- NA_real_
    
    if (length(v) >= 20) {
      adf_p  <- tryCatch(tseries::adf.test(v)$p.value, error = function(e) NA_real_)
      kpss_p <- tryCatch(tseries::kpss.test(v, null = kpss_null)$p.value, error = function(e) NA_real_)
    }
    
    data.frame(
      series = nm,
      n = length(v),
      adf_pvalue = adf_p,
      kpss_pvalue = kpss_p,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, out)
}

# ----- connectedness tables -----

# Build a connectedness table from DY measures.
# Expects a list with measures$TO, measures$FROM, measures$NET, measures$TCI.
connectedness_table <- function(dy_res) {
  if (is.null(dy_res$measures)) stop("dy_res must contain $measures.")
  m <- dy_res$measures
  
  df <- data.frame(
    series = names(m$TO),
    TO = as.numeric(m$TO),
    FROM = as.numeric(m$FROM),
    NET = as.numeric(m$NET),
    stringsAsFactors = FALSE
  )
  
  df <- df[order(-df$NET), ]
  attr(df, "TCI") <- as.numeric(m$TCI)
  df
}

# Build a one-row summary table for TCI and top net transmitters/receivers.
connectedness_summary <- function(conn_df, top_n = 3) {
  tci <- attr(conn_df, "TCI")
  if (is.null(tci)) tci <- NA_real_
  
  top_tx <- paste(head(conn_df$series, top_n), collapse = ", ")
  top_rx <- paste(tail(conn_df$series, top_n), collapse = ", ")
  
  data.frame(
    TCI = tci,
    top_net_transmitters = top_tx,
    top_net_receivers = top_rx,
    stringsAsFactors = FALSE
  )
}

# ----- full reporting wrapper -----

# Generate and export all reporting tables for a market.
# Inputs:
# - vol_xts: volatility panel used for VAR/DY
# - ret_xts: returns panel (optional but recommended for appendix)
# - dy_res: output of compute_dy_connectedness()
# Outputs: CSV files in outputs/tables
generate_reporting_tables <- function(market, cfg, vol_xts, ret_xts = NULL, dy_res = NULL) {
  if (!is.character(market) || length(market) != 1) stop("market must be a single string.")
  
  out_paths <- list()
  
  # Volatility-based tables
  vol_sum <- summary_stats_xts(vol_xts)
  out_paths$vol_summary <- write_table_csv(vol_sum, file.path(cfg$paths$tables, paste0("summary_vol_", market, ".csv")))
  
  vol_norm <- normality_tests_xts(vol_xts)
  out_paths$vol_normality <- write_table_csv(vol_norm, file.path(cfg$paths$tables, paste0("normality_vol_", market, ".csv")))
  
  vol_stat <- stationarity_tests_xts(vol_xts, kpss_null = "Level")
  out_paths$vol_stationarity <- write_table_csv(vol_stat, file.path(cfg$paths$tables, paste0("stationarity_vol_", market, ".csv")))
  
  # Returns-based tables (if provided)
  if (!is.null(ret_xts)) {
    ret_sum <- summary_stats_xts(ret_xts)
    out_paths$ret_summary <- write_table_csv(ret_sum, file.path(cfg$paths$tables, paste0("summary_returns_", market, ".csv")))
    
    ret_norm <- normality_tests_xts(ret_xts)
    out_paths$ret_normality <- write_table_csv(ret_norm, file.path(cfg$paths$tables, paste0("normality_returns_", market, ".csv")))
    
    ret_stat <- stationarity_tests_xts(ret_xts, kpss_null = "Level")
    out_paths$ret_stationarity <- write_table_csv(ret_stat, file.path(cfg$paths$tables, paste0("stationarity_returns_", market, ".csv")))
  }
  
  # Connectedness tables (if provided)
  if (!is.null(dy_res)) {
    conn_df <- connectedness_table(dy_res)
    out_paths$connectedness <- write_table_csv(conn_df, file.path(cfg$paths$tables, paste0("connectedness_", market, ".csv")))
    
    conn_sum <- connectedness_summary(conn_df, top_n = 3)
    out_paths$connectedness_summary <- write_table_csv(conn_sum, file.path(cfg$paths$tables, paste0("connectedness_summary_", market, ".csv")))
  }
  
  invisible(out_paths)
}
