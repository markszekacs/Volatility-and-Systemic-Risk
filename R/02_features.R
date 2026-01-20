suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

# ----- basic helpers -----

# Compute log returns from a close-price panel (xts with one column per ticker).
compute_log_returns <- function(close_xts) {
  if (!xts::is.xts(close_xts)) stop("close_xts must be an xts object.")
  rets <- diff(log(close_xts))
  colnames(rets) <- colnames(close_xts)
  na.omit(rets)
}

# Apply a simple column-wise transformation to an xts panel.
transform_panel <- function(x, transform = c("none", "asinh")) {
  transform <- match.arg(transform)
  if (transform == "none") return(x)
  if (transform == "asinh") return(asinh(x))
  stop("Unknown transform: ", transform)
}

# ----- volatility from OHLC -----

# Compute your range-based volatility from a single OHLC xts object.
# Assumes standard quantmod OHLC column order: Open, High, Low, Close.
compute_range_vol_single <- function(ohlc_xts, annual = 252, scale = 0.361, transform = c("none", "asinh")) {
  transform <- match.arg(transform)
  
  if (!xts::is.xts(ohlc_xts)) stop("ohlc_xts must be an xts object.")
  if (NCOL(ohlc_xts) < 4) stop("ohlc_xts must contain at least 4 columns (O/H/L/C).")
  
  high <- ohlc_xts[, 2]
  low  <- ohlc_xts[, 3]
  
  # Your formula: sqrt(annual * scale * log(High/Low)^2)
  vol <- sqrt(annual * scale * (log(high / low) ^ 2))
  
  if (transform == "asinh") vol <- asinh(vol)
  vol
}

# Build a volatility panel (xts) from a getSymbols-like env/list of OHLC xts objects.
compute_range_vol_panel <- function(prices_env_or_list, annual = 252, scale = 0.361, transform = c("none", "asinh")) {
  transform <- match.arg(transform)
  
  # Accept either an environment (from getSymbols) or a named list of xts objects.
  lst <- if (is.environment(prices_env_or_list)) as.list(prices_env_or_list) else prices_env_or_list
  if (!is.list(lst) || length(lst) == 0) stop("prices_env_or_list must be a non-empty env or list.")
  
  vols <- lapply(lst, compute_range_vol_single, annual = annual, scale = scale, transform = transform)
  
  # Merge into one panel with consistent column names.
  vol_xts <- do.call(merge, vols)
  colnames(vol_xts) <- names(lst)
  
  na.omit(vol_xts)
}

# Convenience: compute both close prices and returns from an OHLC env/list.
extract_close_panel <- function(prices_env_or_list) {
  lst <- if (is.environment(prices_env_or_list)) as.list(prices_env_or_list) else prices_env_or_list
  closes <- lapply(lst, quantmod::Cl)
  close_xts <- do.call(merge, closes)
  colnames(close_xts) <- names(lst)
  na.omit(close_xts)
}

# Align two xts panels on the intersection of timestamps.
align_panels <- function(...) {
  panels <- list(...)
  panels <- panels[vapply(panels, xts::is.xts, logical(1))]
  if (length(panels) < 2) stop("Provide at least two xts panels.")
  
  # Build a single merged object using an inner join on timestamps.
  merged <- do.call(merge, c(panels, list(join = "inner")))
  merged <- na.omit(merged)
  
  if (nrow(merged) == 0) {
    stop("No overlapping timestamps after inner join. Check your date ranges and NA handling.")
  }
  
  # Split back into the original panels (same column blocks).
  out <- vector("list", length(panels))
  start <- 1
  for (i in seq_along(panels)) {
    k <- ncol(panels[[i]])
    out[[i]] <- merged[, start:(start + k - 1)]
    colnames(out[[i]]) <- colnames(panels[[i]])
    start <- start + k
  }
  
  out
}
