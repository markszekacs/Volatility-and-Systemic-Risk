suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

# Rolling Total Connectedness Index (TCI).
# This computes DY connectedness on each rolling window and stores the scalar TCI.
compute_rolling_tci <- function(vol_xts, window, p, var_type, horizon, gfevd_fun) {
  if (!xts::is.xts(vol_xts)) stop("vol_xts must be xts.")
  if (window <= 10) stop("window is too small.")
  if (nrow(vol_xts) < window) stop("Not enough rows for the rolling window.")
  
  n <- nrow(vol_xts)
  out_len <- n - window + 1
  out_dates <- zoo::index(vol_xts)[window:n]
  tci <- rep(NA_real_, out_len)
  
  for (i in seq_len(out_len)) {
    wdat <- vol_xts[i:(i + window - 1), ]
    res <- compute_dy_connectedness(
      vol_xts = wdat,
      p = p,
      var_type = var_type,
      horizon = horizon,
      gfevd_fun = gfevd_fun
    )
    tci[i] <- res$measures$TCI
  }
  
  xts::xts(tci, order.by = out_dates)
}

# Rolling NET connectedness (vector per asset per time).
# Returns an xts object with one column per ticker: NET(t).
compute_rolling_net <- function(vol_xts, window, p, var_type, horizon, gfevd_fun) {
  if (!xts::is.xts(vol_xts)) stop("vol_xts must be xts.")
  if (window <= 10) stop("window is too small.")
  if (nrow(vol_xts) < window) stop("Not enough rows for the rolling window.")
  
  n <- nrow(vol_xts)
  out_len <- n - window + 1
  out_dates <- zoo::index(vol_xts)[window:n]
  
  tickers <- colnames(vol_xts)
  net_mat <- matrix(NA_real_, nrow = out_len, ncol = length(tickers))
  colnames(net_mat) <- tickers
  
  for (i in seq_len(out_len)) {
    wdat <- vol_xts[i:(i + window - 1), ]
    res <- compute_dy_connectedness(
      vol_xts = wdat,
      p = p,
      var_type = var_type,
      horizon = horizon,
      gfevd_fun = gfevd_fun
    )
    net_mat[i, ] <- as.numeric(res$measures$NET)
  }
  
  xts::xts(net_mat, order.by = out_dates)
}

# Convenience wrapper: compute both TCI and NET in one pass.
compute_rolling_connectedness <- function(vol_xts, window, p, var_type, horizon, gfevd_fun, compute_net = TRUE) {
  tci <- compute_rolling_tci(vol_xts, window, p, var_type, horizon, gfevd_fun)
  if (!compute_net) {
    return(list(tci = tci))
  }
  net <- compute_rolling_net(vol_xts, window, p, var_type, horizon, gfevd_fun)
  list(tci = tci, net = net)
}