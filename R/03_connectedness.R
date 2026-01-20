suppressPackageStartupMessages({
  library(vars)
  library(xts)
})

# Fit a VAR model on an xts panel.
fit_var_model <- function(x, p = 1, type = "none") {
  if (!xts::is.xts(x)) stop("x must be xts.")
  vars::VAR(x, p = p, type = type)
}

# Convert Phi array to a list of coefficient matrices by horizon step.
phi_to_list <- function(phi_array) {
  # phi_array is (k x k x H)
  plyr::alply(phi_array, 3)
}

# Normalize FEVD rows to sum to 100.
normalize_fevd <- function(fevd_mat) {
  fevd_mat / rowSums(fevd_mat) * 100
}

# Compute connectedness measures from normalized FEVD.
connectedness_measures <- function(norm_fevd) {
  k <- ncol(norm_fevd)
  
  to_others   <- colSums(norm_fevd) - diag(norm_fevd)
  from_others <- rowSums(norm_fevd) - diag(norm_fevd)
  net         <- to_others - from_others
  
  total_offdiag <- sum(norm_fevd) - sum(diag(norm_fevd))
  tci <- total_offdiag / sum(norm_fevd) * 100
  
  net_matrix <- t(norm_fevd) - norm_fevd
  
  list(
    TCI = tci,
    TO = to_others,
    FROM = from_others,
    NET = net,
    NET_MATRIX = net_matrix
  )
}

# Main wrapper: compute DY connectedness for a volatility panel.
compute_dy_connectedness <- function(vol_xts, p = 1, var_type = "none", horizon = 10, gfevd_fun) {
  if (missing(gfevd_fun)) stop("Provide gfevd_fun (your existing GFEVD function).")
  
  var_model <- fit_var_model(vol_xts, p = p, type = var_type)
  phi_array <- vars::Phi(var_model, nstep = horizon)
  phi_list  <- phi_to_list(phi_array)
  
  fevd <- t(gfevd_fun(residuals(var_model), phi_list))
  norm <- normalize_fevd(fevd)
  
  measures <- connectedness_measures(norm)
  
  list(
    var_model = var_model,
    fevd = fevd,
    norm_fevd = norm,
    measures = measures
  )
}
