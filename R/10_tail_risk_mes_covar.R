suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantreg)
})

# ----- VaR / ES (historical) -----

# Historical VaR for a numeric vector (returns).
historical_var <- function(x, alpha = 0.05, na_rm = TRUE) {
  if (na_rm) x <- x[is.finite(x)]
  stats::quantile(x, probs = alpha, na.rm = na_rm, type = 7)
}

# Historical ES (Expected Shortfall) for a numeric vector (returns).
historical_es <- function(x, alpha = 0.05, na_rm = TRUE) {
  if (na_rm) x <- x[is.finite(x)]
  q <- as.numeric(historical_var(x, alpha = alpha, na_rm = na_rm))
  mean(x[x <= q], na.rm = na_rm)
}

# Compute VaR/ES for each column of an xts return panel.
compute_var_es_panel <- function(ret_xts, alpha = 0.05) {
  if (!xts::is.xts(ret_xts)) stop("ret_xts must be xts.")
  out <- lapply(colnames(ret_xts), function(nm) {
    x <- as.numeric(ret_xts[, nm])
    data.frame(
      asset = nm,
      VaR = as.numeric(historical_var(x, alpha = alpha)),
      ES  = as.numeric(historical_es(x, alpha = alpha)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

# ----- CoVaR / MES (simple nonparametric) -----

# Nonparametric CoVaR: CoVaR_{i|m} = Quantile of i given market <= VaR_m(alpha).
# This is a simple conditional empirical quantile.
compute_covar_np <- function(ret_xts, market_col, alpha = 0.05) {
  if (!xts::is.xts(ret_xts)) stop("ret_xts must be xts.")
  if (!(market_col %in% colnames(ret_xts))) stop("market_col not found in ret_xts.")
  
  m <- as.numeric(ret_xts[, market_col])
  var_m <- as.numeric(historical_var(m, alpha = alpha))
  tail_idx <- which(m <= var_m)
  
  if (length(tail_idx) < 30) warning("Very few tail observations for CoVaR (nonparametric).")
  
  assets <- setdiff(colnames(ret_xts), market_col)
  
  out <- lapply(assets, function(a) {
    x <- as.numeric(ret_xts[, a])
    covar <- stats::quantile(x[tail_idx], probs = alpha, na.rm = TRUE, type = 7)
    data.frame(asset = a, CoVaR = as.numeric(covar), stringsAsFactors = FALSE)
  })
  
  do.call(rbind, out)
}

# Nonparametric MES: MES_i = E[ r_i | market <= VaR_m(alpha) ].
compute_mes_np <- function(ret_xts, market_col, alpha = 0.05) {
  if (!xts::is.xts(ret_xts)) stop("ret_xts must be xts.")
  if (!(market_col %in% colnames(ret_xts))) stop("market_col not found in ret_xts.")
  
  m <- as.numeric(ret_xts[, market_col])
  var_m <- as.numeric(historical_var(m, alpha = alpha))
  tail_idx <- which(m <= var_m)
  
  if (length(tail_idx) < 30) warning("Very few tail observations for MES (nonparametric).")
  
  assets <- setdiff(colnames(ret_xts), market_col)
  
  out <- lapply(assets, function(a) {
    x <- as.numeric(ret_xts[, a])
    mes <- mean(x[tail_idx], na.rm = TRUE)
    data.frame(asset = a, MES = as.numeric(mes), stringsAsFactors = FALSE)
  })
  
  do.call(rbind, out)
}

# ----- CoVaR / MES (quantile regression) -----

# Quantile regression CoVaR:
# Fit rq(r_i ~ r_m) at quantile alpha, then plug in r_m = VaR_m(alpha).
compute_covar_qr <- function(ret_xts, market_col, alpha = 0.05) {
  if (!xts::is.xts(ret_xts)) stop("ret_xts must be xts.")
  if (!(market_col %in% colnames(ret_xts))) stop("market_col not found in ret_xts.")
  
  m <- as.numeric(ret_xts[, market_col])
  var_m <- as.numeric(historical_var(m, alpha = alpha))
  
  assets <- setdiff(colnames(ret_xts), market_col)
  
  out <- lapply(assets, function(a) {
    x <- as.numeric(ret_xts[, a])
    
    df <- data.frame(x = x, m = m)
    df <- df[is.finite(df$x) & is.finite(df$m), , drop = FALSE]
    
    fit <- quantreg::rq(x ~ m, tau = alpha, data = df)
    b <- stats::coef(fit)
    
    covar <- b[1] + b[2] * var_m
    data.frame(asset = a, CoVaR = as.numeric(covar), stringsAsFactors = FALSE)
  })
  
  do.call(rbind, out)
}

# Quantile regression MES (simple version):
# Compute MES as the conditional mean in the market tail as before (nonparametric),
# or optionally use a regression-based tail expectation model.
# Here we keep a stable nonparametric definition and pair it with QR-CoVaR.
compute_mes_qrproxy <- function(ret_xts, market_col, alpha = 0.05) {
  compute_mes_np(ret_xts, market_col, alpha)
}

# ----- Unified wrapper -----

compute_tail_risk <- function(ret_xts, market_col, alpha = 0.05, method = c("np", "qr")) {
  method <- match.arg(method)
  if (!xts::is.xts(ret_xts)) stop("ret_xts must be xts.")
  
  var_es <- compute_var_es_panel(ret_xts, alpha = alpha)
  
  if (method == "np") {
    covar <- compute_covar_np(ret_xts, market_col, alpha = alpha)
    mes   <- compute_mes_np(ret_xts, market_col, alpha = alpha)
  } else {
    covar <- compute_covar_qr(ret_xts, market_col, alpha = alpha)
    mes   <- compute_mes_qrproxy(ret_xts, market_col, alpha = alpha)
  }
  
  list(
    var_es = var_es,
    covar = covar,
    mes = mes
  )
}

export_tail_risk <- function(tail_res, base_path_no_ext) {
  if (!is.list(tail_res)) stop("tail_res must be a list.")
  saveRDS(tail_res, paste0(base_path_no_ext, "_full.rds"))
  
  if (!is.null(tail_res$var_es)) utils::write.csv(tail_res$var_es, paste0(base_path_no_ext, "_var_es.csv"), row.names = FALSE)
  if (!is.null(tail_res$covar))  utils::write.csv(tail_res$covar,  paste0(base_path_no_ext, "_covar.csv"),  row.names = FALSE)
  if (!is.null(tail_res$mes))    utils::write.csv(tail_res$mes,    paste0(base_path_no_ext, "_mes.csv"),    row.names = FALSE)
  
  invisible(TRUE)
}
